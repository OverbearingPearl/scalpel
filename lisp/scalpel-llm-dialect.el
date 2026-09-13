;;; scalpel-llm-dialect.el --- Per-backend reply dialect dispatch for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Backends differ in how their replies surface, not in how they are
;; sent: gptel already owns the wire protocol.  What varies is the
;; reply dialect -- tool-call markers a model leaks as plain text,
;; escapes JSON forbids, prose around the payload.  This registry
;; dispatches on the active gptel backend name to a provider that
;; turns a raw reply into a parsed action list, mirroring
;; `scalpel-locate''s provider dispatch.  A backend with no
;; registered provider uses the default parser.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'gptel)

(defvar scalpel-llm-dialect-providers nil
  "Alist of (REGEXP . PROVIDER-PLIST) for registered reply dialects.
PROVIDER-PLIST keys:
:parse-reply -- function (RAW), returns a list of action plists or
signals `user-error'.  Registering the same REGEXP replaces the
previous provider.")

(defun scalpel-llm-dialect-register (regexp provider)
  "Register PROVIDER as the reply dialect for backends matching REGEXP.
PROVIDER is a plist with a :parse-reply entry.  Registering the
same REGEXP replaces the previous provider."
  (setq scalpel-llm-dialect-providers
        (cons (cons regexp provider)
              (cl-remove-if (lambda (entry)
                              (string= (car entry) regexp))
                            scalpel-llm-dialect-providers))))

(defun scalpel-llm-dialect--provider ()
  "Return the provider plist for the active gptel backend, or nil.
Dispatch keys off `gptel-backend-name'; a backend with no matching
registration falls back to the default parser."
  (let ((name (and (boundp 'gptel-backend) gptel-backend
                   (gptel-backend-name gptel-backend))))
    (when name
      (let ((entry (cl-find-if (lambda (entry)
                                 (string-match-p (car entry) name))
                               scalpel-llm-dialect-providers)))
        (and entry (cdr entry))))))

(defun scalpel-llm-dialect--visible-raw (raw)
  "Return RAW with newlines, control bytes and non-ASCII characters escaped.
`prin1' alone hides control bytes (`print-escape-control-characters'
defaults to nil) and prints non-ASCII literally (`print-escape-nonascii'
defaults to nil), so a reply that failed to parse is indistinguishable
by eye from one that did."
  (let ((print-escape-newlines t)
        (print-escape-control-characters t)
        (print-escape-nonascii t)
        (print-escape-multibyte t))
    (prin1-to-string raw)))

(defun scalpel-llm-dialect--json-payload (raw)
  "Return the JSON action payload embedded in RAW, or nil.
A planner reply is a container: the JSON array may be preceded by
prose, wrapped in a markdown code fence, or both.  Return the first
balanced JSON array or object in RAW, ignoring everything around it;
return nil when RAW holds no complete JSON value.  Brackets inside
JSON strings never count, so a command such as \"echo ']'\" does not
end the payload early."
  (let ((start (string-match "\\[\\|{" raw))
        (i 0)
        (depth 0)
        (in-string nil)
        (escaped nil)
        end)
    (when start
      (setq i start)
      (while (and (< i (length raw)) (null end))
        (let ((char (aref raw i)))
          (cond
           (escaped (setq escaped nil))
           (in-string
            (cond ((eq char ?\\) (setq escaped t))
                  ((eq char ?\") (setq in-string nil))))
           ((eq char ?\") (setq in-string t))
           ((memq char '(?\[ ?\{)) (setq depth (1+ depth)))
           ((memq char '(?\] ?\})) (setq depth (1- depth))
            (when (= depth 0) (setq end (1+ i))))))
        (setq i (1+ i)))
      (when end
        (substring raw start end)))))

(defun scalpel-llm-dialect--json-unterminated-p (raw)
  "Return non-nil when RAW opens a JSON value that never closes.
The scanner mirrors `scalpel-llm-dialect--json-payload': brackets
inside string literals never count, and a backslash escapes the
next character."
  (let* ((start (string-match "\\[\\|{" raw))
         (depth 0)
         (in-string nil)
         (escaped nil)
         (i start))
    (and start
         (progn
           (while (and i (< i (length raw)) (zerop depth))
             (let ((char (aref raw i)))
               (cond
                (escaped (setq escaped nil))
                (in-string
                 (cond ((eq char ?\\) (setq escaped t))
                       ((eq char ?\") (setq in-string nil))))
                ((eq char ?\") (setq in-string t))
                ((memq char '(?\[ ?\{)) (setq depth (1+ depth)))
                ((memq char '(?\] ?\})) (setq depth (1- depth)))))
             (setq i (1+ i)))
           (> depth 0)))))

(defconst scalpel-llm-dialect--tool-call-tags
  '("invoke" "tool_call" "tool_calls" "function_call" "function_calls"
    "arg_key" "arg_value")
  "Tag names a reply carries when it writes a tool call as plain text.
Backends disagree on the dialect: some leak the harness's own
markup (`<invoke>'), others the model's internal one (`<tool_call>'
with `<arg_key>' and `<arg_value>' children).  Both mean the same
thing -- the planner answered in a calling convention nothing
parses -- so both are named, and detection and the test that pins
it read this list instead of each carrying its own copy.")

(defconst scalpel-llm-dialect--tool-call-regexp
  (concat "<" (regexp-opt scalpel-llm-dialect--tool-call-tags)
          "[ \t\n/>]")
  "Regexp matching a leaked tool-call tag in a planner reply.
Rendered as a non-capturing group, so callers may embed it.  A
delimiter must follow the name, so `<tool_call>' matches while
`tool_call' does not match the prefix of `<tool_calls>': each
spelling needs its own entry.")

(define-error 'scalpel-llm-dialect-tool-call-error
  "Scalpel: planner used tool-call syntax instead of the JSON action array"
  'user-error)

(defun scalpel-llm-dialect-error-message (err)
  "Return the message carried by the dialect condition ERR.
ERR is a `scalpel-llm-dialect-tool-call-error' condition value, as
`condition-case' binds it; its whole message is that condition's
first data element.  That element is read directly rather than
through `error-message-string', which renders a condition defined
by `define-error' as \"MESSAGE: DATA\" with DATA printed by `%S':
the sentence would come back doubled, and a reply that
`scalpel-llm-dialect--visible-raw' escaped on purpose would be
re-escaped into a form the user cannot read."
  (cadr err))

(defun scalpel-llm-dialect--parse-error (raw)
  "Signal the `user-error' describing why RAW failed to parse.
Distinguishes a planner reply that used tool-call syntax, one that
was cut off before its JSON array closed, and one that was simply
not valid JSON.  RAW is the reply as received.

Tool-call syntax signals `scalpel-llm-dialect-tool-call-error', a
`user-error' subtype, so a caller can tell it from a reply that
merely failed to parse: that one may come back whole on a retry,
while a reply written in another calling convention is the model's
own habit and was observed to repeat three times in a row on one
backend.  The other two branches signal plain `user-error'."
  (cond
   ((string-match-p scalpel-llm-dialect--tool-call-regexp raw)
    (signal 'scalpel-llm-dialect-tool-call-error
            (list
             (format (concat "Scalpel: planner used tool-call syntax "
                             "instead of the JSON action array; nothing was "
                             "executed.  Reply was: %s")
                     (scalpel-llm-dialect--visible-raw raw)))))
   ((scalpel-llm-dialect--json-unterminated-p raw)
    (user-error
     (concat "Scalpel: planner reply was cut off before its JSON array "
             "closed (likely the backend's output limit); nothing was "
             "executed.  Reply was: %s")
     (scalpel-llm-dialect--visible-raw raw)))
   (t
    (user-error "Scalpel: planner returned invalid JSON: %s"
                (scalpel-llm-dialect--visible-raw raw)))))

(defun scalpel-llm-dialect--escape-raw-controls (payload)
  "Return PAYLOAD with raw control characters inside JSON strings escaped.
Models sometimes emit literal newlines or tabs inside JSON string
values, which JSON forbids, so the whole plan fails to parse.  Only
characters inside a string literal are touched: outside one, a
newline is legal whitespace.  The scanner mirrors
`scalpel-llm-dialect--json-payload': a backslash escapes the next
character, and a quote toggles the string."
  (let ((in-string nil)
        (escaped nil))
    (mapconcat
     (lambda (char)
       (cond
        ;; A backslash followed by a character JSON does not define as
        ;; an escape (models emit things like "\ docstring") is doubled,
        ;; so the pair parses as a literal backslash instead of failing
        ;; the whole payload.  Valid escapes pass through untouched.
        (escaped
         (setq escaped nil)
         (if (memq char '(?\" ?\\ ?/ ?b ?f ?n ?r ?t ?u))
             (string char)
           (concat "\\\\" (string char))))
        ((eq char ?\\) (setq escaped t) (string char))
        ((eq char ?\") (setq in-string (not in-string)) (string char))
        ((and in-string (memq char '(?\n ?\r ?\t)))
         (format "\\u%04X" char))
        (t (string char))))
     payload "")))

(defun scalpel-llm-dialect--default-parse (raw)
  "Parse RAW to a list of action plists with the default dialect.
RAW is the planner's whole reply, so the JSON payload is extracted
from whatever prose or markdown fences surround it.  Signal
`user-error' when RAW holds no valid JSON action array."
  (when (string-empty-p (string-trim raw))
    ;; An empty reply is a backend failure, not a JSON syntax problem:
    ;; naming JSON here would send the user hunting for a syntax error
    ;; that does not exist.
    (user-error
     "Scalpel: planner returned an empty reply; check the backend's \
API key, quota and network, then retry"))
  (let ((payload (scalpel-llm-dialect--json-payload raw)))
    (unless payload
      ;; `scalpel-llm-dialect--parse-error' signals, so a missing payload
      ;; and an unparsable one share a single explanation path.
      (scalpel-llm-dialect--parse-error raw))
    (let ((parsed
           (condition-case err
               (json-parse-string
                ;; Models emit \x2014-style escapes, which JSON forbids;
                ;; normalize them to \uXXXX before parsing, then escape
                ;; raw control characters inside string literals.
                (scalpel-llm-dialect--escape-raw-controls
                 (replace-regexp-in-string
                  "\\\\x\\([0-9a-fA-F]\\{4\\}\\)" "\\\\u\\1" payload))
                :object-type 'plist
                :array-type 'list)
             ;; The parser's own message names the offending construct;
             ;; dropping it, as a bare `condition-case nil' would, makes
             ;; every parse failure indistinguishable.
             (error
              (user-error
               "Scalpel: planner returned invalid JSON (%s): %s"
               (error-message-string err)
               (scalpel-llm-dialect--visible-raw raw))))))
      (when (and (plistp parsed) (plist-get parsed :tool))
        (setq parsed (list parsed)))
      (unless (and (listp parsed)
                   (cl-every (lambda (item)
                               (and (listp item)
                                    (plist-get item :tool)))
                             parsed))
        (user-error
         (concat "Scalpel: planner returned unexpected structure "
                 "(expected a JSON array of action objects): %s")
         (scalpel-llm-dialect--visible-raw raw)))
      parsed)))

(defun scalpel-llm-dialect-parse (raw)
  "Parse the raw planner reply RAW through the active backend's dialect.
Return a list of action plists.  A provider registered for the
active gptel backend handles the reply; with no provider, the
default parser applies."
  (let ((provider (scalpel-llm-dialect--provider)))
    (if provider
        (funcall (plist-get provider :parse-reply) raw)
      (scalpel-llm-dialect--default-parse raw))))

(provide 'scalpel-llm-dialect)

;;; scalpel-llm-dialect.el ends here
