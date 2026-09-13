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

(defun scalpel-llm-dialect--parse-error (raw)
  "Signal the `user-error' describing why RAW failed to parse.
Distinguishes a planner reply that used tool-call syntax from one
that was simply not valid JSON.  RAW is the reply as received."
  (if (string-match-p "<\\(?:invoke\\|tool_calls\\|function_calls\\)\\b" raw)
      (user-error
       (concat "Scalpel: planner used tool-call syntax instead of the JSON "
               "action array; nothing was executed.  Reply was: %s")
       (scalpel-llm-dialect--visible-raw raw))
    (user-error "Scalpel: planner returned invalid JSON: %s"
                (scalpel-llm-dialect--visible-raw raw))))

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
