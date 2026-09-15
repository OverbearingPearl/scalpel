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
;; dispatches on the active gptel backend's name, then on its model
;; name, to a provider that turns a raw reply into a parsed action
;; list, mirroring `scalpel-locate''s provider dispatch.  Both names
;; are tried because a backend is named after the provider while a
;; dialect belongs to the model: a Laguna model served by a backend
;; named "OpenRouter" is reached through its model name alone.  The
;; model name counts only when the backend declares that model:
;; `gptel-model' keeps its value after another backend is created or
;; selected, so an undeclared name would let one model's dialect
;; answer for a backend that does not serve it.  See
;; `scalpel-llm-dialect--model-name'.  A backend with no registered
;; provider uses the default parser.

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

(defun scalpel-llm-dialect--backend-name ()
  "Return the name of the active gptel backend, or nil.
A backend that is not a gptel backend structure -- a stub leaked
into the session by a test, say -- has no name rather than raising
from inside dispatch."
  (let ((backend (and (boundp 'gptel-backend)
                      (symbol-value 'gptel-backend))))
    (when backend
      (condition-case nil
          (gptel-backend-name backend)
        (error nil)))))

(defun scalpel-llm-dialect--name-string (value)
  "Return VALUE as a name string, or nil when it names nothing.
gptel takes a backend or model name as either a string or a symbol,
so both spellings are normalized the same way here."
  (cond ((null value) nil)
        ((stringp value) value)
        ((symbolp value) (symbol-name value))
        (t nil)))

(defun scalpel-llm-dialect--served-model-names ()
  "Return the model names the active gptel backend declares, or nil.
The declaration is what distinguishes a model this backend serves
from a `gptel-model' left over from another one.  A backend that is
not a gptel backend structure, or a gptel whose model accessor is
absent, declares nothing rather than raising from inside dispatch."
  (let ((backend (and (boundp 'gptel-backend)
                      (symbol-value 'gptel-backend))))
    (when backend
      (condition-case nil
          (delq nil (mapcar #'scalpel-llm-dialect--name-string
                            (gptel-backend-models backend)))
        (error nil)))))

(defun scalpel-llm-dialect--model-name ()
  "Return the model the active gptel backend serves, or nil.
`gptel-backend' names the provider, and the provider is not always
the dialect: a Laguna model reached through an OpenRouter backend
is named only by its model.  The value is read through
`symbol-value' on a quoted symbol, so a gptel without `gptel-model'
yields nil instead of a void-variable error or a byte-compile
warning, and a model held as a symbol is named by that symbol.

Only a model the backend declares is returned.  `gptel-model' is
not reset when another backend is created or selected, so a stale
name would otherwise let a dialect registered for one model answer
for a backend that does not serve it -- and a session that had once
selected that model would keep reading every later reply in its
dialect."
  (let* ((model (and (boundp 'gptel-model)
                     (symbol-value 'gptel-model)))
         (name (scalpel-llm-dialect--name-string model)))
    (when (member name (scalpel-llm-dialect--served-model-names))
      name)))

(defun scalpel-llm-dialect--provider ()
  "Return the provider plist for the active gptel backend, or nil.
Dispatch tries the backend's own name first and its model name
second, because a backend is named after the provider while a
dialect belongs to the model: `poolside/laguna-s-2.1' reached
through a backend named \"OpenRouter\" is found by its model name
alone.  That model name is the one the backend declares, as
`scalpel-llm-dialect--model-name' returns it.  A backend with no
matching registration falls back to the default parser."
  (cl-loop for name in (delq nil (list (scalpel-llm-dialect--backend-name)
                                       (scalpel-llm-dialect--model-name)))
           for entry = (cl-find-if (lambda (entry)
                                     (string-match-p (car entry) name))
                                   scalpel-llm-dialect-providers)
           when entry return (cdr entry)))

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

(defun scalpel-llm-dialect--json-container-start-p (raw pos)
  "Return non-nil when RAW at POS plausibly start a JSON container.
An array may begin with any JSON value or close immediately; an
object may begin with a quoted key or close immediately.  End of
input is plausible too, because it represents a truncated container."
  (let ((opener (aref raw pos))
        (next-pos (1+ pos)))
    (while (and (< next-pos (length raw))
                (memq (aref raw next-pos) '(?\s ?\t ?\n ?\r)))
      (setq next-pos (1+ next-pos)))
    (let ((next (and (< next-pos (length raw))
                     (aref raw next-pos))))
      (or (null next)
          (pcase opener
            (?\[
             (or (memq next '(?\" ?\[ ?\{ ?\] ?- ?t ?f ?n))
                 (and (<= ?0 next) (<= next ?9))))
            (?\{
             (memq next '(?\" ?\})))
            (_ nil))))))

(defun scalpel-llm-dialect--json-start (raw &optional from)
  "Return the next plausible JSON container start in RAW after FROM."
  (let ((pos (or from 0))
        found)
    (while (and (not found)
                (setq pos (string-match "\\[\\|{" raw pos)))
      (if (scalpel-llm-dialect--json-container-start-p raw pos)
          (setq found pos)
        (setq pos (1+ pos))))
    found))

(defun scalpel-llm-dialect--json-payloads (raw)
  "Return every balanced JSON array or object span in RAW, in order.
A planner reply is a container: the JSON array may be preceded by
prose, wrapped in a markdown code fence, or both -- and the prose or
the code it quotes may carry brackets of its own (a sed character
class, say).  A single first-span extraction would then hand back a
code fragment as the payload and report the whole reply as invalid
JSON, so every candidate span comes back and the caller picks the
one that really parses.  Brackets inside JSON strings never count,
so a command such as \"echo ']'\" does not end a span early."
  (let ((spans nil)
        (i 0))
    (while (< i (length raw))
      (let ((char (aref raw i)))
        (if (and (memq char '(?\[ ?\{))
                 (scalpel-llm-dialect--json-container-start-p raw i))
            (let ((depth 0)
                  (in-string nil)
                  (escaped nil)
                  end
                  (j i))
              (while (and (< j (length raw)) (null end))
                (let ((c (aref raw j)))
                  (cond
                   (escaped (setq escaped nil))
                   (in-string
                    (cond ((eq c ?\\) (setq escaped t))
                          ((eq c ?\") (setq in-string nil))))
                   ((eq c ?\") (setq in-string t))
                   ((memq c '(?\[ ?\{)) (setq depth (1+ depth)))
                   ((memq c '(?\] ?\})) (setq depth (1- depth))
                    (when (= depth 0) (setq end (1+ j)))))
                  (setq j (1+ j))))
              (if end
                  (progn
                    (push (substring raw i end) spans)
                    (setq i end))
                ;; An opener with no closer: nothing later can open a
                ;; complete span either, so stop scanning.
                (setq i (length raw))))
          (setq i (1+ i)))))
    (nreverse spans)))

(defun scalpel-llm-dialect--json-unterminated-p (raw)
  "Return non-nil when RAW opens a JSON value that never closes.
The scanner mirrors `scalpel-llm-dialect--json-payloads': brackets
inside string literals never count, and a backslash escapes the
next character.  Unlike the previous version, it scans the whole
string from the first opener, so a balanced bracket in prose --
such as \"grok-3-[beta]\" -- is not mistaken for an unterminated
JSON value."
  (let* ((start (scalpel-llm-dialect--json-start raw))
         (depth 0)
         (in-string nil)
         (escaped nil)
         (i start))
    (and start
         (progn
           (while (and i (< i (length raw)))
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

(define-error 'scalpel-llm-dialect-prose-reply-error
  "Scalpel: planner replied in prose instead of the JSON action array"
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

(defun scalpel-llm-dialect--readable-raw (raw)
  "Return RAW as it was written, for a reply that is prose.
The reply is the answer here, not a failed parse: there is no syntax
to inspect, and escaping it -- as `scalpel-llm-dialect--visible-raw'
does for the branches that do have one -- turns a multi-paragraph
answer into a single line of \\n and \\uXXXX escapes, which is what
made a prose reply unreadable in the console.  Newlines and tabs are
kept, because they are the answer's own shape.  Every other control
character is dropped: the console would ring its bell for one, and a
C1 byte would hide the text after it.  The filter mirrors
`scalpel-agent--printable-output', which cannot be reused here --
`scalpel-agent' requires this module, so the dependency runs the
other way."
  (mapconcat #'char-to-string
             (cl-remove-if-not
              (lambda (char)
                (or (memq char '(?\n ?\t))
                    (and (<= 32 char)
                         (not (<= 127 char 159)))))
              (string-to-list raw))
             ""))

(defun scalpel-llm-dialect--parse-error (raw)
  "Signal the `user-error' describing why RAW failed to parse.
Distinguishes a planner reply that used tool-call syntax, one that
was cut off before its JSON array closed, one that held no JSON
value at all -- prose, where there is no array to be invalid -- and
one that was simply not valid JSON.  RAW is the reply as received.
A prose reply is shown as it was written, because it is the answer
rather than a failed parse; the other branches show it escaped.

Every message states the failure and shows the reply, and nothing
more: what the user can do about it belongs to the console, which
prints the advice its error type warrants.  A remedy written here
too would be read twice in the console -- the two lines are
adjacent -- and the message joins the conversation, so it would
also be re-sent with every later request.

Tool-call syntax signals `scalpel-llm-dialect-tool-call-error' and
a prose reply `scalpel-llm-dialect-prose-reply-error', both
`user-error' subtypes, so a caller can tell either from a reply
that merely failed to parse: that one may come back whole on a
retry, while a reply written in another calling convention -- or in
none -- is the model's own habit and was observed to repeat on one
backend.  `scalpel-agent-plan' maps each condition to its own
planner error type for that reason.  The other branches signal
plain `user-error'."
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
   ;; No JSON value opened anywhere in the reply, so there is no array
   ;; that could be invalid: the planner answered in prose.  Naming a
   ;; syntax error here would send the user hunting for one that is not
   ;; there, the mistake the empty-reply branch of `--default-parse'
   ;; already exists to avoid.  This branch is reached only on the
   ;; console's path, where a reply with an opener is either parsed or
   ;; reported as truncated above.
   ((null (scalpel-llm-dialect--json-start raw))
    (signal 'scalpel-llm-dialect-prose-reply-error
            (list
             (format (concat "Scalpel: planner replied in prose and sent no "
                             "JSON action array; nothing was executed.  The "
                             "reply is shown below as it was written, so the "
                             "answer it holds can still be read.  "
                             "Reply was:\n%s")
                     (scalpel-llm-dialect--readable-raw raw))
             ;; The prose rides along as a second data element, so a
             ;; caller that degrades prose into a reply action delivers
             ;; the answer itself, not the error narrative above.
             (scalpel-llm-dialect--readable-raw raw))))
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
  (let ((candidates (scalpel-llm-dialect--json-payloads raw))
        (winner nil)
        (parsed nil)
        ;; Models emit \x2014-style escapes, which JSON forbids;
        ;; normalize them to \uXXXX before parsing, then escape raw
        ;; control characters inside string literals.
        (normalize
         (lambda (payload)
           (scalpel-llm-dialect--escape-raw-controls
            (replace-regexp-in-string
             "\\\\x\\([0-9a-fA-F]\\{4\\}\\)" "\\\\u\\1" payload)))))
    (unless candidates
      ;; `scalpel-llm-dialect--parse-error' signals, so a missing payload
      ;; and an unparsable one share a single explanation path.
      (scalpel-llm-dialect--parse-error raw))
    ;; The first bracket pair in a reply may belong to code the reply
    ;; quotes rather than to the action array, so every candidate is
    ;; tried and the first one that parses into action objects wins.
    (dolist (payload candidates)
      (unless winner
        (let ((attempt
               (condition-case nil
                   (let ((value
                          (json-parse-string (funcall normalize payload)
                                             :object-type 'plist
                                             :array-type 'list)))
                     (when (and (plistp value) (plist-get value :tool))
                       (setq value (list value)))
                     (when (and (listp value)
                                (cl-every (lambda (item)
                                            (and (listp item)
                                                 (plist-get item :tool)))
                                          value))
                       value))
                 (error nil))))
          (when attempt
            (setq winner payload parsed attempt)))))
    (unless parsed
      ;; Nothing parsed: the failure is reported against the whole raw
      ;; reply, not a bracket fragment of it, so the reader sees what
      ;; the planner actually wrote.
      (scalpel-llm-dialect--parse-error raw))
    parsed))

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
