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
;; prose around the payload.  This registry
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
;; The plan contract is TOML: the default dialect parser writes the raw
;; reply to a temp file and reads it back with `toml:read-from-file', so
;; this package is a hard dependency like gptel and must be required
;; here, not relied on to be autoloaded.
(require 'toml)
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

(defun scalpel-llm-dialect--comment-prose (raw)
  "Comment out the prose note ahead of the first TOML table.
Planners sometimes prepend a status sentence before the
document.  Every line before the first line starting with `['
is prefixed with `# ' so `toml:read-from-file' still parses
the cleaned text.  RAW without any table header, or with the
table header already first, is returned unchanged."
  (let ((idx (string-match "^\x5c[" raw)))
    (if (or (null idx) (= idx 0))
        raw
      (concat
       (replace-regexp-in-string "^" "# " (substring raw 0 idx))
       (substring raw idx)))))

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
  "Scalpel: planner used tool-call syntax instead of the TOML action document"
  'user-error)

(define-error 'scalpel-llm-dialect-prose-reply-error
  "Scalpel: planner replied in prose instead of the TOML action document"
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

(defun scalpel-llm-dialect--count-fences (text)
  "Count non-overlapping occurrences of the ''' fence in TEXT."
  (let ((count 0) (pos 0))
    (while (string-match "'''" text pos)
      (setq count (1+ count)
            pos (match-end 0)))
    count))

(defun scalpel-llm-dialect--parse-error (raw)
  "Signal the `user-error' describing why RAW failed to parse.
Distinguishes a planner reply that used tool-call syntax, one that
was cut off before its TOML document closed, one that held no TOML
value at all -- prose, where there is no document to be invalid --
and one that was simply not valid TOML.  RAW is the reply as
received.  A prose reply is shown as it was written, because it is
the answer rather than a failed parse; the other branches show it
escaped.

Classification is fence-based: the reply's TOML document lives
inside triple-quote fences, so an even number of ''' fences means
the document is complete and either parses or is invalid TOML,
while an odd count means it was cut off before it closed; a reply
that opens an [[action]] table but never closes a fence was cut off
too, even if no fence ever opened.  A reply with no fence and no
`key = ' assignment anywhere is prose.

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
  (let ((fences (scalpel-llm-dialect--count-fences raw)))
    (cond
     ((string-match-p scalpel-llm-dialect--tool-call-regexp raw)
      (signal 'scalpel-llm-dialect-tool-call-error
              (list
               (format (concat "Scalpel: planner used tool-call syntax "
                               "instead of the TOML action document; nothing "
                               "was executed.  Reply was: %s")
                       (scalpel-llm-dialect--visible-raw raw)))))
     ((or (cl-oddp fences)
          (and (zerop fences)
               (string-match-p "^\\[\\[action\\]\\]" raw)))
      (user-error
       (concat "Scalpel: planner reply was cut off before its TOML document "
               "closed (likely the backend's output limit); nothing was "
               "executed.  Reply was: %s")
       (scalpel-llm-dialect--visible-raw raw)))
     ;; No fence opened anywhere in the reply and no `key = ' assignment
     ;; either, so there is no document that could be invalid: the
     ;; planner answered in prose.  Naming a syntax error here would
     ;; send the user hunting for one that is not there, the mistake
     ;; the empty-reply branch of `--default-parse' already exists to
     ;; avoid.  This branch is reached only on the console's path, where
     ;; a reply with structure is either parsed or reported as truncated
     ;; or invalid above.
     ((and (zerop fences)
           (not (string-match-p
                 "\\(\\`\\|[\r\n]\\)[ \t]*[A-Za-z_][A-Za-z0-9_.-]*[ \t]*="
                 raw)))
      (signal 'scalpel-llm-dialect-prose-reply-error
              (list
               (format (concat "Scalpel: planner replied in prose and sent no "
                               "TOML action document; nothing was executed.  "
                               "The reply is shown below as it was written, so "
                               "the answer it holds can still be read.  "
                               "Reply was:\n%s")
                       (scalpel-llm-dialect--readable-raw raw))
               ;; The prose rides along as a second data element, so a
               ;; caller that degrades prose into a reply action delivers
               ;; the answer itself, not the error narrative above.
               (scalpel-llm-dialect--readable-raw raw))))
     (t
      (user-error "Scalpel: planner returned invalid TOML: %s"
                  (scalpel-llm-dialect--visible-raw raw))))))

(defun scalpel-llm-dialect--default-parse (raw)
  "Parse RAW to a list of action plists with the default dialect.
RAW is the planner's whole TOML reply.  It is written verbatim to
a temp file and parsed with `toml:read-from-file' in one round
trip, and mirrored verbatim into a raw-TOML echo buffer named
after the thinking buffer so the reader sees exactly what the
planner wrote.  The contract is TOML: the parsed document is an
alist of pairs (STRING-KEY . VALUE).  The `'''` fences must
balance; a double quote inside '...' or '''...''' is content, not
a delimiter, and the TOML parser itself rejects a malformed
double-quoted string.  Either an [[action]] table array or a
single top-level table carrying a `tool' key is accepted; each
table's fields become the action plist keys.  TOML booleans
arrive as t and, for false, the symbol :false; in particular
`long-running' is checked for presence on the assoc entry itself,
since a TOML false parses to nil and would otherwise be
indistinguishable from an absent key.  Every failure is routed
through `scalpel-llm-dialect--parse-error'."
  (when (string-empty-p (string-trim raw))
    ;; An empty reply is a backend failure, not a TOML syntax problem:
    ;; naming TOML here would send the user hunting for a syntax error
    ;; that does not exist.
    (user-error
     "Scalpel: planner returned an empty reply; check the backend's \
API key, quota and network, then retry"))
  (let* ((echo-buffer (get-buffer-create "*Scalpel Raw TOML*"))
         (temp-file nil)
         (result nil)
         ;; Read KEY from an alist TABLE as produced by
         ;; `toml:read-from-file'; return nil when absent.
         (table-value (lambda (table key) (cdr (assoc key table))))
         ;; Comment the prose note ahead of the first table header so
         ;; the parser accepts the reply and the reader sees the same
         ;; commented text.
         (raw-text (scalpel-llm-dialect--comment-prose raw)))
    ;; Mirror RAW verbatim so failures can be read against the reply
    ;; the planner actually wrote, unmodified.
    (with-current-buffer echo-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert raw-text)))
    (unwind-protect
        (progn
          ;; Single-quote literal contract: only check that ''' fences
          ;; balance.  A double quote inside '...' or '''...''' is
          ;; content, not a delimiter, so scanning the raw text for
          ;; '"' cannot tell delimiter from content.  The TOML parser
          ;; itself rejects a malformed double-quoted string, so
          ;; nothing more is enforced here.
          (let ((pieces (split-string raw "'''")))
            (unless (cl-evenp (1- (length pieces)))
              (scalpel-llm-dialect--parse-error raw)))
          ;; Write RAW verbatim and parse in a single round trip.
          (setq temp-file (make-temp-file "scalpel-toml"))
          (with-temp-file temp-file
            (insert raw-text))
          (setq result (condition-case nil (toml:read-from-file temp-file) (error nil)))
          (unless (and (listp result)
                       (cl-every #'consp result))
            (scalpel-llm-dialect--parse-error raw))
          ;; Either an [[action]] table array or a single top-level
          ;; table with a `tool' key is a valid contract shape.
          (let ((raw-actions (funcall table-value result "action")))
            (unless (or (funcall table-value result "tool") raw-actions)
              (scalpel-llm-dialect--parse-error raw))
            (when (vectorp raw-actions)
              (setq raw-actions (append raw-actions nil)))
            (when (and (null raw-actions)
                       (funcall table-value result "tool"))
              (setq raw-actions (list result)))
            (unless (and (listp raw-actions)
                         (cl-every
                          (lambda (item)
                            (and (listp item)
                                 (cl-every #'consp item)
                                 (funcall table-value item "tool")))
                          raw-actions))
              (scalpel-llm-dialect--parse-error raw))
            (mapcar
             (lambda (item)
               (let ((pl nil)
                     (pairs '(("tool" . :tool)
                              ("file" . :file)
                              ("symbol" . :symbol)
                              ("instruction" . :instruction)
                              ("after" . :after)
                              ("text" . :text)
                              ("to" . :to)
                              ("files" . :files)
                              ("pattern" . :pattern)
                              ("replacement" . :replacement)
                              ("reason" . :reason)
                              ("command" . :command))))
                 (dolist (pair pairs)
                   (let ((value (funcall table-value item (car pair))))
                     (when value
                       (setq pl (plist-put pl (cdr pair) value)))))
                 ;; toml.el returns a TOML array as a vector, while
                 ;; file-substitute expects a list of paths.
                 (when (vectorp (plist-get pl :files))
                   (setq pl (plist-put
                             pl :files
                             (append (plist-get pl :files) nil))))
                 ;; `long-running' is looked up on the entry itself:
                 ;; a TOML false parses to nil, indistinguishable
                 ;; from absence through the helper.  When present,
                 ;; always put :long-running -- using :false for a
                 ;; TOML false -- so it keeps a non-nil representation.
                 (let ((entry (assoc "long-running" item)))
                   (when entry
                     (setq pl (plist-put
                               pl :long-running
                               (or (cdr entry) :false)))))
                 pl))
             raw-actions)))
      (when (and temp-file (file-exists-p temp-file))
        (delete-file temp-file)))))

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
