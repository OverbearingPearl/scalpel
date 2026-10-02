;;; scalpel-diagnose.el --- Classify planner errors by who caused them -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; One place that says who a failure belongs to.  A round can fail
;; because the model's reply broke the action contract, because the
;; user must do something (add a file to the context), or because
;; Scalpel itself misbehaved.  The console used to keep these tables
;; ad hoc across two files; this module owns the categories, the type
;; lists behind them, and the per-failure advice text.

;;; Code:

(defconst scalpel-diagnose-planner-types
  '(parse tool-call prose unknown-tool missing-field malformed no-replacement
          no-such-symbol pattern-no-match bad-path unbalanced no-validation)
  "Error types caused by the planner's reply, not by Scalpel or the user.
Under the default-allow self-heal policy every type in this list is
eligible for an automatic retry round except the two names held in the
denylist: PROSE and the context type FILE-OUTSIDE-CONTEXT.  The
denylist therefore contains only those two entries; no other planner
type here may be excluded from self-heal.
A round that fails with one of these did not run anything: the
model's output broke the action contract.  Retry advice differs
type by type, so see the variable `scalpel-diagnose-advice-category-table'.
NO-VALIDATION means the planner picked file-substitute for a structured
language whose provider cannot validate a rewrite; the remedy is
block-edit, which the retry round can plan directly.
UNKNOWN-TOOL means the planner named a tool the action contract
does not define, and MISSING-FIELD means an action omitted a
required field; each is corrected by re-emitting the document with
the tool name or field fixed.")

(defconst scalpel-diagnose-context-types
  '(file-outside-context)
  "Error types the user, not the planner, can fix.
These name something missing from the environment -- a file the
action needed that the context does not hold.  Retrying never
helps; the remedy is stated in the advice.")

(defun scalpel-diagnose-category (type)
  "Return the blame category of error TYPE: planner, context, or executor.
Planner means the model's reply broke the contract; context means
the user must change the environment; executor means Scalpel
itself misbehaved and is a bug."
  (cond
   ((memq type scalpel-diagnose-planner-types) 'planner)
   ((memq type scalpel-diagnose-context-types) 'context)
   (t 'executor)))

(defun scalpel-diagnose-planner-error-p (err)
  "Return non-nil when ERR is a plist naming a planner-output failure.
Such a round executed nothing and failed because the model's reply
did not follow the action contract."
  (eq (scalpel-diagnose-category (plist-get err :type)) 'planner))

(defconst scalpel-diagnose-self-heal-denylist
  '(prose file-outside-context)
  "Error types excluded from automatic self-healing retries.

By default self-healing is attempted for EVERY planner error
type: a type is denied only when retrying the identical prompt
provably repeats the failure and thus burns retry tokens for
nothing.  The console retry budget still caps the total number
of attempts for any error that IS retried.

The list contains:

- `prose': retrying a prose-generation failure cannot fix the
  underlying problem and only wastes tokens repeating the same
  output, which is exactly why the old allowlist excluded it.

- `file-outside-context': the only real context-category error;
  it means some file must be provided by the user, so retrying
  cannot add a file the user must add, making an automatic
  retry pointless.")

(defun scalpel-diagnose-self-heal-p (err)
  "Return non-nil when ERR is a plist whose :type is self-healable.
Self-healing is the default: any plist carrying a :type is
considered a planner-output failure that a retry with the error
text in the conversation can fix, unless that type is explicitly
listed in the denylist variable `scalpel-diagnose-self-heal-denylist'."
  (and (listp err)
       (plist-member err :type)
       (not (memq (plist-get err :type) scalpel-diagnose-self-heal-denylist))))

(defconst scalpel-diagnose-dialect-error-types
  '((scalpel-llm-dialect-tool-call-error . tool-call)
    (scalpel-llm-dialect-prose-reply-error . prose))
  "Single owner of the dialect-condition-to-planner-error-type mapping.
The dialect module defines the condition symbols; this module decides
which planner error type each one means.  `scalpel-agent-plan'
consults this table instead of keeping its own private copy, so
adding a new dialect condition means touching only this table.")

(defvar scalpel-diagnose-paren-providers '()
  "Register bracket defect diagnostic providers per language in an alist.
The value is a list of \(EXTENSION . PROVIDER-FUNCTION) pairs where
EXTENSION is a string like \"el\" without the leading dot, and
PROVIDER-FUNCTION takes a buffer or file name and returns a list of
bracket defect diagnostics \(plist with :type :line :col :message)
for a rejected reply.  Order matters: earlier entries win when
extensions collide.  Defined with defvar so that reloading this file
does not reset registrations made by language modules loaded before it.")

(defun scalpel-diagnose-register-paren-provider (extensions provider)
  "Register PROVIDER for file EXTENSIONS (list of strings).
The provider replaces any previous registration for the same
extensions so that reloading definitions stays idempotent."
  (dolist (ext extensions)
    (setq scalpel-diagnose-paren-providers
          (cons (cons ext provider)
                (cl-remove-if
                 (lambda (entry) (equal (car entry) ext))
                 scalpel-diagnose-paren-providers))))
  scalpel-diagnose-paren-providers)

(defun scalpel-diagnose-paren-defects (file text)
  "Ask the registered provider for bracket defects in TEXT.
TEXT is a candidate replacement text for FILE.  Dispatch on FILE's
extension without knowing the language; return nil when no provider is
registered for that extension.  Diagnostics returned by the provider are
passed through as-is."
  (let* ((name (if (bufferp file) (buffer-file-name file) file))
         (ext (and name
                   (string-match "\\.\\([^../\\]+\\)\\'" name)
                   (match-string 1 name)))
         (entry (and ext (assoc ext scalpel-diagnose-paren-providers))))
    (when entry
      (funcall (cdr entry) text))))

(provide 'scalpel-diagnose)

;;; scalpel-diagnose.el ends here
