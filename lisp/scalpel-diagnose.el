;;; scalpel-diagnose.el --- Classify planner errors by who caused them -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
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
  '(parse tool-call prose malformed unknown-tool no-replacement
          no-such-symbol pattern-no-match bad-path unbalanced)
  "Error types caused by the planner's reply, not by Scalpel or the user.
A round that fails with one of these did not run anything: the
model's output broke the action contract.  Retry advice differs
type by type, so see the variable `scalpel-diagnose-advice'.")

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

(defconst scalpel-diagnose-self-heal-types
  '(parse malformed no-replacement no-such-symbol pattern-no-match
          bad-path unbalanced)
  "Planner error types the console may retry automatically.
Their failure reports carry enough context (near-miss lines, closest
symbols) for the model to correct its own reply next round.")

(defun scalpel-diagnose-self-heal-p (err)
  "Return non-nil when ERR is a plist whose :type is self-healable.
Such an error names a planner-output failure that a retry with
the error text in the conversation can fix; see the variable
`scalpel-diagnose-self-heal-types'."
  (and (listp err)
       (plist-member err :type)
       (memq (plist-get err :type) scalpel-diagnose-self-heal-types)))

(defconst scalpel-diagnose-advice
  '((tool-call
     . "the model answered in a tool-calling convention Scalpel does not
parse, so nothing was executed.  A backend that answers this way
tends to answer this way again, so retrying the same request
rarely helps: switch the backend with C-c C-b, or rephrase.  To
try again anyway: C-c C-e")
    (prose
     . "the model followed no parseable convention at all, so nothing was
executed.  Retry tends to repeat it: switch the backend with
C-c C-b, or rephrase the instruction.  To try again anyway: C-c C-e")
    (parse
     . "the model's reply was not readable; nothing was executed.  A
second try may come back whole.  Press C-c C-e or M-x
scalpel-console-repeat to retry"))
  "Advice text by exact error type.
Looked up by function `scalpel-diagnose-advice-for', which falls
back to variable `scalpel-diagnose-category-advice' via the
type's category when no exact entry matches.")

(defconst scalpel-diagnose-category-advice
  '((planner
     . "the model's reply was not usable; nothing was executed.  Press
C-c C-e or M-x scalpel-console-repeat to retry")
    (context
     . "nothing was executed.  Fix the environment first -- for a file
outside the context, add it with C-c C-a -- then ask again")
    (executor
     . "Scalpel itself failed; this looks like a bug.  Please report it
with the error text above"))
  "Advice text by error category, as a fallback.
`scalpel-diagnose-advice-for' consults this only when the
variable `scalpel-diagnose-advice' has no entry for the exact
type, so a new planner type needs no advice of its own.  A type
whose category is absent here resolves to the executor advice.")

(defun scalpel-diagnose-advice-for (type)
  "Return the advice string for error TYPE, or nil if unknown.
An exact entry in the variable `scalpel-diagnose-advice' wins;
otherwise the entry named after the type's category in the
variable `scalpel-diagnose-category-advice' is used.  Unknown
types fall to the executor entry, which that table always holds."
  (or (cdr (assq type scalpel-diagnose-advice))
      (cdr (assq (scalpel-diagnose-category type) scalpel-diagnose-category-advice))))

(defun scalpel-diagnose-advice (error-plist)
  "Extract :type from ERROR-PLIST and delegate to `scalpel-diagnose-advice-for'."
  (scalpel-diagnose-advice-for (plist-get error-plist :type)))

(provide 'scalpel-diagnose)

;;; scalpel-diagnose.el ends here
