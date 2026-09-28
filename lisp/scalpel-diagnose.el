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
  '(parse tool-call prose unknown-tool missing-field malformed no-replacement
          no-such-symbol pattern-no-match bad-path unbalanced no-validation)
  "Error types caused by the planner's reply, not by Scalpel or the user.
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

(defconst scalpel-diagnose-self-heal-types
  '(parse malformed no-replacement no-such-symbol pattern-no-match
          bad-path unbalanced no-validation)
  "Planner error types the console may retry automatically.
Their failure reports carry enough context (near-miss lines, closest
symbols) for the model to correct its own reply next round.
NO-VALIDATION is the capability gate's refusal -- its message already
names the remedy (block-edit), so the retried round can plan the
right command from it.
PROSE is deliberately excluded: an output-contract violation where
the planner replied with prose instead of the TOML action document.
Retrying the identical prompt only repeats the prose and burns
tokens; instead the prose answer stays in the conversation history
for the user to read and re-ask on.")

(defun scalpel-diagnose-self-heal-p (err)
  "Return non-nil when ERR is a plist whose :type is self-healable.
Such an error names a planner-output failure that a retry with
the error text in the conversation can fix; see the variable
`scalpel-diagnose-self-heal-types'."
  (and (listp err)
       (plist-member err :type)
       (memq (plist-get err :type) scalpel-diagnose-self-heal-types)))

(defconst scalpel-diagnose-dialect-error-types
  '((scalpel-llm-dialect-tool-call-error . tool-call)
    (scalpel-llm-dialect-prose-reply-error . prose))
  "Single owner of the dialect-condition-to-planner-error-type mapping.
The dialect module defines the condition symbols; this module decides
which planner error type each one means.  `scalpel-agent-plan'
consults this table instead of keeping its own private copy, so
adding a new dialect condition means touching only this table.")

(provide 'scalpel-diagnose)

;;; scalpel-diagnose.el ends here
