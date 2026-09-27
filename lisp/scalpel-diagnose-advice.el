;;; scalpel-diagnose-advice.el --- Advice text for Scalpel errors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The per-failure advice tables and their lookups, extracted from
;; `scalpel-diagnose'.  The categories and type lists stay in
;; `scalpel-diagnose'; this module owns only the remedy text and how
;; an error type or plist resolves to it.  It requires
;; `scalpel-diagnose' for the category fallback, so the console must
;; require this module to reach `scalpel-diagnose-advice-table' and
;; `scalpel-diagnose-advice-for'.

;;; Code:

(require 'scalpel-diagnose)

(defconst scalpel-diagnose-advice-table
  '((no-validation
     . "file-substitute was refused because this file's language has no
structural check (:balanced-p), so a batch rewrite there cannot be
validated.  Redo the edit as one block-edit per named definition
next round; the refusal is what frees the retry, so nothing is
broken.  To try again anyway: C-c C-e")
    (tool-call
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
back to variable `scalpel-diagnose-advice-category-table' via the
type's category when no exact entry matches.
NO-VALIDATION has its own entry because its remedy is a
different tool, not a corrected spelling of the same one.")

(defconst scalpel-diagnose-advice-category-table
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
variable `scalpel-diagnose-advice-category-table' has no entry for the exact
type, so a new planner type needs no advice of its own.  A type
whose category is absent here resolves to the executor advice.")

(defun scalpel-diagnose-advice-for (type)
  "Return the advice string for error TYPE, or nil if unknown.
An exact entry in the variable `scalpel-diagnose-advice-table' wins;
otherwise the entry named after the type's category in the
variable `scalpel-diagnose-advice-category-table' is used.  Unknown
types fall to the executor entry, which that table always holds."
  (or (cdr (assq type scalpel-diagnose-advice-table))
      (cdr (assq (scalpel-diagnose-category type)
                 scalpel-diagnose-advice-category-table))))

(defun scalpel-diagnose-advice-plist (error-plist)
  "Extract :type from ERROR-PLIST and delegate to `scalpel-diagnose-advice-for'."
  (scalpel-diagnose-advice-for (plist-get error-plist :type)))

(provide 'scalpel-diagnose-advice)

;;; scalpel-diagnose-advice.el ends here
