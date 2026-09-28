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
  '((unknown-tool
     . "the planner named a tool the action contract does not define,
so nothing was executed.  The tool vocabulary is fixed: use one of
reply, file-peek, block-edit, block-insert, block-delete,
file-create, file-rename, file-delete, file-substitute, shell,
confirm, and re-emit the whole document with the corrected tool
name.  A second try may come back whole.  Press C-c C-e or M-x
scalpel-console-repeat to retry")
    (missing-field
     . "the planner's action omitted a required field named in the
error, so nothing was executed.  Re-emit the same action with
every required field filled, checking the field list for that
tool in the system prompt.  A second try may come back whole.
Press C-c C-e or M-x scalpel-console-repeat to retry")
    (no-validation
     . "file-substitute was refused because this file's language has no
structural check (:balanced-p), so a batch rewrite there cannot be
validated.  Redo the edit as one block-edit per named definition
next round; the refusal is what frees the retry, so nothing is
broken.  To try again anyway: C-c C-e")
    (pattern-no-match
     . "the pattern matched nothing in any file, so nothing was
executed.  The target text may already have been rewritten by an
earlier round, leaving the pattern stale.  The planner must
re-read the file with file-peek to confirm the current text
before rewriting the pattern or giving up; resending the
identical action only repeats the refusal")
    (tool-call
     . "the planner violated the output contract by wrapping its answer
in XML-style tags instead of writing the TOML action document
directly; that convention is forbidden here, so nothing was
executed.  A backend that answers this way tends to answer this
way again, so retrying the same request on it rarely helps:
switch the backend with C-c C-b, or rephrase the instruction.  To
try again anyway, C-c C-t analyzes the failure deeply first")
    (prose
     . "the model followed no parseable convention at all, so nothing was
executed.  Retry tends to repeat it: switch the backend with
C-c C-b, or rephrase the instruction.  To try again anyway,
C-c C-t analyzes the failure deeply first")
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

(defun scalpel-diagnose-advice-mechanical-repair (error-plist)
  "Return a scalpel suggestion fence string for ERROR-PLIST.

The string contains a mechanically repaired version of the failed input,
or nil when no deterministic fix is possible.
Applies `scalpel-redact-apply` before returning."
  (let* ((type (plist-get error-plist :type))
         (message (plist-get error-plist :message))
         (input (plist-get error-plist :input))
         (content nil))
    (cond
     ((eq type 'tool-call)
      (let* ((no-xml (replace-regexp-in-string
                      (rx (or "<invoke>" "</invoke>" "<tool_call>" "</tool_call>"))
                      "" input t t))
             (lines (split-string no-xml "\n" t))
             (toml-lines (seq-filter
                          (lambda (l)
                            (or (string-match
                                 (rx bol "[[" (literal "action") "]]") l)
                                (string-match
                                 (rx bol "tool" (one-or-more blank) "=") l)))
                          lines)))
        (setq content (mapconcat #'identity toml-lines "\n"))))
     ((eq type 'parse)
      (let ((lines (split-string input "\n")))
        (while (and lines (not (string-match-p "^\\[\\[" (car lines))))
          (setq lines (cdr lines)))
        (while (and lines
                    (string-match (rx bol (0+ blank) "]]" (0+ blank) eol)
                                  (car (last lines))))
          (setq lines (butlast lines)))
        (setq content (mapconcat #'identity lines "\n"))))
     ((eq type 'no-such-symbol)
      (when (string-match
             (rx "No such symbol:" (one-or-more blank)
                 (group (one-or-more (not (any "." ","))))
                 ", did you mean:" (one-or-more blank)
                 (group (one-or-more (not (any ".")))))
             message)
        (let ((bad-symbol (match-string 1 message))
              (good-symbol (match-string 2 message)))
          (setq content (replace-regexp-in-string
                         (concat "\\b" (regexp-quote bad-symbol) "\\b")
                         good-symbol input t t)))))
     ((eq type 'pattern-no-match)
      (when (string-match
             (rx "near-miss line:" (one-or-more blank)
                 (group (one-or-more any)))
             message)
        (let ((line (match-string 1 message)))
          (setq content (concat "; Hint: near-miss line: " line)))))
     ((eq type 'prose)
      (let ((idx (string-match "\\[\\[" input)))
        (when idx
          (setq content (substring input idx)))))
     (t nil))
    (when (and content (not (string-empty-p content)))
      (scalpel-redact-apply
       (format "```scalpel suggestion\n%s\n```" content)))))

(provide 'scalpel-diagnose-advice)

;;; scalpel-diagnose-advice.el ends here
