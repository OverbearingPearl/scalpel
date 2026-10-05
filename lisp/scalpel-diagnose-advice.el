;;; scalpel-diagnose-advice.el --- Advice text for Scalpel errors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
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

(defvar scalpel-diagnose-advice--perlre-toc-cache nil
  "Cache of the perlre POD table of contents.
Nil means not yet computed; the empty string means extraction was
attempted and failed, so failures are not retried needlessly.")

(defvar scalpel-diagnose-advice--perlre-toc-cache nil
  "Cached table of contents for the perlre POD.
Populated once by `scalpel-diagnose-advice-perlre-toc' and reused
on every subsequent call so that POD extraction is not repeated.")

(defun scalpel-diagnose-advice-perlre-toc ()
  "Return the cached perlre POD table of contents.
This is the single, self-contained advice-layer owner of TOC
extraction.  On the first call the perlre POD file is located via
`perldoc -l perlre', the file is read, and every =head1..=head4
title is collected and indented according to its heading level.
The joined string is cached in
`scalpel-diagnose-advice--perlre-toc-cache' and returned on all
later calls without re-extraction.  Consumers (console error
rendering, agent refusal assembly) rely on this advice-layer
entry point instead of the prompt layer owning the TOC.  Returns
the empty string on any failure; never returns nil."
  (or scalpel-diagnose-advice--perlre-toc-cache
      (let ((raw (condition-case nil
                     (with-temp-buffer
                       (call-process "perldoc" nil t nil "-l" "perlre")
                       (let ((path (string-trim (buffer-string))))
                         (when (and (string-match-p "\\`/.*\\'" path)
                                    (file-readable-p path))
                           (with-current-buffer (find-file-noselect path)
                             (unwind-protect
                                 (buffer-string)
                               (kill-buffer))))))
                   (error nil))))
        (setq scalpel-diagnose-advice--perlre-toc-cache
              (if (not raw)
                  ""
                (with-temp-buffer
                  (insert raw)
                  (goto-char (point-min))
                  (let ((lines nil)
                        (case-fold-search nil))
                    (while (re-search-forward
                            "^=head\\([1-4]\\)[ \t]+\\([^\n]+\\)" nil t)
                      (let ((level (string-to-number (match-string 1)))
                            (title (string-trim (match-string 2))))
                        (push (concat (make-string (* (1- level) 2)
                                                   ?\s)
                                      title)
                              lines)))
                    (if lines
                        (string-join (nreverse lines) "\n")
                      ""))))))))

(defconst scalpel-diagnose-advice-perl-error-types
  '(pattern-no-match)
  "These error types always belong to the perl rewriting path.
They carry the perlre TOC.")

(defun scalpel-diagnose-advice-perlre-toc-for (error)
  "Return the perlre TOC string if ERROR belongs to the perl rewriting path.
ERROR is a plist describing a diagnostic error.  The decision lives
entirely in this advice layer: the error belongs to the perl path when
its type is listed in `scalpel-diagnose-advice-perl-error-types', or
when its message mentions perl, regexp, pattern or substitute
case-insensitively.  Return nil otherwise; the TOC text itself is
still computed by `scalpel-diagnose-advice-perlre-toc'."
  (let ((type (plist-get error :type))
        (message (plist-get error :message)))
    (when (or (member type scalpel-diagnose-advice-perl-error-types)
              (and (stringp message)
                   (string-match-p
                    "\\_<\\(?:perl\\|regexp\\|pattern\\|substitute\\)_\\>"
                    (downcase message))))
      (scalpel-diagnose-advice-perlre-toc))))

(defun scalpel-diagnose-advice-plist (error-plist)
  "Extract :type from ERROR-PLIST and delegate to `scalpel-diagnose-advice-for'."
  (scalpel-diagnose-advice-for (plist-get error-plist :type)))

(defun scalpel-diagnose-advice-mechanical-repair (error-plist)
  "Return a scalpel suggestion fence string for ERROR-PLIST.

The string contains a mechanically repaired version of the failed input,
or nil when no deterministic fix is possible.
A repair that is identical to the failed input yields nil, so a missing
suggestion fence means no mechanical fix was possible.
In the parse branch, advice blocks the model echoed back are
disinfected before repair.
Applies `scalpel-redact-apply' before returning."
  (let* ((type (plist-get error-plist :type))
         (message (plist-get error-plist :message))
         (input (plist-get error-plist :input))
         (content nil)
         (fence-open-rx
          (rx bol (0+ blank)
              "@scalpel@" (0+ blank) "suggestion"))
         (fence-close-rx
          (rx bol (0+ blank)
              "@scalpel@" (0+ blank) "end" (0+ blank) eol)))
    (cond
     ((eq type 'tool-call)
      (let* ((no-xml (replace-regexp-in-string
                      (rx (or "<invoke>" "</invoke>" ""))
                      "" input t t))
             (lines (split-string no-xml "\n" t)))
        (while (and lines (not (string-match-p "^\\[\\[" (car lines))))
          (setq lines (cdr lines)))
        (while (and lines
                    (string-match-p fence-close-rx (car (last lines))))
          (setq lines (butlast lines)))
        (setq content (mapconcat #'identity lines "\n"))))
     ((eq type 'parse)
      (let* ((lines (split-string input "\n")))
        (while (and lines (not (string-match-p "^\\[\\[" (car lines))))
          (setq lines (cdr lines)))
        (while (and lines
                    (string-match-p (rx bol (0+ blank) "]]" (0+ blank) eol)
                                    (car (last lines))))
          (setq lines (butlast lines)))
        ;; Strip any echoed-back advice block: from a fence opener line
        ;; holding "@scalpel@ suggestion" through the next closing fence.
        (let ((kept nil)
              (skipping nil))
          (dolist (line lines)
            (cond
             ((and (not skipping)
                   (string-match-p fence-open-rx line))
              (setq skipping t))
             ((and skipping
                   (string-match-p fence-close-rx line))
              (setq skipping nil))
             ((not skipping)
              (push line kept))))
          (setq lines (nreverse kept)))
        ;; Strip dangling closing fence lines left by the echoed error
        ;; wrapper at the end of the input.
        (while (and lines
                    (string-match-p fence-close-rx (car (last lines))))
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
    (when (and content (not (string-empty-p content))
               (not (string-equal (string-trim content)
                                  (string-trim (or input "")))))
      (scalpel-redact-apply
       (concat "@scalpel@ suggestion\n" content "\n@scalpel@ end")))))

(provide 'scalpel-diagnose-advice)

;;; scalpel-diagnose-advice.el ends here
