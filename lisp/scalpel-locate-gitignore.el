;;; scalpel-locate-gitignore.el --- .gitignore locator provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Structural locator for .gitignore buffers.  A "definition" is one
;; pattern line; the symbol is the line's text.  Comment and blank
;; lines are never symbols.

;;; Code:

(defconst scalpel-locate-gitignore--pattern-regexp
  "^[ \t]*\\([^ \t\n#][^ \t\n]*\\)[ \t]*$"
  "Regexp matching a .gitignore pattern line, capturing its text.
The negated classes exclude newlines explicitly: a negated class
without `\\n' matches across lines, which would swallow the whole
buffer as one pattern.")

(defun scalpel-locate-gitignore--top-definition-range (symbol)
  "Return (BEG . END) of the pattern line SYMBOL.
The range covers the whole line, newline included.  Return nil
when SYMBOL is absent."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward scalpel-locate-gitignore--pattern-regexp
                                     nil t))
        (when (string= (match-string-no-properties 1) symbol)
          (setq found t)))
      (when found
        (let ((beg (match-beginning 0)))
          (goto-char beg)
          (end-of-line)
          (when (< (point) (point-max))
            (forward-char 1))
          (cons beg (point)))))))

(defun scalpel-locate-gitignore-range (_file symbol)
  "Return the range of SYMBOL's line as (BEG . END).
Current buffer is the .gitignore file referenced by FILE."
  (scalpel-locate-gitignore--top-definition-range symbol))

(defun scalpel-locate-gitignore--single-definition-p (text)
  "Return non-nil when TEXT is exactly one pattern line.
TEXT must be a single line holding one pattern, with or without
its trailing newline."
  (let ((trimmed (string-trim-right text "\n")))
    (and (not (string-match-p "\n" trimmed))
         (string-match-p scalpel-locate-gitignore--pattern-regexp trimmed))))

(defun scalpel-locate-gitignore-list-symbols (_file)
  "Return the list of pattern lines in the current buffer."
  (let (patterns)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward scalpel-locate-gitignore--pattern-regexp nil t)
        (push (match-string-no-properties 1) patterns)))
    (nreverse patterns)))

(provide 'scalpel-locate-gitignore)

;;; scalpel-locate-gitignore.el ends here
