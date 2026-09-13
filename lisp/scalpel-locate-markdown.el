;;; scalpel-locate-markdown.el --- Markdown locator provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Structural locator for Markdown buffers.  A "definition" is a heading
;; section: the heading line plus everything up to the next heading of
;; the same or higher level.  The symbol is the heading text.

;;; Code:

(require 'cl-lib)

(defconst scalpel-locate-markdown--heading-regexp
  "^\\(#+\\)[ \t]+\\(.*?\\)[ \t]*$"
  "Regexp matching an ATX heading, capturing its level and text.")

(defun scalpel-locate-markdown--heading-level (line)
  "Return the heading level of LINE, or nil when LINE is no heading."
  (and (string-match scalpel-locate-markdown--heading-regexp line)
       (length (match-string-no-properties 1 line))))

(defun scalpel-locate-markdown--next-heading (level)
  "Move point to the next heading of level LEVEL or shallower.
Return non-nil when one is found; point is left at its line start."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward scalpel-locate-markdown--heading-regexp nil t))
      (when (<= (length (match-string-no-properties 1)) level)
        (setq found t)
        (goto-char (match-beginning 0))))
    found))

(defun scalpel-locate-markdown--top-definition-range (symbol)
  "Return (BEG . END) of the heading section titled SYMBOL.
The section runs from the heading line to just before the next
heading of the same or higher level, or to `point-max'.  Trailing
blank lines are excluded.  Return nil when SYMBOL is absent."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (pattern (format "\\`%s[ \t]*\\'" (regexp-quote symbol)))
          found)
      (while (and (not found)
                  (re-search-forward scalpel-locate-markdown--heading-regexp nil t))
        (when (string-match-p pattern (match-string-no-properties 2))
          (setq found t)))
      (when found
        (let* ((level (length (match-string-no-properties 1)))
               (beg (match-beginning 0))
               (end (if (scalpel-locate-markdown--next-heading level)
                        (point)
                      (point-max))))
          ;; Do not hand back blank lines that belong to the gap
          ;; before the next section.
          (goto-char end)
          (skip-chars-backward " \t\n\r")
          (if (< (point) beg)
              (cons beg (1+ beg))
            (cons beg (point))))))))

(defun scalpel-locate-markdown-range (_file symbol)
  "Return the range of SYMBOL's section as (BEG . END).
Current buffer is the Markdown file referenced by FILE."
  (scalpel-locate-markdown--top-definition-range symbol))

(defun scalpel-locate-markdown--single-definition-p (text)
  "Return non-nil when TEXT is exactly one heading section.
TEXT must start with a heading and hold no later heading of the
same or higher level, so a replacement can only ever land on one
section."
  (let ((case-fold-search nil)
        (lines (split-string text "\n")))
    (when (and lines (string-match-p scalpel-locate-markdown--heading-regexp
                                     (car lines)))
      (let ((level (scalpel-locate-markdown--heading-level (car lines))))
        (not (cl-some
              (lambda (line)
                (and (scalpel-locate-markdown--heading-level line)
                     (<= (scalpel-locate-markdown--heading-level line) level)))
              (cdr lines)))))))

(defun scalpel-locate-markdown-list-symbols (_file)
  "Return the list of heading texts in the current buffer."
  (let ((case-fold-search nil)
        syms)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward scalpel-locate-markdown--heading-regexp nil t)
        (push (match-string-no-properties 2) syms)))
    (nreverse syms)))

(provide 'scalpel-locate-markdown)

;;; scalpel-locate-markdown.el ends here
