;;; scalpel-locate-org.el --- Org locator provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Structural locator for Org buffers.  A "definition" is a headline
;; subtree: the headline line plus everything up to the next headline
;; of the same or higher level.  The symbol is the headline text.
;; Blocks delimited by #+begin_src / #+end_src are tracked so headline
;; text inside them is never reported.

;;; Code:

(require 'cl-lib)

(defconst scalpel-locate-org--headline-regexp
  "^\\(\\*+\\)[ \t]+\\(.*?\\)[ \t]*$"
  "Regexp matching an Org headline, capturing its stars and text.")

(defconst scalpel-locate-org--block-regexp
  "^[ \t]*#\\+\\(?:begin\\|end\\)_\\([^ \t\n\r]+\\)"
  "Regexp matching an Org block delimiter line.
Group 1 captures the block type; a matching begin/end pair with
the same type opens and closes a block.")

(defun scalpel-locate-org--headline-level (line)
  "Return the heading level of LINE, or nil when LINE is no headline."
  (and (string-match scalpel-locate-org--headline-regexp line)
       (length (match-string-no-properties 1 line))))

(defun scalpel-locate-org--next-headline (level)
  "Move point to the next headline of level LEVEL or shallower.
Return non-nil when one is found; point is left at its line start."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward scalpel-locate-org--headline-regexp nil t))
      (when (<= (length (match-string-no-properties 1)) level)
        (setq found t)
        (goto-char (match-beginning 0))))
    found))

(defun scalpel-locate-org--top-definition-range (symbol)
  "Return (BEG . END) of the headline subtree titled SYMBOL.
The subtree runs from the headline line to just before the next
headline of the same or higher level, or to `point-max'.
Trailing blank lines are excluded.  Return nil when SYMBOL is
absent."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (pattern (format "\\`%s[ \t]*\\'" (regexp-quote symbol)))
          found)
      (while (and (not found)
                  (re-search-forward scalpel-locate-org--headline-regexp nil t))
        (when (string-match-p pattern (match-string-no-properties 2))
          (setq found t)))
      (when found
        (let* ((level (length (match-string-no-properties 1)))
               (beg (match-beginning 0))
               (end (if (scalpel-locate-org--next-headline level)
                        (point)
                      (point-max))))
          ;; Do not hand back blank lines that belong to the gap
          ;; before the next subtree.
          (goto-char end)
          (skip-chars-backward " \t\n\r")
          (if (< (point) beg)
              (cons beg (1+ beg))
            (cons beg (point))))))))

(defun scalpel-locate-org-range (_file symbol)
  "Return the range of SYMBOL's subtree as (BEG . END).
Current buffer is the Org file referenced by FILE."
  (scalpel-locate-org--top-definition-range symbol))

(defun scalpel-locate-org--single-definition-p (text)
  "Return non-nil when TEXT is exactly one headline subtree.
TEXT must start with a headline and hold no later headline of the
same or higher level, so a replacement can only ever land on one
subtree."
  (let ((case-fold-search t)
        (lines (split-string text "\n")))
    (when (and lines (string-match-p scalpel-locate-org--headline-regexp
                                     (car lines)))
      (let ((level (scalpel-locate-org--headline-level (car lines))))
        (not (cl-some
              (lambda (line)
                (let ((l (scalpel-locate-org--headline-level line)))
                  (and l (<= l level))))
              (cdr lines)))))))

(defun scalpel-locate-org-list-symbols (_file)
  "Return the list of headline texts in the current buffer.
Skip #+begin_src-style blocks: headline-looking lines inside them
are not reported."
  (let ((case-fold-search t)
        (block-stack nil)
        syms)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (cond
           ((string-match scalpel-locate-org--block-regexp line)
            (let ((type (downcase (match-string 1 line))))
              (if (string-match-p "#\\+begin_" line)
                  (push type block-stack)
                ;; End delimiter pops the matching type.
                (when (member type block-stack)
                  (setq block-stack
                        (cl-remove type block-stack
                                   :count 1 :test #'string=))))))
           ((and (not block-stack)
                 (string-match scalpel-locate-org--headline-regexp line))
            (push (match-string-no-properties 2 line) syms))))
        (forward-line 1)))
    (nreverse syms)))

(defun scalpel-locate-org-register-provider ()
  "Register the org locate provider with Scalpel."
  (scalpel-locate-register-provider
   "\\.org\\'"
   (list :locate 'scalpel-locate-org-range
         :list-symbols 'scalpel-locate-org-list-symbols
         :single-definition-p 'scalpel-locate-org--single-definition-p)))

(provide 'scalpel-locate-org)

;;; scalpel-locate-org.el ends here
