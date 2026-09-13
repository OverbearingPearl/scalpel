;;; scalpel-locate-yaml.el --- YAML locator provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Structural locator for YAML buffers.  A "definition" is one top-level
;; key's block: the key line plus everything up to the next line that is
;; neither indented nor blank.  The symbol is the key text.

;;; Code:

(require 'cl-lib)

(defconst scalpel-locate-yaml--key-regexp
  "^\\([^ \t#][^:]*?\\):[ \t]*\\($\\|[^\n]*\\)"
  "Regexp matching a top-level key line, capturing the key text.")

(defun scalpel-locate-yaml--top-definition-range (symbol)
  "Return (BEG . END) of top-level key SYMBOL's block.
The block runs from the key line to just before the next top-level
key or other non-indented line, or to `point-max'.  Trailing blank
lines are excluded.  Return nil when SYMBOL is absent."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward scalpel-locate-yaml--key-regexp nil t))
        (when (string= (match-string-no-properties 1) symbol)
          (setq found t)))
      (when found
        (let ((beg (match-beginning 0)))
          ;; The block ends at the next line that starts without
          ;; indentation: a top-level key or a document marker.
          (forward-line 1)
          (while (and (< (point) (point-max))
                      (or (looking-at-p "[ \t]")
                          (looking-at-p "[ \t]*$")))
            (forward-line 1))
          ;; Do not hand back blank lines that belong to the gap
          ;; before the next block.
          (goto-char (point))
          (skip-chars-backward " \t\n\r")
          (if (< (point) beg)
              (cons beg (1+ beg))
            (cons beg (point))))))))

(defun scalpel-locate-yaml-range (_file symbol)
  "Return the range of SYMBOL's block as (BEG . END).
Current buffer is the YAML file referenced by FILE."
  (scalpel-locate-yaml--top-definition-range symbol))

(defun scalpel-locate-yaml--single-definition-p (text)
  "Return non-nil when TEXT is exactly one top-level key block.
TEXT must start with a top-level key and hold no later top-level
key, so a replacement can only ever land on one block."
  (let ((lines (split-string text "\n")))
    (when (and lines (string-match-p scalpel-locate-yaml--key-regexp
                                     (car lines)))
      (not (cl-some (lambda (line)
                      (string-match-p scalpel-locate-yaml--key-regexp line))
                    (cdr lines))))))

(defun scalpel-locate-yaml-list-symbols (_file)
  "Return the list of top-level key texts in the current buffer."
  (let (keys)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward scalpel-locate-yaml--key-regexp nil t)
        (push (match-string-no-properties 1) keys)))
    (nreverse keys)))

(provide 'scalpel-locate-yaml)

;;; scalpel-locate-yaml.el ends here
