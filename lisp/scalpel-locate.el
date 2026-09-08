;;; scalpel-locate.el --- Deterministic Emacs-Lisp block locator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Bracket-level locator for Emacs Lisp top-level definitions.

;;; Code:

(defconst scalpel-locate--def-header-regex
  "^(def\\(?:un\\|macro\\|var\\|custom\\|const\\)[ \t]+"
  "Regex matching the start of an Emacs Lisp top-level definer.")

(defun scalpel-locate--top-definition-range (symbol)
  "Return (BEG . END) of top-level form named SYMBOL in current buffer.
Signal an error when SYMBOL cannot be found."
  (save-excursion
    (goto-char (point-min))
    (let* ((case-fold-search nil)
           (pattern (format "%s%s\\(?:[ \t\n\r]\\|\\'\\)"
                            scalpel-locate--def-header-regex
                            (regexp-quote symbol))))
      (when (re-search-forward pattern nil t)
        (goto-char (match-beginning 0))
        (let ((beg (point)))
          (forward-list 1)
          (cons beg (point)))))))

(defun scalpel-locate-range (file symbol)
  "Return byte range of SYMBOL in FILE.
Raise user-error if FILE is not loaded or SYMBOL is not found."
  (find-file-noselect file)
  (with-current-buffer (get-file-buffer file)
    (or (scalpel-locate--top-definition-range symbol)
        (user-error "Scalpel: symbol %s not found in %s" symbol file))))

(provide 'scalpel-locate)
;;; scalpel-locate.el ends here
