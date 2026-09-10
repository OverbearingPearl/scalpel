;;; scalpel-locate-elisp.el --- Emacs Lisp locator provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Structural locator for Emacs Lisp buffers.

;;; Code:

(defconst scalpel-locate-elisp--defining-forms
  '(defun defmacro defvar defcustom defconst)
  "Top-level defining forms recognised as one complete definition.
Structural contract shared by the definition regexes below and by
`scalpel-locate-elisp--single-definition-p'.")

(defconst scalpel-locate-elisp--definer-regexp
  (regexp-opt (mapcar #'symbol-name scalpel-locate-elisp--defining-forms))
  "Regexp matching a single top-level Emacs Lisp definer keyword.
Rendered as a non-capturing group, so inserting it does not shift
any capture used by callers.")

(defconst scalpel-locate-elisp--def-header-prefix
  (concat "^(" scalpel-locate-elisp--definer-regexp "[ \t]+")
  "Regex matching a top-level Emacs Lisp definer up to its name.")

(defconst scalpel-locate-elisp--def-name-regex
  (concat "^(" "\\(" scalpel-locate-elisp--definer-regexp "[ \t]+\\)"
          "\\([^ \t\n()]+\\)")
  "Regex matching a top-level Emacs Lisp definer and capturing its name.")

(defun scalpel-locate-elisp--top-definition-range (symbol)
  "Return (BEG . END) of top-level form named SYMBOL in current buffer.
Return nil when SYMBOL is absent."
  (save-excursion
    (goto-char (point-min))
    (let* ((case-fold-search nil)
           (pattern (format "%s%s\\(?:[ \t\n\r]\\|\\'\\)"
                            scalpel-locate-elisp--def-header-prefix
                            (regexp-quote symbol))))
      (when (re-search-forward pattern nil t)
        (goto-char (match-beginning 0))
        (let ((beg (point)))
          (forward-list 1)
          (cons beg (point)))))))

(defun scalpel-locate-elisp-range (_file symbol)
  "Return byte range of SYMBOL definition as (BEG . END) in current buffer.
Current buffer is the Emacs Lisp file referenced by FILE."
  (scalpel-locate-elisp--top-definition-range symbol))

(defun scalpel-locate-elisp--single-definition-p (text)
  "Return non-nil when TEXT is exactly one top-level defining form."
  (condition-case nil
      (let* ((parsed (read-from-string text))
             (form (car parsed))
             (end (cdr parsed)))
        (and (listp form)
             (memq (car form) scalpel-locate-elisp--defining-forms)
             (= end (length text))))
    (error nil)))

(defun scalpel-locate-elisp-list-symbols (_file)
  "Return a list of top-level definition names in current buffer."
  (let ((case-fold-search nil)
        syms)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward scalpel-locate-elisp--def-name-regex nil t)
        (push (match-string-no-properties 2) syms)))
    (nreverse syms)))

(provide 'scalpel-locate-elisp)

;;; scalpel-locate-elisp.el ends here
