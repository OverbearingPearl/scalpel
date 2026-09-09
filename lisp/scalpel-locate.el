;;; scalpel-locate.el --- Deterministic block locator dispatch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Dispatch to per-language locator providers by file-name regexp.

;;; Code:

(require 'cl-lib)
(require 'scalpel-locate-elisp)

(defvar scalpel-locate-providers nil
  "Alist of (REGEXP . PROVIDER-PLIST) for registered locator providers.

PROVIDER-PLIST keys:
:locate -- function (FILE SYMBOL), returns (BEG . END) or nil.
:list-symbols -- function (FILE), returns list of symbol name strings.")

(defun scalpel-locate-register-provider (regexp provider)
  "Register PROVIDER for file names matching REGEXP.
PROVIDER is a plist with :locate and :list-symbols entries.
Registering the same REGEXP replaces the previous provider."
  (setq scalpel-locate-providers
        (cons (cons regexp provider)
              (cl-remove-if (lambda (entry)
                              (string= (car entry) regexp))
                            scalpel-locate-providers))))

(defun scalpel-locate-provider-for-file (file)
  "Return provider plist for FILE, or nil."
  (let ((entry (cl-find-if (lambda (entry)
                             (string-match-p (car entry) file))
                           scalpel-locate-providers)))
    (and entry (cdr entry))))

(defun scalpel-locate-range (file symbol)
  "Return byte range (BEG . END) of SYMBOL in FILE.
Open FILE if needed.  Signal `user-error' when no locator is
registered for FILE or SYMBOL cannot be found."
  (find-file-noselect file)
  (let* ((provider (scalpel-locate-provider-for-file file))
         (locate (and provider (plist-get provider :locate))))
    (unless locate
      (user-error "Scalpel: no locator registered for %s" file))
    (with-current-buffer (get-file-buffer file)
      (or (funcall locate file symbol)
          (user-error "Scalpel: symbol %s not found in %s" symbol file)))))

(defun scalpel-locate-list-symbols (file)
  "Return a list of top-level symbol names in FILE.
Open FILE if needed.  Signal `user-error' when no locator is
registered for FILE."
  (find-file-noselect file)
  (let* ((provider (scalpel-locate-provider-for-file file))
         (list-symbols (and provider (plist-get provider :list-symbols))))
    (unless list-symbols
      (user-error "Scalpel: no locator registered for %s" file))
    (with-current-buffer (get-file-buffer file)
      (funcall list-symbols file))))

;; Built-in Emacs Lisp provider.
(scalpel-locate-register-provider
 "\\.el\\'"
 (list :locate #'scalpel-locate-elisp-range
       :list-symbols #'scalpel-locate-elisp-list-symbols))

(provide 'scalpel-locate)
;;; scalpel-locate.el ends here
