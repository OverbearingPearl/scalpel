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
(require 'scalpel-locate-markdown)
(require 'scalpel-locate-yaml)
(require 'scalpel-locate-gitignore)

(defvar scalpel-locate-providers nil
  "Alist of (REGEXP . PROVIDER-PLIST) for registered locator providers.

PROVIDER-PLIST keys:
:locate -- function (FILE SYMBOL), returns (BEG . END) or nil.
:list-symbols -- function (FILE), returns list of symbol name strings.
:single-definition-p -- function (TEXT), returns non-nil when TEXT is
exactly one complete top-level definition in this language.
:single-form-p -- function (TEXT), returns non-nil when TEXT is exactly
one complete top-level form in this language, whether or not it defines
a name.  `scalpel-locate-single-form-p' falls back to
:single-definition-p when a provider registers no :single-form-p: a
language whose top-level units are its definitions has no separate form
to validate.  Emacs Lisp is the exception the key exists for, because a
top-level unit there may be a call.
:form-count -- function (TEXT), returns how many complete top-level
forms TEXT holds, or nil when the reader cannot finish it.
`scalpel-locate-form-count' answers nil when a provider registers no
:form-count, because the count explains a refusal rather than deciding
one: a language whose top-level units are its definitions has no
separate count to add.
:balanced-p -- function (TEXT), returns non-nil when TEXT's brackets
balance.  `scalpel-locate-balanced-p' answers t when a provider
registers no :balanced-p: a language that says nothing about bracket
shape must not be reported as having a bracket problem.")

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

(defun scalpel-locate--sync-buffer (file)
  "Return a buffer visiting FILE whose text matches the disk.
A file changed behind Emacs -- a git checkout, another tool's
write -- would otherwise be located in stale text and a definition
present on disk reported missing.  An existing buffer is handled
here rather than through `find-file-noselect': that function
prompts to discard unsaved edits when the file changed, and the
answer must never decide a locate.  A modified buffer is left
alone, because reverting it would discard the user's work; an
unmodified stale buffer is reverted without asking."
  (let ((buffer (find-buffer-visiting file)))
    (if buffer
        (with-current-buffer buffer
          ;; A modified buffer keeps the user's in-flight text; an
          ;; unmodified one that fell behind the disk is refreshed,
          ;; NOCONFIRM because there is nothing to lose.
          (when (and (not (buffer-modified-p))
                     (not (verify-visited-file-modtime buffer)))
            (revert-buffer t t t))
          buffer)
      (find-file-noselect file))))

(defun scalpel-locate-range (file symbol)
  "Return byte range (BEG . END) of SYMBOL in FILE.
Open FILE if needed.  Signal `user-error' when no locator is
registered for FILE or SYMBOL cannot be found."
  (scalpel-locate--sync-buffer file)
  (let* ((provider (scalpel-locate-provider-for-file file))
         (locate (and provider (plist-get provider :locate))))
    (unless locate
      (user-error "Scalpel: no locator registered for %s" file))
    (with-current-buffer (get-file-buffer file)
      (or (funcall locate file symbol)
          (user-error "Scalpel: symbol %s not found in %s" symbol file)))))

(defun scalpel-locate-single-definition-p (file text)
  "Return non-nil when TEXT is one complete top-level definition in FILE.
Dispatch to FILE's provider.  Signal `user-error' when no provider
handles FILE or the provider does not validate replacements."
  (let* ((provider (scalpel-locate-provider-for-file file))
         (pred (and provider (plist-get provider :single-definition-p))))
    (unless pred
      (user-error "Scalpel: no replacement validator registered for %s" file))
    (funcall pred text)))

(defun scalpel-locate-single-form-p (file text)
  "Return non-nil when TEXT is exactly one complete top-level form in FILE.
This is the question an insertion asks, and it is not the one
`scalpel-locate-single-definition-p' answers: a top-level unit need
not define a name, and an Emacs Lisp file's own registration idiom --
`scalpel-locate-register-provider', `scalpel-llm-dialect-register' --
is a call, which no definition validator admits.  Dispatch to FILE's
provider, and fall back to its definition validator when it registers
no :single-form-p, because a language whose top-level units are its
definitions -- every provider but Emacs Lisp -- has no separate form to
check.  Signal `user-error' when FILE has no provider, or when that
provider registers neither validator."
  (let* ((provider (scalpel-locate-provider-for-file file))
         (pred (and provider
                    (or (plist-get provider :single-form-p)
                        (plist-get provider :single-definition-p)))))
    (unless pred
      (user-error "Scalpel: no form validator registered for %s" file))
    (funcall pred text)))

(defun scalpel-locate-form-count (file text)
  "Return the number of complete top-level forms in TEXT for FILE's language.
Dispatch to FILE's provider.  Nil is the answer both for a text the
language's reader cannot finish and for a language whose provider
counts no forms at all, because this predicate explains a refusal
rather than deciding one: a caller that cannot be told how many forms
a text holds must say nothing about how many it holds, which is why
`scalpel-locate-balanced-p' is asked alongside it.

The count is what separates a text holding several forms from one no
reader reaches the end of.  The boolean `scalpel-locate-single-form-p'
cannot tell those apart, and a refusal that named the wrong one would
send the planner after a defect that is not there."
  (let* ((provider (scalpel-locate-provider-for-file file))
         (counter (and provider (plist-get provider :form-count))))
    (when counter (funcall counter text))))

(defun scalpel-locate-balanced-p (file text)
  "Return non-nil when TEXT's brackets balance in FILE's language.
Dispatch to FILE's provider.  A provider registering no :balanced-p
answers t, so a language that says nothing about bracket shape is
never reported as having one: this predicate exists to name the cause
of a refusal, and \"no answer\" is not a cause.  The default is a
stated silence, not a claim that TEXT is well formed.

Only a language whose provider counts forms can reach this question --
`scalpel-locate-form-count' is asked first -- so the answer refines a
refusal instead of deciding whether to make one."
  (let* ((provider (scalpel-locate-provider-for-file file))
         (pred (and provider (plist-get provider :balanced-p))))
    (if pred (funcall pred text) t)))

(defun scalpel-locate-list-symbols (file)
  "Return a list of top-level symbol names in FILE.
Open FILE if needed.  Signal `user-error' when no locator is
registered for FILE."
  (scalpel-locate--sync-buffer file)
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
       :list-symbols #'scalpel-locate-elisp-list-symbols
       :single-definition-p #'scalpel-locate-elisp--single-definition-p
       :single-form-p #'scalpel-locate-elisp--single-form-p
       :form-count #'scalpel-locate-elisp--form-count
       :balanced-p #'scalpel-locate-elisp--balanced-p))

;; Built-in Markdown provider.
(scalpel-locate-register-provider
 "\\.md\\'"
 (list :locate #'scalpel-locate-markdown-range
       :list-symbols #'scalpel-locate-markdown-list-symbols
       :single-definition-p #'scalpel-locate-markdown--single-definition-p))

(provide 'scalpel-locate)

;;; scalpel-locate.el ends here
