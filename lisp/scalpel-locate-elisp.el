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
  '(defun defsubst defmacro defvar defvar-local defcustom defconst
    defgroup defface
    cl-defun cl-defmacro cl-defmethod cl-defgeneric cl-defstruct
    define-derived-mode define-minor-mode define-globalized-minor-mode
    define-generic-mode define-skeleton define-inline
    transient-define-prefix transient-define-suffix
    transient-define-argument
    ert-deftest)
  "Top-level defining forms recognised as one complete definition.
Structural contract shared by the definition regexes below and by
`scalpel-locate-elisp--single-definition-p'.

A form belongs here when its second element is the unquoted name
the form defines, which is what lets `--def-name-regex' read that
name and `--top-definition-range' find the form again from it.  The
core Emacs Lisp definers come first, then the mode and group
spellings, then the cl-lib ones, then the transient spellings this
package's menus are written with, then ERT.

Membership is a contract of spellings, not of one package's
vocabulary, so a macro from a package belongs here when the files a
session edits are written with it.  Leaving one out costs more than
a convenience: a name the list misses is reported \"not found\" by
`scalpel-locate-range' even though the file plainly holds it, and
every round that names it dies before it can be edited --
`llm-pick-view-mode' at its `define-derived-mode' form was, and
`llm-pick-menu' at its `transient-define-prefix' one was too.  Such
a name is also absent from `list-symbols', so the planner is never
told it exists.

A definer whose name is *quoted* stays out.  `defalias',
`defvaralias' and `define-error' are the common shapes: the reader
hands back the form `(quote name)', and the locator -- which
searches for the symbol as a bare word -- could never match that
spelling, turning one not-found failure into a permanent one.")

(defconst scalpel-locate-elisp--definer-regexp
  (concat "\\(?:" (regexp-opt
                   (mapcar #'symbol-name
                           scalpel-locate-elisp--defining-forms))
          "\\)")
  "Regexp matching a single top-level Emacs Lisp definer keyword.
Rendered as one shy group -- wrapped explicitly here, not by
passing a PAREN argument, because `regexp-opt's grouping for a
non-nil PAREN is not something to bet on: the run that motivated
the shy flag left the group capturing, which shifted every capture
number in `--def-name-regex' and made `list-symbols' read the
definer keyword instead of the name.  The embedding matters more
than the matching: `--def-header-prefix' and `--def-name-regex'
splice this into larger patterns between \"^(\" and \"[ \\t]+\", so
an ungrouped alternation would tear the pattern apart, and a
capturing one would shift the captures.  The explicit shy wrapper
rules out both, whatever `regexp-opt' emits inside.")

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
           (pattern (format "%s%s\\(?:[ \t\n\r()]\\|\\'\\)"
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
  "Return non-nil when TEXT is one or more complete defining forms.
Each top-level form must parse cleanly to the end of TEXT and be one
of `scalpel-locate-elisp--defining-forms'; no trailing garbage is
allowed after the last form."
  (condition-case nil
      (let ((pos 0)
            (count 0))
        (while (< pos (length text))
          (let* ((parsed (read-from-string text pos))
                 (form (car parsed))
                 (end (cdr parsed)))
            (unless (and (listp form)
                         (memq (car form) scalpel-locate-elisp--defining-forms))
              (error "not a defining form"))
            (setq pos end
                  count (1+ count))))
        (> count 0))
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
