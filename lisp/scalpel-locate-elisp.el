;;; scalpel-locate-elisp.el --- Emacs Lisp locator provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Structural locator for Emacs Lisp buffers.

;;; Code:

(require 'subr-x)

(defconst scalpel-locate-elisp--defining-forms
  '(defun defsubst defmacro defvar defvar-local defcustom defconst
    defgroup defface
    cl-defun cl-defmacro cl-defmethod cl-defgeneric cl-defstruct
    define-derived-mode define-minor-mode define-globalized-minor-mode
    define-generic-mode define-skeleton define-inline
    transient-define-prefix transient-define-suffix
    transient-define-argument
    ert-deftest ert-gwt-deftest)
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
              (error "Not a defining form"))
            (setq pos end
                  count (1+ count))))
        (> count 0))
    (error nil)))

(defun scalpel-locate-elisp--form-count (text)
  "Return the number of complete top-level forms in TEXT, or nil.
TEXT is read as Emacs Lisp.  Nil means the reader cannot reach the
end of TEXT -- brackets left open somewhere, a character or string
literal it cannot finish -- so the count says nothing about how many
forms were meant, and a caller must read it as \"cannot tell\", never
as \"none\".

This is `--single-form-p' as a number rather than a yes or no, and
that is what a refused reply needs: a text holding several forms and
one no reader reaches the end of get different advice, and a boolean
cannot tell them apart.  Layout between forms is skipped by the
reader itself, so nothing here depends on how the reply is spaced."
  (condition-case nil
      (let ((text (string-trim text))
            (pos 0)
            (count 0))
        (while (< pos (length text))
          (let* ((parsed (read-from-string text pos))
                 (end (cdr parsed)))
            ;; A form that consumed no input would leave this loop asking
            ;; at the same position forever.  The comparison is END
            ;; against POS, not POS against END: the reader returns where
            ;; it stopped, and it stops ahead of where it started, so the
            ;; inverted test read every finished form as a stall.
            (when (<= end pos)
              (error "No input consumed"))
            (setq pos end
                  count (1+ count))))
        count)
    (error nil)))

(defun scalpel-locate-elisp--balanced-p (text)
  "Return non-nil for balanced Emacs Lisp over the whole of TEXT.
Every top-level form is walked to the end of TEXT, so a bracket left
open anywhere in it is seen rather than only in the first form.  The
walk runs in `emacs-lisp-mode' with its hooks delayed, because the
syntax table is what reads strings, comments and character literals
the way an Emacs Lisp file does: a bracket inside a docstring or a
comment is text, not structure.  Blank space between forms is
skipped, so a text whose forms are separated by blank lines is still
read whole.

This answers the one question `--form-count' leaves open when it
returns nil: whether the reader stopped because brackets do not
balance, or for a cause brackets cannot name.

The walk is bounded by construction, because an unbounded one hung
the editor: every step either moves to the end of the form the scan
returned or steps over one character, so the position always
advances.  `scan-sexps' answers with a position and leaves point
where it was, which is why the walk goes to that position itself; a
step that scans nothing is a character no scan moves over, and this
answer only ever becomes the wording of a refusal, so walking past it
decides nothing, while a loop that keeps asking at the same position
never returns at all."
  (with-temp-buffer
    (delay-mode-hooks (emacs-lisp-mode))
    (insert text)
    (goto-char (point-min))
    (condition-case nil
        (progn
          (while (progn (skip-chars-forward " \t\n\r")
                        (forward-comment 1)
                        (skip-chars-forward " \t\n\r")
                        (< (point) (point-max)))
            (let* ((from (point))
                   (next (scan-sexps from 1)))
              ;; `scan-sexps' returns the end of the form without moving
              ;; point, so the walk moves there itself; only a scan that
              ;; makes no progress is stepped over, because that is a
              ;; character no scan can cross.
              (if (and next (> next from))
                  (goto-char next)
                (forward-char 1))))
          t)
      (scan-error nil))))

(defun scalpel-locate-elisp--single-form-p (text)
  "Return non-nil when TEXT is exactly one complete top-level form.
The form need not define anything.  `--single-definition-p' answers
the question a replacement landing on a located range asks, where a
text that defines nothing would delete the definition it replaced;
this answers the question an insertion asks, where a registration
call -- `scalpel-locate-register-provider',
`scalpel-llm-dialect-register' -- is a top-level unit of the file and
belongs in it like any definition.

Exactly one form is required.  A reply that echoes a whole file holds
several, and accepting it would append the file to itself.

A list form is required, so a bare word is refused: the reader hands
prose such as \"No change needed.\" back as the symbol `No', which
would otherwise be inserted as a form.

The count comes from `--form-count', so the two judgements cannot
disagree about how many forms a text holds; only the list requirement
is checked here, because it is about accepting a reply rather than
about explaining one."
  (and (eql 1 (scalpel-locate-elisp--form-count text))
       (consp (car (read-from-string text)))))

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
