;;; scalpel-locate-elisp.el --- Emacs Lisp locator provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
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

(defconst scalpel-locate-elisp--anonymous-definer-registry
  '()
  "Alist mapping definer symbols to locator functions for anonymous definitions.

Each entry maps a definer symbol (e.g. `cl-defmethod' with an implicit
name, or macros that generate definitions without writing the name in
the source) to a function taking a FILE argument and returning, in
call order, the list of definition names that definer expands to in
that file.

This registry is needed because anonymous names never appear in the
source text: `scalpel-locate-elisp--def-name-regex' and literal name
searches cannot see them, so expansion-time bookkeeping is the only
reliable way to locate such definitions.")

(defun scalpel-locate-elisp-count-definer-forms (definer)
  "Count top-level forms in the current buffer headed by DEFINER.
DEFINER is a symbol naming a definer macro.  Scan using `forward-sexp'
so that only top-level forms are considered.  Read errors and end of
buffer are handled gracefully; this function never signals."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((count 0)
            (definer-name (symbol-name definer)))
        (while (condition-case nil
                   (progn (forward-sexp) t)
                 (scan-error nil)
                 (invalid-regexp nil)
                 (args-out-of-range nil))
          (condition-case nil
              (save-excursion
                (backward-sexp)
                (forward-comment (point-max))
                (when (looking-at (concat "(" (regexp-quote definer-name)
                                          "\\(?:[ \t\n\r]\\|)\\)"))
                  (setq count (1+ count))))
            (error nil)))
        count))))

(defun scalpel-locate-elisp-register-anonymous-definer (symbol function)
  "Register SYMBOL as an anonymous definer whose names FUNCTION supplies.

FUNCTION is called with the file path and must return, in call
order, the definition names the calls of SYMBOL in that file
generate at macro-expansion time.  Re-registering a symbol
replaces its function.  Registration belongs to the package that
defines the macro, because only it knows how the names come
about.  Returns nil."
  (setq scalpel-locate-elisp--anonymous-definer-registry
        (assq-delete-all
         symbol scalpel-locate-elisp--anonymous-definer-registry))
  (push (cons symbol function)
        scalpel-locate-elisp--anonymous-definer-registry)
  nil)

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

(defun scalpel-locate-elisp--top-definition-anonymous-range (symbol file)
  "Locate the range of the anonymous top-level definition of SYMBOL.
Walk the anonymous definer registry: for the first entry whose
function, called with FILE, returns a name list containing SYMBOL,
compute SYMBOL's index in that list, then scan the current buffer's
top-level forms with `read'.  Return (BEG . END) of the definer call
whose occurrence index matches, where BEG is the form start after
leading whitespace and END is point after `read' consumed the form.
Return nil when no registered definer accounts for SYMBOL or when the
walk ends early on a read error."
  (let ((target nil))
    (dolist (entry scalpel-locate-elisp--anonymous-definer-registry)
      (unless target
        (let ((names (funcall (cdr entry) file)))
          (when (member symbol names)
            (setq target (cons (car entry)
                               (- (length names)
                                  (length (member symbol names)))))))))
    (when target
      (let ((definer (car target))
            (index (cdr target))
            (seen 0))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (condition-case nil
                (catch 'done
                  (while t
                    (forward-comment (buffer-size))
                    (let ((beg (point))
                          (form (read (current-buffer))))
                      (when (and (consp form) (eq (car form) definer))
                        (if (eq seen index)
                            (throw 'done (cons beg (point)))
                          (setq seen (1+ seen)))))))
              (error nil))))))))

(defun scalpel-locate-elisp-range (file symbol)
  "Return byte range of SYMBOL definition as (BEG . END) in current buffer.
Current buffer is the Emacs Lisp file referenced by FILE.  A
symbol the literal search cannot find may still be generated by a
registered anonymous definer; that case is tried as a fallback."
  (or (scalpel-locate-elisp--top-definition-range symbol)
      (scalpel-locate-elisp--top-definition-anonymous-range file symbol)))

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

(defun scalpel-locate-elisp-list-symbols (file)
  "Return a list of top-level definition names in current buffer.

Literal names come from `scalpel-locate-elisp--def-name-regex'.
Names a registered anonymous definer generates are asked from its
registry function and merged in, so macros whose definitions carry
no source-level name still answer when the file has been loaded.
Independently of the registry, a load-free source-level fallback
walks the top-level forms of the buffer one at a time: any form
whose head is a member of `scalpel-locate-elisp--defining-forms'
but whose second element is a list rather than a symbol or string
\(e.g. an ert-gwt-deftest call like (ert-gwt-deftest (:given ...)
...) whose clauses follow the head directly) carries no name in
the usual place and counts as an anonymous use; such calls are
counted per definer in source order and synthesized as
\"<definer>-1\", \"<definer>-2\", ..., so even never-loaded files
still contribute a stable count.  FILE is the path the caller
names; anonymous-definer functions receive it unchanged."
  (let ((case-fold-search nil)
        syms)
    ;; Literal names via the source-level regex scan.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward scalpel-locate-elisp--def-name-regex nil t)
        (push (match-string-no-properties 2) syms)))
    (setq syms (nreverse syms))
    ;; Names generated by registered anonymous definers (needs the
    ;; file to have been loaded; failure or nil is tolerated here,
    ;; the source-level fallback below covers those cases).
    (dolist (entry scalpel-locate-elisp--anonymous-definer-registry)
      (let ((names (condition-case nil
                       (funcall (cdr entry) file)
                     (error nil))))
        (when names
          (dolist (name names)
            (push name syms)))))
    ;; Source-level fallback needing no registration: walk the
    ;; top-level forms and synthesize placeholder names for
    ;; anonymous defining calls.
    (save-excursion
      (goto-char (point-min))
      (let ((anon-counts (make-hash-table :test #'eq)))
        (condition-case nil
            (while (not (eobp))
              (let ((form (read (current-buffer))))
                (let ((head (car-safe form)))
                  (when (and (symbolp head)
                             (memq head scalpel-locate-elisp--defining-forms)
                             ;; Anonymous use: no name in the usual
                             ;; place -- the second element is itself
                             ;; a list, e.g. (ert-gwt-deftest (:given
                             ;; ...) ...) with clauses after the head.
                             (consp (cadr form)))
                    (let ((n (1+ (gethash head anon-counts 0))))
                      (puthash head n anon-counts)
                      (push (format "%s-%d" (symbol-name head) n) syms))))))
          ;; A malformed or truncated file still yields whatever we
          ;; managed to read so far.
          (invalid-read-syntax nil)
          (end-of-file nil))))
    (delete-dups (nreverse syms))))

(defun scalpel-locate-elisp-undefined-definers (_file)
  "Scan the current buffer for defining-form heads missing registration.

Read each top-level form and report its head symbol when all of
the following hold: the symbol is not registered in
`scalpel-locate-elisp--defining-forms'; its name contains the
definer morpheme \"def\" (covering \"def-*\", \"define-*\",
\"cl-def*\", \"transient-define-*\" and \"ert-deftest\"); and the
second element of the form is an unquoted symbol, so forms whose
name argument is quoted (such as \"defalias\" calls) are skipped,
matching the registry's own policy.

Return the deduplicated list of such symbol names, in order of
first appearance, so that a missed registration is discoverable
instead of silent.  The scan stops cleanly on a read error at end
of buffer or on an unbalanced file, keeping the symbols collected
so far."
  (save-excursion
    (goto-char (point-min))
    (let ((seen (make-hash-table :test #'equal))
          result)
      (catch 'done
        (while t
          (let ((form (condition-case nil
                          (read (current-buffer))
                        (end-of-file (throw 'done nil))
                        (invalid-read-syntax (throw 'done nil))
                        (scan-error (throw 'done nil)))))
            (when (consp form)
              (let ((head (car form)))
                (when (and (symbolp head)
                           (string-match-p "def" (symbol-name head))
                           (not (member head
                                        scalpel-locate-elisp--defining-forms))
                           (symbolp (cadr form)))
                  (let ((name (symbol-name head)))
                    (unless (gethash name seen)
                      (puthash name t seen)
                      (push name result)))))))))
      (nreverse result))))

(provide 'scalpel-locate-elisp)

;;; scalpel-locate-elisp.el ends here
