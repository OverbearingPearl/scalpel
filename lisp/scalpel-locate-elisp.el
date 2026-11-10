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
    ert-deftest ert-gwt-deftest
    use-package)
  "Top-level defining forms recognised as one complete definition.
Structural contract shared by the definition regexes below and by
`scalpel-locate-elisp--single-definition-p'.

A form belongs here when its second element is the unquoted name
the form defines, which is what lets `--def-name-regex' read that
name and `--top-definition-range' find the form again from it.  The
core Emacs Lisp definers come first, then the mode and group
spellings, then the cl-lib ones, then the transient spellings this
package's menus are written with, then ERT, and finally
`use-package'.

`use-package' is the block-granularity case: its second element is
the unquoted package name, so an init file holding only `require',
`use-package' and `provide' still yields addressable symbols.  The
definitions nested in its `:config' body remain unlisted -- the
locator reads source rather than expansion, and only the top-level
form is one complete definition.

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

(require 'cl-lib)

(defvar scalpel-locate-elisp--anonymous-definer-registry
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
reliable way to locate such definitions.

Use `defvar' rather than `defconst' because adapter files (e.g.
scalpel-locate-elisp-ert-gwt.el) may be loaded before this file when
sorted by filename and have already registered entries; re-evaluation
by `defconst' would wipe those registrations.")

(defun scalpel-locate-elisp-register-anonymous-definer (symbol function)
  "Register FUNCTION as the anonymous definer for SYMBOL.

FUNCTION receives the file path as its single argument and returns
the definition names it produces, in expansion order.  Registering
SYMBOL again replaces the previous entry.  Returns nil."
  (setq scalpel-locate-elisp--anonymous-definer-registry
        (assq-delete-all symbol scalpel-locate-elisp--anonymous-definer-registry))
  (push (cons symbol function) scalpel-locate-elisp--anonymous-definer-registry)
  nil)

(defun scalpel-locate-elisp--tree-member-predicate (form)
  "Return non-nil when FORM's tree has an anonymous definer call."
  (let (found)
    (cl-labels ((walk (node)
                  (cond
                   ((consp node)
                    (walk (car node))
                    (walk (cdr node)))
                   ((and (symbolp node)
                         (string-match-p
                          (rx (or "deftest" "definer") symbol-end)
                          (symbol-name node)))
                    (setq found t)))))
      (walk form)
      found)))

(defconst scalpel-locate-elisp-whole-block-container-heads
  '(when unless progn with-eval-after-load eval-after-load)
  "Guard-style container heads eligible for whole-block addressing in Elisp.
A container counts only when its tree holds any definer call
\(see `tree-holds-any-definer-call'), so a bare guard clause or a lone
check never contributes to the block count and is therefore never
miscounted as a whole-block target.")

(defun scalpel-locate-elisp-whole-block-container-names (file)
  "Return anonymous definer names for top-level containers in FILE.

A top-level form whose head is a member of
`scalpel-locate-elisp-whole-block-container-heads' and whose tree
holds at least one definer call (per
`scalpel-locate-elisp--tree-holds-any-definer-call', which skips
quoted data and hash tables) is counted under its head.  Each
eligible form yields a name \"<head>-N\" where N is the 1-based
ordinal of that head among eligible forms, in source order.  If FILE
is unreadable or a form fails to read, return the names collected so
far instead of raising (explicit degradation point: partial index is
preferred over losing the whole file)."
  (let ((counts (mapcar (lambda (h) (cons h 0))
                        scalpel-locate-elisp-whole-block-container-heads))
        (names '()))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (let ((form nil))
            (while (setq form (ignore-error end-of-file (read (current-buffer))))
              (when (and (consp form)
                         (symbolp (car form))
                         (assq (car form) counts)
                         (scalpel-locate-elisp--tree-holds-any-definer-call form))
                (let ((entry (assq (car form) counts)))
                  (setcdr entry (1+ (cdr entry)))
                  (push (format "%s-%d" (car entry) (cdr entry)) names))))))
      ;; Degradation: unreadable file or mid-file read error yields truncated list.
      ((file-error invalid-read-syntax invalid-read-function)
       (message "scalpel-locate: truncated container scan of %s: %s" file err)))
    (nreverse names)))

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
  "Resolve SYMBOL as a HEAD-N anonymous container index in FILE.
SYMBOL must take the HEAD-N form (e.g. `when-1`): HEAD has to be a
member of `scalpel-locate-elisp-whole-block-container-heads' and N
has to be non-zero.  Top level forms are replayed in the buffer in
the same synthesis order that produced `whole-block-container-names';
a form counts when its head equals HEAD and its tree contains a call
to one of the definer symbols in
`scalpel-locate-elisp--defining-forms'.  Returns \(BEG . END) of the
Nth counted form, or nil when HEAD/N fail the checks, reading hits an
error, or fewer than N forms match."
  (let ((parsed
         (and (stringp symbol)
              (string-match "\\`\\([a-z][a-z0-9-]*\\)-\\([1-9][0-9]*\\)\\'" symbol)
              (cons (intern (match-string 1 symbol))
                    (string-to-number (match-string 2 symbol))))))
    (when (and parsed
               (memq (car parsed) scalpel-locate-elisp-whole-block-container-heads)
               (> (cdr parsed) 0))
      (let ((head (car parsed))
            (index (cdr parsed)))
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (forward-comment (buffer-size))
              (catch 'done
                (let ((count 0))
                  (while t
                    (let* ((beg (point))
                           (form (condition-case nil
                                     (read (current-buffer))
                                   (error (throw 'done nil))))
                           (end (point)))
                      (when (and (consp form)
                                 (eq (car form) head)
                                 (cl-labels ((has-definer-call-p (tree)
                                              (cond
                                               ((atom tree) nil)
                                               ((and (symbolp (car tree))
                                                     (memq (car tree)
                                                           scalpel-locate-elisp--defining-forms))
                                                t)
                                               (t (or (has-definer-call-p (car tree))
                                                      (cl-some #'has-definer-call-p
                                                               (cdr tree)))))))
                                   (has-definer-call-p form)))
                        (setq count (1+ count))
                        (when (= count index)
                          (throw 'done (cons beg end)))))))))))))))

(defun scalpel-locate-elisp--tree-holds-definer-call (tree symbol)
  "Return t if TREE hold a defining call for SYMBOL, nil otherwise.

A defining call is a subform whose car is a member of
`scalpel-locate-elisp--defining-forms' and whose cadr is `equal'
to SYMBOL.  That is the same shape
`scalpel-locate-elisp--def-name-regex' matches at top level, so a
definition nested anywhere in TREE is judged by the same contract
as a top-level one.

The walk runs on the data produced by the Lisp reader, not on the
file text.  A text search for a definer spelling cannot tell code
from prose: the same word inside a docstring, a comment, or a
quoted literal would be misread as a nested definition.  Comments
never reach the reader's output, and docstrings and quoted
literals sit in the tree as inert objects -- quoted ones are cut
off by the `quote' branch -- so a data walk is immune to all
three."
  (cond
   ((not (consp tree)) nil)
   ((and (memq (car tree) scalpel-locate-elisp--defining-forms)
         (equal (cadr tree) symbol))
    t)
   ;; A definer spelling inside a quote is data, not structure.
   ((eq (car tree) 'quote) nil)
   (t (or (scalpel-locate-elisp--tree-holds-definer-call (car tree) symbol)
          (scalpel-locate-elisp--tree-holds-definer-call (cdr tree) symbol)))))

(defun scalpel-locate-elisp--tree-holds-any-definer-call (tree)
  "Non-nil when TREE has a subform whose car names one of the defining forms.
The walk stops at quoted data: inside a
quote the subforms are literal data, not code to be loaded, so a
`car' hit there would be a false positive.  Reader output (hash
tables, #s forms) is likewise skipped, because its contents never
come back as forms to evaluate and scanning them would attribute
definitions to positions the reader produced rather than the
source wrote."
  (when (consp tree)
    (or (and (memq (car tree) scalpel-locate-elisp--defining-forms) tree)
        (unless (memq (car tree) '(quote function))
          (or (scalpel-locate-elisp--tree-holds-any-definer-call (car tree))
              (scalpel-locate-elisp--tree-holds-any-definer-call (cdr tree)))))))

(defun scalpel-locate-elisp--nested-definition-block (symbol)
  "Report the block carrying a nested definition of SYMBOL.
SYMBOL is a definition name given as a string or a symbol; a
string is coerced with `intern'.  Read the current buffer's
top-level forms from `point-min' and return, for the first form
that is a cons whose car is a member of
`scalpel-locate-elisp--defining-forms', whose cadr differs from
SYMBOL, and whose cddr holds a defining call for SYMBOL per
`scalpel-locate-elisp--tree-holds-definer-call', the definer
keyword and the cadr joined by a single space, e.g. \"use-package
foo\", so callers can name the block an edit must target.  The
granularity contract of `scalpel-locate-elisp--defining-forms' is
unchanged, so a nested definition stays unlocatable and this
report only names where an edit must land.  Return nil when no
top-level form carries the name in its body.  The walk runs on
reader output, so comments, docstrings and quoted literals cannot
false-match, the same immunity
`scalpel-locate-elisp--tree-holds-definer-call' is written for."
  (when (stringp symbol)
    (setq symbol (intern symbol)))
  (save-excursion
    (goto-char (point-min))
    (catch 'done
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (member (car form)
                                 scalpel-locate-elisp--defining-forms)
                         (not (equal (cadr form) symbol))
                         (scalpel-locate-elisp--tree-holds-definer-call
                          (cddr form) symbol))
                (throw 'done
                       (format "%s %s" (car form) (cadr form))))))
        (end-of-file (throw 'done nil))
        (invalid-read-syntax (throw 'done nil))
        (scan-error (throw 'done nil))))))

(defun scalpel-locate-elisp--form-defines-p (form symbol)
  "Return non-nil if FORM contain a defining form for SYMBOL.
FORM is a form as produced by `read'; the search recurses over every
sub-list of the form tree and never looks at buffer text, so a name
spelled inside a comment or inside a string cannot match: the reader
drops comments, and the contents of a string are a string object,
not a symbol.  A bare call site does not count as a defining call
either, because a sub-list only matches when its head is a member of
`scalpel-locate-elisp--defining-forms' and its second element is a
symbol spelled exactly like SYMBOL.  This is the same spelling
contract the top-level scan applies.  Improper lists such as dotted
pairs are guarded, so `cadr' is never taken from a list too short to
have a second element."
  (cond
   ((atom form) nil)
   ((and (memq (car form) scalpel-locate-elisp--defining-forms)
         (consp (cdr form))
         (symbolp (cadr form))
         (equal (symbol-name (cadr form)) (symbol-name symbol)))
    t)
   (t (or (scalpel-locate-elisp--form-defines-p (car form) symbol)
          (scalpel-locate-elisp--form-defines-p (cdr form) symbol)))))

(defun scalpel-locate-elisp-range (file symbol)
  "Return byte range of SYMBOL definition as (BEG . END) in current buffer.
Current buffer is the Emacs Lisp file referenced by FILE.  A
symbol the literal search cannot find may still be generated by a
registered anonymous definer; that case is tried as a fallback
via the anonymous definer registry, after the HEAD-N anonymous
container lookup."
  (or (scalpel-locate-elisp--top-definition-range symbol)
      (scalpel-locate-elisp--top-definition-anonymous-range symbol file)
      (scalpel-locate-elisp-anonymous-definer-range file symbol)))

(defun scalpel-locate-elisp-anonymous-definer-range (file symbol)
  "Return the byte range of the definer-headed top-level form in FILE.
SYMBOL names the definition; return nil when there is none.
`scalpel-locate-elisp--anonymous-definer-registry' maps each definer
symbol to a locator function taking FILE and returning the names it
generates in order; the 1-based ordinal of SYMBOL within that list
selects which definer-headed top-level form of FILE is reported.
Symbols outside every registered name list yield nil, as does a FILE
whose definer-headed forms are fewer than the required ordinal."
  (catch 'found
    (dolist (entry scalpel-locate-elisp--anonymous-definer-registry)
      (let* ((definer (car entry))
             (names (condition-case nil
                        (funcall (cdr entry) file)
                      (error nil)))
             (k nil)
             (i 0))
        (dolist (name names)
          (setq i (1+ i))
          (when (and (null k) (string= name symbol))
            (setq k i)))
        (when k
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let ((seen 0))
              (while (and (< seen k) (not (eobp)))
                (forward-comment (point-max))
                (condition-case nil
                    (let ((beg (point))
                          (form (read (current-buffer))))
                      (when (eq (car-safe form) definer)
                        (setq seen (1+ seen))
                        (when (= seen k)
                          (throw 'found (cons beg (point))))))
                  (end-of-file nil)
                  (invalid-read-syntax nil))))))))))

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
                        (not (eobp)))
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
Whole-block container names (when-1 style, per
`scalpel-locate-elisp-whole-block-container-names') are merged in
as well.  Independently of the registry, a load-free source-level
fallback walks the top-level forms of the buffer one at a time:
any form whose head is a member of
`scalpel-locate-elisp--defining-forms' but whose second element is
a list rather than a symbol or string \(e.g. an `ert-gwt-deftest'
call like (ert-gwt-deftest (:given ...) ...) whose clauses follow
the head directly) carries no name in the usual place and counts
as an anonymous use.  For definers that have a registered
anonymous-definer function (assq hit in
`scalpel-locate-elisp--anonymous-definer-registry') the fallback
synthesizes nothing: the real hash names come from the registry
alone, avoiding duplicate placeholder names.  Only unregistered
definers get fallback synthesis, counted per definer in source
order and named \"<definer>-1\", \"<definer>-2\", ..., so even
never-loaded files still contribute a stable count.  FILE is the
path the caller names; anonymous-definer functions receive it
unchanged."
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
    ;; Whole-block container names merged in; a failure there
    ;; degrades to no container names rather than aborting the
    ;; listing (non-critical path, explicit degradation point).
    (let ((names (condition-case nil
                     (scalpel-locate-elisp-whole-block-container-names file)
                   (file-error nil)
                   (invalid-read-syntax nil))))
      (when names
        (dolist (name names)
          (push name syms))))
    ;; Source-level fallback needing no registration: walk the
    ;; top-level forms and synthesize placeholder names for
    ;; anonymous defining calls of unregistered definers only.
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
                             (consp (cadr form))
                             ;; Registered anonymous definers supply
                             ;; real names above; skip synthesis so
                             ;; placeholders never duplicate them.
                             (not (assq head
                                        scalpel-locate-elisp--anonymous-definer-registry)))
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
