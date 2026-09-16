;;; scalpel-locate-test.el --- Tests for scalpel-locate -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-locate.

;;; Code:

(require 'ert)
(require 'scalpel-locate)
(require 'scalpel-locate-elisp)
(require 'scalpel-utils-test)

(ert-deftest scalpel-locate-test-elisp-top-definition-range ()
  "Locate the byte range of a top-level defun in an Emacs Lisp buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo (x)\n  (+ x 1))\n(defun bar ())\n")
    (let ((range (scalpel-locate-elisp--top-definition-range "foo")))
      (should (= (car range) 1))
      (should (= (- (cdr range) (car range))
                 (length "(defun foo (x)\n  (+ x 1))"))))))

(ert-deftest scalpel-locate-test-elisp-top-definition-range-not-found ()
  "When the symbol is absent, return nil (public locate-range signals)."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo (x)\n  (+ x 1))\n")
    (should (null (scalpel-locate-elisp--top-definition-range "bar")))))

(ert-deftest scalpel-locate-test-range-dispatched-by-file ()
  "Public range API opens an Elisp file and resolves a top-level symbol."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n"))
    (let ((range (scalpel-locate-range this-file "foo")))
      (should (= (car range) 1))
      (should (string=
               (with-current-buffer (get-file-buffer this-file)
                 (buffer-substring-no-properties (car range) (cdr range)))
               "(defun foo (x)\n  (+ x 1))")))))

(ert-deftest scalpel-locate-test-range-unregistered-file ()
  "Range request for an unregistered file type signals user-error."
  (scalpel-utils-test-with-temp-file ".unknown"
    (should-error
     (scalpel-locate-range this-file "foo")
     :type 'user-error)))

(ert-deftest scalpel-locate-test-list-symbols ()
  "List top-level definitions through the registered Elisp provider."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n(defmacro bar ()\n  nil)\n"))
    (let ((syms (scalpel-locate-list-symbols this-file)))
      (should (equal syms '("foo" "bar"))))))

(ert-deftest scalpel-locate-test-range-sees-external-modification ()
  "A locate on a stale visiting buffer still sees the disk.
Regression: `scalpel-locate-range' reused an existing buffer via
`find-file-noselect' without checking the disk, so a file changed
behind Emacs reported a definition present on disk as missing."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun old ())\n"))
    (find-file-noselect this-file)
    (with-temp-file this-file (insert "(defun fresh ())\n"))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (let ((range (scalpel-locate-range this-file "fresh")))
        (ert-info ((format "Range: %S" range))
          (should range))))))

(ert-deftest scalpel-locate-test-sync-preserves-unsaved-edits ()
  "A buffer with unsaved edits is never reverted by a locate."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun old ())\n"))
    (let ((buf (find-file-noselect this-file)))
      ;; Reverting or discarding a stale buffer must never block on a
      ;; question the suite cannot answer.
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (unwind-protect
            (progn
              (with-current-buffer buf (insert "(defun draft ())\n"))
              (with-temp-file this-file (insert "(defun fresh ())\n"))
              (let ((range (scalpel-locate-range this-file "old")))
                (ert-info ((format "Range: %S" range))
                  ;; The unsaved buffer text is what locate works on: the
                  ;; user's in-flight edit wins over the disk.
                  (should range))))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))))))

(ert-deftest scalpel-locate-test-elisp-locates-ert-deftest ()
  "An `ert-deftest' is located like any other top-level definition.
Regression: the defining-form whitelist held only the core Elisp
definers, so a symbol inside a test file -- the file an agent round
most often edits -- reported \"not found\" even though the deftest
sat in the file; the same gap covered `cl-defun' and friends.
The provider functions work on the current buffer, so the content
is inserted here directly: calling them with a file path outside
the dispatch layer would search the caller's own buffer."
  (with-temp-buffer
    (insert "(require 'ert)\n\n"
            "(ert-deftest llm-pick-align-test-a-gap-is-not-ambiguous ()\n"
            "  (should t))\n\n"
            "(cl-defun cl-pick--helper (&key x)\n  x)\n")
    (ert-info ((format "Symbols: %S"
                       (scalpel-locate-elisp-list-symbols nil)))
      (should (member "llm-pick-align-test-a-gap-is-not-ambiguous"
                      (scalpel-locate-elisp-list-symbols nil)))
      (should (member "cl-pick--helper"
                      (scalpel-locate-elisp-list-symbols nil))))
    (should (scalpel-locate-elisp--top-definition-range
             "llm-pick-align-test-a-gap-is-not-ambiguous"))
    (should (scalpel-locate-elisp--top-definition-range
             "cl-pick--helper"))))

(ert-deftest scalpel-locate-test-elisp-locates-a-package-definer ()
  "A name defined by a mode or package macro is listed and located.
Regression: the list held only the core definers and their cl-lib
and ERT aliases, so `llm-pick-view-mode' -- present in the file at
its own `define-derived-mode' form -- was reported \"not found in ...
nor anywhere in the context\", and every round naming it died
before it could be edited; `llm-pick-menu' at its
`transient-define-prefix' form failed the same way.  The reader and
the locator share one list, so both have to know the spelling the
file was written with; the two forms below are those shapes, name
unquoted, which is the membership rule the list documents."
  (with-temp-buffer
    (insert "(define-derived-mode llm-pick-view-mode special-mode"
            " \"llm-pick-view\")\n\n"
            "(transient-define-prefix llm-pick-menu ()\n"
            "  [\"Actions\" (\"q\" \"quit\" llm-pick-quit)])\n")
    (ert-info ((format "Symbols: %S"
                       (scalpel-locate-elisp-list-symbols nil)))
      (should (member "llm-pick-view-mode"
                      (scalpel-locate-elisp-list-symbols nil)))
      (should (member "llm-pick-menu"
                      (scalpel-locate-elisp-list-symbols nil))))
    (let ((range (scalpel-locate-elisp--top-definition-range
                  "llm-pick-view-mode")))
      (ert-info ((format "Range: %S" range))
        (should range)
        (should (string-match-p
                 "\\`(define-derived-mode llm-pick-view-mode"
                 (buffer-substring-no-properties (car range) (cdr range))))))
    (let ((range (scalpel-locate-elisp--top-definition-range
                  "llm-pick-menu")))
      (ert-info ((format "Range: %S" range))
        (should range)
        (should (string-match-p
                 "\\`(transient-define-prefix llm-pick-menu"
                 (buffer-substring-no-properties (car range) (cdr range))))))
    ;; The same list backs the replacement check, so a round that
    ;; hands such a definition back must be accepted as a whole block;
    ;; otherwise the definition could be located but never edited.
    (should (scalpel-locate-elisp--single-definition-p
             (concat "(define-derived-mode llm-pick-view-mode special-mode"
                     " \"llm-pick-view\")")))))

(ert-deftest scalpel-locate-test-elisp-rejects-quoted-name-definers ()
  "A definer that takes a quoted name stays out of the list.
Regression risk: adding `defalias', `defvaralias' or `define-error'
looks like widening the vocabulary, but their names are read as
`(quote name)', so `list-symbols' would report `'foo' -- a spelling
the locator, which searches for the bare word, can never match --
and a name that used to be found by another form would be reported
with a quote instead.  The exclusion is the same rule that admits
the mode and transient spellings."
  (dolist (definer '(defalias defvaralias define-error define-widget))
    (ert-info ((format "Definer: %S" definer))
      (should-not (memq definer scalpel-locate-elisp--defining-forms)))))

(ert-deftest scalpel-locate-test-elisp-single-definition-p ()
  "One or more complete top-level defining forms are accepted."
  (should (scalpel-locate-elisp--single-definition-p
           "(defun foo (x) (+ x 1))"))
  (should (scalpel-locate-elisp--single-definition-p
           "(ert-deftest foo-test () (should t))"))
  (should (scalpel-locate-elisp--single-definition-p
           "(cl-defun foo (&key x) x)"))
  (should (scalpel-locate-elisp--single-definition-p
           "(defvar scalpel-execute-providers nil\n  \"Providers.\")\n(defun bar ())"))
  (should-not (scalpel-locate-elisp--single-definition-p
               "(message \"hello\")"))
  (should-not (scalpel-locate-elisp--single-definition-p
               "(defun foo (x) (+ x 1))\n(message \"hello\")"))
  (should-not (scalpel-locate-elisp--single-definition-p
               "(defun foo (x) (+ x 1"))
  (should-not (scalpel-locate-elisp--single-definition-p
               "There is nothing to change here.")))

(ert-deftest scalpel-locate-test-single-definition-p-dispatches ()
  "The public predicate dispatches by file type and rejects unknown types."
  (scalpel-utils-test-with-temp-file ".el"
    (should (scalpel-locate-single-definition-p this-file "(defvar x 1)")))
  (scalpel-utils-test-with-temp-file ".unknown"
    (should-error (scalpel-locate-single-definition-p this-file "(defvar x 1)")
                  :type 'user-error)))

(ert-deftest scalpel-locate-test-elisp-single-form-p ()
  "Exactly one complete top-level form is accepted, definition or not.
The form need not define anything, which is the whole reason the
predicate exists: a registration call is a top-level unit of an
Emacs Lisp file."
  (should (scalpel-locate-elisp--single-form-p "(defun foo (x) (+ x 1))"))
  (should (scalpel-locate-elisp--single-form-p
           (concat "(llm-pick-source-register 'artificial-analysis\n"
                   "  :kind 'capability\n"
                   "  :fetcher #'loader)")))
  ;; Trailing whitespace is not a second form.
  (should (scalpel-locate-elisp--single-form-p "(defun foo ())\n"))
  ;; Two forms are refused: a reply that echoes a whole file holds
  ;; several, and accepting one would append the file to itself.
  (should-not (scalpel-locate-elisp--single-form-p
               "(defun foo ())\n(defun bar ())"))
  ;; A bare word reads as a symbol, so prose would otherwise be
  ;; inserted as a form.
  (should-not (scalpel-locate-elisp--single-form-p "No change needed."))
  (should-not (scalpel-locate-elisp--single-form-p "(defun foo (x)"))
  (should-not (scalpel-locate-elisp--single-form-p "")))

(ert-deftest scalpel-locate-test-single-form-p-dispatches ()
  "The public form predicate dispatches by file type and rejects unknown types."
  (scalpel-utils-test-with-temp-file ".el"
    (should (scalpel-locate-single-form-p this-file "(defun foo ())"))
    (should (scalpel-locate-single-form-p
             this-file "(scalpel-llm-dialect-register \"x\" 'y)")))
  (scalpel-utils-test-with-temp-file ".unknown"
    (should-error (scalpel-locate-single-form-p this-file "(defun foo ())")
                  :type 'user-error)))

(ert-deftest scalpel-locate-test-single-form-p-falls-back-to-definitions ()
  "A provider without a form validator reuses its definition validator.
A language whose top-level units are its definitions -- every
provider but Emacs Lisp -- has no separate notion of a form, so the
two checks are the same one there.  A provider registering neither is
still refused: the fallback is a stated equivalence, not a silent
pass."
  (let ((scalpel-locate-providers
         (cons (cons "\\.formless\\'"
                     (list :single-definition-p
                           (lambda (text) (string= text "ok"))))
               scalpel-locate-providers)))
    (should (scalpel-locate-single-form-p "/tmp/a.formless" "ok"))
    (should-not (scalpel-locate-single-form-p "/tmp/a.formless" "no")))
  (let ((scalpel-locate-providers
         (cons (cons "\\.bare\\'" (list :locate #'ignore))
               scalpel-locate-providers)))
    (should-error (scalpel-locate-single-form-p "/tmp/a.bare" "ok")
                  :type 'user-error)))

(ert-deftest scalpel-locate-test-elisp-form-count ()
  "The reader's count separates several forms from a text it cannot finish.
A refusal is explained with this count: a text holding several forms
and one no reader reaches the end of get different advice, and a
boolean cannot tell them apart."
  (should (= 1 (scalpel-locate-elisp--form-count
                "(defun foo (x) (+ x 1))")))
  (should (= 2 (scalpel-locate-elisp--form-count
                "(defun a ())\n(defun b ())")))
  ;; A registration call is a form like any other.
  (should (= 1 (scalpel-locate-elisp--form-count
                "(scalpel-llm-dialect-register \"x\" 'y)")))
  ;; Layout between forms is not a form.
  (should (= 1 (scalpel-locate-elisp--form-count "(defun foo ())\n")))
  ;; A text the reader cannot finish has no count to give.
  (should-not (scalpel-locate-elisp--form-count "(defun foo (x)"))
  (should-not (scalpel-locate-elisp--form-count "\"unterminated"))
  ;; Empty is a count of zero, not a failure to count.
  (should (= 0 (scalpel-locate-elisp--form-count "")))
  (should (= 0 (scalpel-locate-elisp--form-count "  \n"))))

(ert-deftest scalpel-locate-test-elisp-balanced-p ()
  "A bracket inside a docstring or a comment is text, not structure."
  (should (scalpel-locate-elisp--balanced-p "(defun a ())"))
  (should (scalpel-locate-elisp--balanced-p
           "(defun a ()\n  \"See ()\")\n"))
  (should (scalpel-locate-elisp--balanced-p
           "(defun a ()\n  ;; a lone ( here\n  nil)\n"))
  (should-not (scalpel-locate-elisp--balanced-p "(defun a ("))
  (should-not (scalpel-locate-elisp--balanced-p "a)\n")))

(ert-deftest scalpel-locate-test-form-count-and-balanced-p-dispatch ()
  "Both diagnostics dispatch by file type and stay silent when unsure.
A provider registering neither answers nil for the count and t for
the brackets: an unexplained refusal must not be handed a cause about
shape, and \"no answer\" is not a cause.  Emacs Lisp registers both,
so a text its reader cannot finish is nilled by the count and refused
by the bracket answer."
  (scalpel-utils-test-with-temp-file ".el"
    (should (= 2 (scalpel-locate-form-count
                  this-file "(defun a ())\n(defun b ())")))
    (should-not (scalpel-locate-form-count this-file "(defun a ("))
    (should (scalpel-locate-balanced-p this-file "(defun a ())"))
    (should-not (scalpel-locate-balanced-p this-file "(defun a (")))
  (scalpel-utils-test-with-temp-file ".md"
    (should-not (scalpel-locate-form-count this-file "# A\n# B\n"))
    (should (scalpel-locate-balanced-p this-file "# A\n# B\n")))
  (scalpel-utils-test-with-temp-file ".unknown"
    (should-not (scalpel-locate-form-count this-file "anything"))
    (should (scalpel-locate-balanced-p this-file "anything"))))

(provide 'scalpel-locate-test)

;;; scalpel-locate-test.el ends here
