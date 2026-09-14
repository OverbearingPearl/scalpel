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

(ert-deftest scalpel-locate-test-elisp-single-definition-p ()
  "One or more complete top-level defining forms are accepted."
  (should (scalpel-locate-elisp--single-definition-p
           "(defun foo (x) (+ x 1))"))
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

(provide 'scalpel-locate-test)

;;; scalpel-locate-test.el ends here
