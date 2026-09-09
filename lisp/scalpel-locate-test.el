;;; scalpel-locate-test.el --- Tests for scalpel-locate -*- lexical-binding: t; -*-
;;; Commentary:

;; Tests for scalpel-locate.

;;; Code:

(require 'ert)
(require 'scalpel-locate)
(require 'scalpel-locate-elisp)

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
  (let ((file (make-temp-file "scalpel-locate-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun foo (x)\n  (+ x 1))\n"))
          (let ((range (scalpel-locate-range file "foo")))
            (should (= (car range) 1))
            (should (string=
                     (with-current-buffer (get-file-buffer file)
                       (buffer-substring-no-properties (car range) (cdr range)))
                     "(defun foo (x)\n  (+ x 1))"))))
      (when (get-file-buffer file)
        (with-current-buffer (get-file-buffer file)
          (set-buffer-modified-p nil))
        (kill-buffer (get-file-buffer file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest scalpel-locate-test-range-unregistered-file ()
  "Range request for an unregistered file type signals user-error."
  (let ((file (make-temp-file "scalpel-locate-test-" nil ".unknown")))
    (unwind-protect
        (should-error
         (scalpel-locate-range file "foo")
         :type 'user-error)
      (when (get-file-buffer file)
        (with-current-buffer (get-file-buffer file)
          (set-buffer-modified-p nil))
        (kill-buffer (get-file-buffer file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest scalpel-locate-test-list-symbols ()
  "List top-level definitions through the registered Elisp provider."
  (let ((file (make-temp-file "scalpel-locate-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun foo (x)\n  (+ x 1))\n(defmacro bar ()\n  nil)\n"))
          (let ((syms (scalpel-locate-list-symbols file)))
            (should (equal syms '("foo" "bar")))))
      (when (get-file-buffer file)
        (with-current-buffer (get-file-buffer file)
          (set-buffer-modified-p nil))
        (kill-buffer (get-file-buffer file)))
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'scalpel-locate-test)

;;; scalpel-locate-test.el ends here
