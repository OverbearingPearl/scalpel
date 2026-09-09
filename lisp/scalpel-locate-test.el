;;; scalpel-locate-test.el --- Tests for scalpel-locate -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'scalpel-locate)

(ert-deftest scalpel-locate-test-top-definition-range ()
  "Locate the byte range of a top-level defun in an Emacs Lisp buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo (x)\n  (+ x 1))\n(defun bar ())\n")
    (let ((range (save-excursion
                   (goto-char (point-min))
                   (scalpel-locate--top-definition-range "foo"))))
      (should (= (car range) 1))
      (should (= (- (cdr range) (car range))
                 (length "(defun foo (x)\n  (+ x 1))"))))))

(ert-deftest scalpel-locate-test-top-definition-range-not-found ()
  "When the symbol is absent, return nil (public locate-range signals)."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo (x)\n  (+ x 1))\n")
    (should (null (scalpel-locate--top-definition-range "bar")))))

(provide 'scalpel-locate-test)
;;; scalpel-locate-test.el ends here
