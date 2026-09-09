;;; scalpel-execute-test.el --- Tests for scalpel-execute -*- lexical-binding: t; -*-
;;; Commentary:

;; Tests for scalpel-execute.

;;; Code:

(require 'ert)
(require 'scalpel-execute)

(ert-deftest scalpel-execute-test-brackets-balanced-p ()
  "Balanced parens should be accepted, unbalanced rejected."
  (should (scalpel-execute--brackets-balanced-p "()"))
  (should-not (scalpel-execute--brackets-balanced-p "(")))

(ert-deftest scalpel-execute-test-replace ()
  "Replace a region with balanced text; reject unbalanced text."
  (with-temp-buffer
    (insert "(defun foo (x)\n  (+ x 1))\n")
    (let ((beg (point-min))
          (end (progn (goto-char (point-min))
                      (line-end-position 1))))
      (scalpel-execute-replace beg end "(defun foo (x)\n  (+ x 2))")
      (should (string= (buffer-string)
                       "(defun foo (x)\n  (+ x 2))\n  (+ x 1))\n")))
    (should-error
     (let ((beg (point-min))
           (end (progn (goto-char (point-min))
                       (line-end-position 1))))
       (scalpel-execute-replace beg end "(defun foo (x"))
     :type 'error)))

(provide 'scalpel-execute-test)

;;; scalpel-execute-test.el ends here
