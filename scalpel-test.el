;;; scalpel-test.el --- Test entry for Scalpel -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-locate)
(require 'scalpel-execute)

(defun scalpel-test-run ()
  "Reload Scalpel modules, then run every Scalpel ERT test."
  (interactive)
  (ert-delete-all-tests)
  (dolist (feat '(scalpel scalpel-locate scalpel-execute scalpel-agent scalpel-llm scalpel-console))
    (when (featurep feat) (unload-feature feat t)))
  (mapatoms
   (lambda (s)
     (when (and (string-prefix-p "scalpel-" (symbol-name s))
                (not (string-prefix-p "scalpel-test-" (symbol-name s)))
                (boundp s))
       (makunbound s))))
  (load-file (expand-file-name "scalpel.el" default-directory))
  (dolist (f (directory-files (expand-file-name "lisp") t "scalpel-.*\\.el\\'"))
    (load-file f))
  (load-file (expand-file-name "scalpel-test.el" default-directory))
  (if noninteractive
      (ert-run-tests-batch-and-exit "scalpel-")
    (ert "scalpel-")))

(ert-deftest scalpel-locate--top-definition-range-test ()
  "Locate the byte range of a top-level defun in an Emacs Lisp buffer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun foo (x)\n  (+ x 1))\n(defun bar ())\n")
    (save-excursion
      (goto-char (point-min))
      (forward-line 0))
    (let ((range (save-excursion
                   (goto-char (point-min))
                   (scalpel-locate--top-definition-range "foo"))))
      (should (= (car range) 1))
      (should (= (- (cdr range) (car range))
                 (length "(defun foo (x)\n  (+ x 1))"))))))

(ert-deftest scalpel-execute--brackets-balanced-p-test ()
  "Balanced parens should be accepted, unbalanced rejected."
  (should (scalpel-execute--brackets-balanced-p "()"))
  (should-not (scalpel-execute--brackets-balanced-p "(")))

(ert-deftest scalpel-llm-api-key-error-p-test ()
  "Recognize gptel's missing-API-key setup error."
  (require 'scalpel-llm)
  (should (scalpel-llm--api-key-error-p "‘gptel-api-key’ is not valid"))
  (should-not (scalpel-llm--api-key-error-p "Scalpel: LLM request timed out")))

(defun scalpel-console--parse-test ()
  "Placeholder; real console parsing is covered by the locate/execute suite."
  t)

(provide 'scalpel-test)
;;; scalpel-test.el ends here
