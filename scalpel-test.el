;;; scalpel-test.el --- Test entry for Scalpel -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

(defun scalpel-test-run ()
  "Reload Scalpel modules, then run every Scalpel ERT test."
  (interactive)
  (ert-delete-all-tests)
  (dolist (feat '(scalpel scalpel-locate scalpel-execute scalpel-agent
                 scalpel-llm scalpel-console
                 scalpel-locate-test scalpel-execute-test
                 scalpel-llm-test scalpel-agent-test scalpel-console-test))
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
  (if noninteractive
      (ert-run-tests-batch-and-exit "scalpel-")
    (ert "scalpel-")))

(provide 'scalpel-test)
;;; scalpel-test.el ends here
