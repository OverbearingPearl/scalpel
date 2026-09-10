;;; scalpel-llm-test.el --- Tests for scalpel-llm -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-llm.

;;; Code:

(require 'ert)
(require 'scalpel-llm)

(ert-deftest scalpel-llm-test-api-key-error-p ()
  "Recognize gptel's missing-API-key setup error."
  (should (scalpel-llm--api-key-error-p "‘gptel-api-key’ is not valid"))
  (should-not (scalpel-llm--api-key-error-p "Scalpel: LLM request timed out")))

(ert-deftest scalpel-llm-test-fails-loudly-without-gptel ()
  "Requiring `scalpel-llm' must fail loudly when gptel is absent.
gptel is a hard dependency, so loading must signal `file-missing'
rather than silently deferring.  Runs a child Emacs with every
gptel directory removed from `load-path' and loads the source file
directly, so a stale byte-compiled copy cannot mask the result."
  (let* ((emacs (let ((path (and invocation-directory
                                 (expand-file-name invocation-name
                                                   invocation-directory))))
                  (or (and path (file-executable-p path) path)
                      (executable-find "emacs"))))
         (lib (locate-library "scalpel-llm"))
         (lisp-dir (and lib (file-name-directory lib))))
    (skip-unless (and emacs lisp-dir))
    (with-temp-buffer
      (let* ((code (format (concat "(progn"
                                   " (require 'cl-lib)"
                                   " (setq load-path (cl-remove-if"
                                   " (lambda (d) (string-match-p \"gptel\" d))"
                                   " load-path))"
                                   " (condition-case err"
                                   "     (progn (load-file"
                                   " (expand-file-name \"scalpel-llm.el\" %s))"
                                   " (princ \"SCALPEL-LLM-LOADED\"))"
                                   " (file-missing (princ \"SCALPEL-LLM-FAILED-LOUDLY\"))))")
                           (prin1-to-string lisp-dir)))
             (status (call-process emacs nil t nil
                                   "-Q" "-batch" "-L" lisp-dir
                                   "--eval" code))
             (output (buffer-string)))
        (unless (and (eq 0 status)
                     (string-match-p "SCALPEL-LLM-FAILED-LOUDLY" output)
                     (not (string-match-p "SCALPEL-LLM-LOADED" output)))
          (ert-fail
           (format (concat "scalpel-llm must fail with file-missing when "
                           "gptel is absent: status=%S output=%S")
                   status output)))))))

(provide 'scalpel-llm-test)

;;; scalpel-llm-test.el ends here
