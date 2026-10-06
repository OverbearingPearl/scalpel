;;; scalpel-story-sandbox-test.el --- User-visible sandbox stories;; -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the sandbox capability: the shell
;; I ask the agent to run executes inside the sandbox and its output
;; comes back with an explicit exit status.  Every clause is a single
;; form; every THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-sandbox)

;;; scalpel-story-sandbox-test.el --- User-visible sandbox stories -*- lexical-binding: t; -*-

(defvar scalpel-story-sandbox-test-result nil
  "Holds a sandbox run result captured inside a story.")

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-agent--context-files nil)
           (scalpel-story-sandbox-test-result nil))
          (should (scalpel-sandbox-supported-p)))
  (:when (setq scalpel-story-sandbox-test-result
               (scalpel-sandbox-run "echo hello" root (list (progn (write-region "x\n" nil (expand-file-name "ctx.txt" root)) (expand-file-name "ctx.txt" root))))))
  (:then (should (equal (car scalpel-story-sandbox-test-result) 0)))
  (:then (should (string-search
                  "hello" (cdr scalpel-story-sandbox-test-result))))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(provide 'scalpel-story-sandbox-test)

;;; scalpel-story-sandbox-test.el ends here
