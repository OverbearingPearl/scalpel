;;; scalpel-story-shell-test.el --- User-visible shell stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the shell capability: I ask the
;; agent to run a command in my project, I read a bounded report with
;; an explicit exit status and fenced output; a forbidden tool is
;; refused with a remedy instead of run.  The sandbox boundary is
;; stubbed inside the :when clause itself, so the stub is alive for
;; the one action under test.  Every clause is a single form; every
;; THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-agent)

(defvar scalpel-story-shell-test-report nil
  "Holds a shell report captured inside a story.")

(defvar scalpel-story-shell-test-refusal nil
  "Holds a shell refusal captured inside a story.")

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-agent--context-files nil))
          (setq default-directory root)
          (setq scalpel-story-shell-test-report nil))
  (:when (cl-letf (((symbol-function 'scalpel-sandbox-run)
                    (lambda (command _root _files)
                      (cons 0 (shell-command-to-string command)))))
           (setq scalpel-story-shell-test-report
                 (scalpel-agent-shell "echo hello" "show the greeting"))))
  (:then (should (string-search "Shell: echo hello"
                                scalpel-story-shell-test-report)))
  (:then (should (string-search "Exit: 0"
                                scalpel-story-shell-test-report)))
  (:then (should (string-search "hello"
                                scalpel-story-shell-test-report)))
  (:cleanup (delete-directory root t)
            (setq default-directory (file-name-directory (locate-library "scalpel-test"))
                  scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-agent--context-files nil))
          (setq default-directory root)
          (setq scalpel-story-shell-test-refusal nil))
  (:when (condition-case err
             (scalpel-agent-shell "sed s/a/b/ f.el" "transform text")
           (user-error
            (setq scalpel-story-shell-test-refusal
                  (error-message-string err)))))
  (:then (should (string-search "may not invoke sed"
                                scalpel-story-shell-test-refusal)))
  (:then (should (string-search "Perl 5"
                                scalpel-story-shell-test-refusal)))
  (:cleanup (delete-directory root t)
            (setq default-directory (file-name-directory (locate-library "scalpel-test"))
                  scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-agent--context-files nil))
          (setq default-directory root)
          (setq scalpel-story-shell-test-report nil))
  (:when (cl-letf (((symbol-function 'scalpel-sandbox-run)
                    (lambda (_command _root _files)
                      (cons 0 "ok\0hidden"))))
           (setq scalpel-story-shell-test-report
                 (scalpel-agent-shell "cat blob" "read the blob"))))
  (:then (should (string-search "binary output suppressed"
                                scalpel-story-shell-test-report)))
  (:then (should (not (string-search "hidden"
                                     scalpel-story-shell-test-report))))
  (:cleanup (delete-directory root t)
            (setq default-directory (file-name-directory (locate-library "scalpel-test"))
                  scalpel-agent--context-files nil)))

(provide 'scalpel-story-shell-test)

;;; scalpel-story-shell-test.el ends here
