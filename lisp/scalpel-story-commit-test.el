;;; scalpel-story-commit-test.el --- User-visible commit stories;; -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the commit-message capability:
;; I pick a commit style and the prompt I generate states that style's
;; rules, so the LLM writes a message in the shape I chose.  Every
;; clause is a single form; every THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-commit)

(defvar scalpel-story-commit-test-report nil
  "Holds a commit prompt captured inside a story.")

(ert-gwt-deftest
  (:given ((style 'angular))
          (setq scalpel-story-commit-test-report nil))
  (:when (setq scalpel-story-commit-test-report
               (scalpel-commit--style-prompt style)))
  (:then (should (stringp scalpel-story-commit-test-report)))
  (:then (should (> (length scalpel-story-commit-test-report) 0)))
  (:cleanup))

(ert-gwt-deftest
  (:given ((style 'linux))
          (setq scalpel-story-commit-test-report nil))
  (:when (setq scalpel-story-commit-test-report
               (scalpel-commit--style-prompt style)))
  (:then (should (stringp scalpel-story-commit-test-report)))
  (:then (should (> (length scalpel-story-commit-test-report) 0)))
  (:cleanup))

(provide 'scalpel-story-commit-test)

;;; scalpel-story-commit-test.el ends here
