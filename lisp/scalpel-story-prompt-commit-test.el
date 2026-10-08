;;; scalpel-story-prompt-commit-test.el --- Commit prompt stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for commit-message prompt building.  I
;; ask for an angular style and read type-prefix rules; I ask for the
;; Linux style and read imperative-summary rules without type tags; an
;; unknown style yields no style rules; and a built prompt carries the
;; subject and width limits, the language, the rejection instruction
;; and the diff.  Every clause is a single form and every THEN one
;; `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-prompt-commit)

(ert-gwt-deftest
  (:given ((prompt (scalpel-prompt-commit-style-prompt 'angular))))
  (:when (progn prompt))
  (:then (should (and (string-search "type(scope):" prompt)
                      (string-search "BREAKING CHANGE:" prompt))))
  (:cleanup nil))

(ert-gwt-deftest
  (:given ((prompt (scalpel-prompt-commit-style-prompt 'linux))))
  (:when (progn prompt))
  (:then (should (and (string-search "no type prefix" prompt)
                      (string-search "present-tense imperative" prompt))))
  (:cleanup nil))

(ert-gwt-deftest
  (:given ((prompt (scalpel-prompt-commit-style-prompt 'mystery))))
  (:when (progn prompt))
  (:then (should (equal prompt "")))
  (:cleanup nil))

(ert-gwt-deftest
  (:given ((prompt (scalpel-prompt-commit-build-prompt
                    "diff body" 'angular "Chinese" nil 50 72))))
  (:when (progn prompt))
  (:then (should (and (string-search "No more than 50" prompt)
                      (string-search "wrapped to 72" prompt)
                      (string-search "in Chinese" prompt)
                      (string-search "DIFF:\ndiff body" prompt))))
  (:cleanup nil))

(ert-gwt-deftest
  (:given ((prompt (scalpel-prompt-commit-build-prompt
                    "diff body" 'linux "English"
                    "asked for more detail" 50 72))))
  (:when (progn prompt))
  (:then (should (and (string-search
                       "rejected because: asked for more detail" prompt)
                      (string-search "Produce a different message" prompt))))
  (:cleanup nil))

(provide 'scalpel-story-prompt-commit-test)

;;; scalpel-story-prompt-commit-test.el ends here
