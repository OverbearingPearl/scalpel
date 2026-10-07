;;; scalpel-story-prompt-rule-test.el --- Prompt rule stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the named prompt rules.  When I
;; ask for a language rule in a language I get one sentence naming
;; that language; when I ask without a language I get nothing, so the
;; prompt assembles cleanly without the rule.  Every clause is a
;; single form and every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-prompt-rule)

(ert-gwt-deftest
  (:given ((rule nil)))
  (:when (progn
           (setq rule
                 (scalpel-prompt-rule--reply-language-rule
                  "Chinese"))))
  (:then (should (equal rule
                        "The text of reply actions must be written in Chinese."))))

(ert-gwt-deftest
  (:given ((rule nil)))
  (:when (progn
           (setq rule
                 (scalpel-prompt-rule--reply-language-rule nil))))
  (:then (should (null rule))))

(ert-gwt-deftest
  (:given ((rule nil)))
  (:when (progn
           (setq rule
                 (scalpel-prompt-rule--shell-action-reason-language-rule
                  "Chinese"))))
  (:then (should (equal rule
                        "The reason field of every shell action must be written in Chinese."))))

(ert-gwt-deftest
  (:given ((rule nil)))
  (:when (progn
           (setq rule
                 (scalpel-prompt-rule--compress-reason-language-rule
                  "Chinese"))))
  (:then (should (equal rule
                        "The compressed history summary must be written in Chinese."))))

(ert-gwt-deftest
  (:given ((rule nil)))
  (:when (progn
           (setq rule
                 (scalpel-prompt-rule--thinking-language-rule
                  "Chinese"))))
  (:then (should (equal rule
                        "Your private thinking and reasoning must be written in Chinese."))))

(ert-gwt-deftest
  (:given ((rule nil)))
  (:when (progn
           (setq rule
                 (scalpel-prompt-rule--thinking-language-rule nil))))
  (:then (should (null rule))))

(provide 'scalpel-story-prompt-rule-test)

;;; scalpel-story-prompt-rule-test.el ends here
