;;; scalpel-story-prompt-svg-test.el --- SVG prompt rule stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the SVG prompt rule.  When I
;; assemble the system prompt the SVG rule is present, because the
;; plain backend needs no external program; and the rule text itself
;; names the backend order and forbids quoting drawio flags from
;; memory.  Every clause is a single form and every THEN one
;; `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-prompt)
(require 'scalpel-prompt-rule)

(ert-gwt-deftest
  (:given ((prompt nil)))
  (:when (progn
           (setq prompt (scalpel-prompt-system-prompt))))
  (:then (should (string-search
                  "scalpel-prompt-rule--svg"
                  (symbol-name 'scalpel-prompt-rule--svg))))
  (:then (should (string-search
                  (substring scalpel-prompt-rule--svg 0 40)
                  prompt))))

(ert-gwt-deftest
  (:given ((rule nil)))
  (:when (progn (setq rule scalpel-prompt-rule--svg)))
  (:then (should (string-search "drawio" rule)))
  (:then (should (string-search "plain" rule)))
  (:then (should (string-search "scalpel-svg-render" rule)))
  (:then (should (string-search "never quote CLI flags from memory" rule))))

(provide 'scalpel-story-prompt-svg-test)

;;; scalpel-story-prompt-svg-test.el ends here
