;;; scalpel-story-locate-gitignore-test.el --- Gitignore locator stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the .gitignore locator: a user
;; points Scalpel at a .gitignore file and asks "what patterns are
;; here" and "where does this pattern line sit".  Comments and blank
;; lines are never symbols.  Every clause is a single form; every THEN
;; holds one 'should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-locate-gitignore)

(ert-gwt-deftest
  (:given ((content "node_modules/\n*.log\n# comment\n\nbuild/\n")
           (result nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-gitignore-list-symbols nil))))
  (:then (should (equal result
                        '("node_modules/" "*.log" "build/")))))

(ert-gwt-deftest
  (:given ((content "node_modules/\n*.log\nbuild/\n")
           (range nil)
           (text nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq range (scalpel-locate-gitignore-range nil "*.log"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal text "*.log\n"))))

(ert-gwt-deftest
  (:given ((full nil)
           (partial nil)))
  (:when (setq full (scalpel-locate-gitignore--single-definition-p
                     "*.log\n")
             partial (scalpel-locate-gitignore--single-definition-p
                      "*.log\nbuild/\n")))
  (:then (should (and full (not partial)))))

(ert-gwt-deftest
  (:given ((content "node_modules/\n*.log\n")
           (range nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq range (scalpel-locate-gitignore-range nil "missing"))))
  (:then (should (null range))))

(ert-gwt-deftest
  (:given ((content "# only a comment\n\n")
           (result nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-gitignore-list-symbols nil))))
  (:then (should (null result))))

(provide 'scalpel-story-locate-gitignore-test)

;;; scalpel-story-locate-gitignore-test.el ends here
