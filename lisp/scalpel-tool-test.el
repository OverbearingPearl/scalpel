;;; scalpel-tool-test.el --- Tool selection stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the tool table: an absent category
;; has no entry and no argv, an entry's argv is read straight from the
;; table, a prompt rule is returned only when its executable is
;; installed, and the concatenated rules skip tools without one.
;; Entries are built with `list' so plist values stay true symbols
;; instead of (quote ...) forms, and the no-rule story uses an
;; executable name that cannot exist.  The preference table is bound
;; per story and restored in cleanup.  Every clause is a single form
;; and every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-tool)

(ert-gwt-deftest
  (:given ((scalpel-tool--preferences nil)
           (_ (push (list 'search :argv (list "rg"))
                    scalpel-tool--preferences))
           (result 'unset)))
  (:when (setq result (list (scalpel-tool--entry 'substitute)
                            (scalpel-tool--argv 'search))))
  (:then (should (equal result (list nil (list "rg")))))
  (:cleanup (setq scalpel-tool--preferences nil)))

(ert-gwt-deftest
  (:given ((scalpel-tool--preferences nil)
           (_ (push (list 'search
                          :argv (list "scalpel-story-no-such-exec"))
                    scalpel-tool--preferences))
           (result 'unset)))
  (:when (setq result (scalpel-tool--prompt-rule 'search)))
  (:then (should (not result)))
  (:cleanup (setq scalpel-tool--preferences nil)))

(ert-gwt-deftest
  (:given ((scalpel-tool--preferences nil)
           (_ (defconst scalpel-story-tool-test-rule "rule text"))
           (_ (push (list 'search
                          :argv (list "emacs")
                          :prompt-rule 'scalpel-story-tool-test-rule)
                    scalpel-tool--preferences))
           (result 'unset)))
  (:when (setq result (scalpel-tool--prompt-rule 'search)))
  (:then (should (string= result "rule text")))
  (:cleanup (setq scalpel-tool--preferences nil)
            (makunbound 'scalpel-story-tool-test-rule)))

(ert-gwt-deftest
  (:given ((scalpel-tool--preferences nil)
           (_ (defconst scalpel-story-tool-test-rule-a "a rule"))
           (_ (push (list 'a
                          :argv (list "emacs")
                          :prompt-rule 'scalpel-story-tool-test-rule-a)
                    scalpel-tool--preferences))
           (_ (push (list 'b :argv (list "rg"))
                    scalpel-tool--preferences))
           (result 'unset)))
  (:when (setq result (scalpel-tool--prompt-rules)))
  (:then (should (string= result "a rule")))
  (:cleanup (setq scalpel-tool--preferences nil)
            (makunbound 'scalpel-story-tool-test-rule-a)))

(provide 'scalpel-tool-test)

;;; scalpel-tool-test.el ends here
