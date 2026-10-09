;;; scalpel-story-diagnose-advice-test.el --- Advice stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the failure advice layer.  When a
;; round fails I get remedy text by exact type, an unknown type falls
;; to the executor entry so no failure is silent, the plist entry
;; point delegates by :type, and only the perl rewriting path carries
;; the perlre table of contents.  Every clause is a single form and
;; every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-diagnose-advice)

;; Story A: an exact table entry returns a non-empty remedy string.
(ert-gwt-deftest
  (:given ((advice nil)))
  (:when (progn
           (setq advice
                 (scalpel-diagnose-advice-for 'tool-call))))
  (:then (should (stringp advice)))
  (:then (should (not (string-empty-p advice)))))

;; Story B: an unknown type falls to the executor entry, never nil.
(ert-gwt-deftest
  (:given ((advice nil)))
  (:when (progn
           (setq advice
                 (scalpel-diagnose-advice-for
                  'no-such-advice-type))))
  (:then (should (stringp advice)))
  (:then (should (string-match-p "bug" advice))))

;; Story C: two different types do not share the same remedy text.
(ert-gwt-deftest
  (:given ((exact nil)
           (fallback nil)))
  (:when (progn
           (setq exact (scalpel-diagnose-advice-for 'tool-call)
                 fallback
                 (scalpel-diagnose-advice-for
                  'no-such-advice-type))))
  (:then (should (not (equal exact fallback)))))

;; Story D: the plist entry point extracts :type and delegates.
(ert-gwt-deftest
  (:given ((advice nil)))
  (:when (progn
           (setq advice
                 (scalpel-diagnose-advice-plist
                  (list :type 'parse)))))
  (:then (should (stringp advice)))
  (:then (should (equal advice
                        (scalpel-diagnose-advice-for 'parse)))))

;; Story E: a perl-path error carries the perlre table of contents.
(ert-gwt-deftest
  (:given ((toc nil)))
  (:when (progn
           (setq toc
                 (scalpel-diagnose-advice-perlre-toc-for
                  (list :type 'pattern-no-match
                        :message
                        "perl pattern matched nothing")))))
  (:then (should (stringp toc))))

;; Story F: a non-perl error gets no table of contents at all.
(ert-gwt-deftest
  (:given ((toc nil)))
  (:when (progn
           (setq toc
                 (scalpel-diagnose-advice-perlre-toc-for
                  (list :type 'prose
                        :message "nothing parseable here")))))
  (:then (should (null toc))))

(ert-gwt-deftest
  (:given ((first nil)
           (again nil)))
  (:when (progn
           (setq first (scalpel-diagnose-advice-perlre-toc))
           (setq again (scalpel-diagnose-advice-perlre-toc))))
  (:then (should (stringp first)))
  (:then (should (equal first again))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (setq result (scalpel-diagnose-advice-perlre-toc))))
  (:then (should (stringp result))))

(ert-gwt-deftest
  (:given ((repaired nil)))
  (:when (progn
           (setq repaired
                 (scalpel-diagnose-advice-mechanical-repair
                  (list :type 'tool-call
                        :input
                        "junk line\n[[action]]\ntool = 'reply'\n")))))
  (:then (should (stringp repaired)))
  (:then (should (string-match-p "\\[\\[action\\]\\]" repaired))))

;; Story J: a no-such-symbol error is repaired by swapping the name.
(ert-gwt-deftest
  (:given ((repaired nil)))
  (:when (progn
           (setq repaired
                 (scalpel-diagnose-advice-mechanical-repair
                  (list :type 'no-such-symbol
                        :message
                        "No such symbol: foo-bar, did you mean: foo-baz"
                        :input "(use foo-bar)")))))
  (:then (should (stringp repaired)))
  (:then (should (string-match-p "foo-baz" repaired))))

(provide 'scalpel-story-diagnose-advice-test)

;;; scalpel-story-diagnose-advice-test.el ends here
