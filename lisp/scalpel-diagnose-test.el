;;; scalpel-diagnose-test.el --- tests for scalpel-diagnose -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `scalpel-diagnose', the module that classifies a failed
;; round by who is at fault.  Each category gets one test: planner
;; types (the model's reply broke the contract) map to `planner',
;; environment types (a file outside the context, say) map to
;; `context', and anything unlisted falls to `executor', which is
;; Scalpel's own bug.  The advice tests pin the lookup order -- an
;; exact type entry wins, the category entry is the fallback -- and
;; that every listed type carries advice, so a console header never
;; prints a blank suggestion.  `scalpel-diagnose-planner-error-p' is
;; covered for both a plist and its bare type.

;;; Code:

(require 'ert)
(require 'scalpel-diagnose)

(ert-deftest scalpel-diagnose-test-planner-types-are-planner-category ()
  (dolist (type scalpel-diagnose-planner-types)
    (should (eq (scalpel-diagnose-category type) 'planner))))

(ert-deftest scalpel-diagnose-test-context-types-are-context-category ()
  (dolist (type scalpel-diagnose-context-types)
    (should (eq (scalpel-diagnose-category type) 'context))))

(ert-deftest scalpel-diagnose-test-unknown-type-is-executor-category ()
  (should (eq (scalpel-diagnose-category 'some-unknown-type) 'executor)))

(ert-deftest scalpel-diagnose-test-planner-error-p-covers-planner-types ()
  (should (scalpel-diagnose-planner-error-p (list :type 'prose)))
  (should-not (scalpel-diagnose-planner-error-p
               (list :type (car scalpel-diagnose-context-types)))))

(ert-deftest scalpel-diagnose-test-advice-for-covers-every-type ()
  (dolist (type (append scalpel-diagnose-planner-types
                        scalpel-diagnose-context-types))
    (should (stringp (scalpel-diagnose-advice-for type)))))

(ert-deftest scalpel-diagnose-test-advice-for-unknown-type-is-nil ()
  (should (stringp (scalpel-diagnose-advice-for 'some-unknown-type)))
  (should (equal (scalpel-diagnose-advice-for 'some-unknown-type)
                 (cdr (assq 'executor scalpel-diagnose-category-advice)))))

(ert-deftest scalpel-diagnose-test-advice-accepts-an-error-plist ()
  (should (equal (scalpel-diagnose-advice-for 'tool-call)
                 (cdr (assq 'tool-call scalpel-diagnose-advice))))
  (should (equal (scalpel-diagnose-advice (list :type 'malformed))
                 (scalpel-diagnose-advice-for 'malformed)))
  (should (equal (scalpel-diagnose-advice (list :type 'unknown))
                 (scalpel-diagnose-advice-for 'executor))))

(provide 'scalpel-diagnose-test)

;;; scalpel-diagnose-test.el ends here
