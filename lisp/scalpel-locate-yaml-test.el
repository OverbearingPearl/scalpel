;;; scalpel-locate-yaml-test.el --- Tests for scalpel-locate-yaml -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the YAML locator provider.

;;; Code:

(require 'ert)
(require 'scalpel-locate-yaml)

(ert-deftest scalpel-locate-yaml-test-top-definition-range ()
  "A top-level key's block spans to the next top-level line."
  (with-temp-buffer
    (insert "alpha:\n  a: 1\n  b: 2\nbeta:\n  c: 3\n")
    (let ((range (scalpel-locate-yaml--top-definition-range "alpha")))
      (ert-info ((format "Range: %S" range))
        (should range)
        (should (string=
                 (buffer-substring-no-properties (car range) (cdr range))
                 "alpha:\n  a: 1\n  b: 2"))))))

(ert-deftest scalpel-locate-yaml-test-range-excludes-trailing-blanks ()
  "Blank lines before the next block are not part of the range."
  (with-temp-buffer
    (insert "alpha:\n  a: 1\n\n\nbeta:\n")
    (let ((range (scalpel-locate-yaml--top-definition-range "alpha")))
      (should (string=
               (buffer-substring-no-properties (car range) (cdr range))
               "alpha:\n  a: 1")))))

(ert-deftest scalpel-locate-yaml-test-top-definition-range-not-found ()
  "An absent key returns nil."
  (with-temp-buffer
    (insert "alpha:\n")
    (should (null (scalpel-locate-yaml--top-definition-range "beta")))))

(ert-deftest scalpel-locate-yaml-test-list-symbols ()
  "Top-level keys are listed in document order."
  (with-temp-buffer
    (insert "alpha:\n  nested: 1\nbeta: 2\n")
    (should (equal (scalpel-locate-yaml-list-symbols nil)
                   '("alpha" "beta")))))

(ert-deftest scalpel-locate-yaml-test-single-definition-p ()
  "One key block is accepted; several or none are rejected."
  (should (scalpel-locate-yaml--single-definition-p "alpha:\n  a: 1\n"))
  (should-not (scalpel-locate-yaml--single-definition-p
               "alpha:\n  a: 1\nbeta: 2\n"))
  (should-not (scalpel-locate-yaml--single-definition-p
               "  indented: 1\n")))

(provide 'scalpel-locate-yaml-test)

;;; scalpel-locate-yaml-test.el ends here
