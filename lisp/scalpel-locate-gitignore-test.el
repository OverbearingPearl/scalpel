;;; scalpel-locate-gitignore-test.el --- Tests for scalpel-locate-gitignore -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the .gitignore locator provider.

;;; Code:

(require 'ert)
(require 'scalpel-locate-gitignore)

(ert-deftest scalpel-locate-gitignore-test-top-definition-range ()
  "A pattern's range is its whole line, newline included."
  (with-temp-buffer
    (insert "*.log\nbuild/\n")
    (let ((range (scalpel-locate-gitignore--top-definition-range "*.log")))
      (ert-info ((format "Range: %S" range))
        (should range)
        (should (string=
                 (buffer-substring-no-properties (car range) (cdr range))
                 "*.log\n"))))))

(ert-deftest scalpel-locate-gitignore-test-top-definition-range-not-found ()
  "An absent pattern returns nil."
  (with-temp-buffer
    (insert "*.log\n")
    (should (null (scalpel-locate-gitignore--top-definition-range "build/")))))

(ert-deftest scalpel-locate-gitignore-test-list-symbols-skips-comments ()
  "Comments and blank lines are never symbols."
  (with-temp-buffer
    (insert "# comment\n\n*.log\nbuild/\n")
    (should (equal (scalpel-locate-gitignore-list-symbols nil)
                   '("*.log" "build/")))))

(ert-deftest scalpel-locate-gitignore-test-single-definition-p ()
  "One pattern line is accepted; several or none are rejected."
  (should (scalpel-locate-gitignore--single-definition-p "*.log\n"))
  (should (scalpel-locate-gitignore--single-definition-p "*.log"))
  (should-not (scalpel-locate-gitignore--single-definition-p
               "*.log\nbuild/\n"))
  (should-not (scalpel-locate-gitignore--single-definition-p
               "# comment\n")))

(provide 'scalpel-locate-gitignore-test)

;;; scalpel-locate-gitignore-test.el ends here
