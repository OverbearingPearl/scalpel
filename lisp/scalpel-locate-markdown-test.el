;;; scalpel-locate-markdown-test.el --- Tests for scalpel-locate-markdown -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Markdown locator provider.

;;; Code:

(require 'ert)
(require 'scalpel-locate-markdown)
(require 'scalpel-utils-test)

(ert-deftest scalpel-locate-markdown-test-top-definition-range ()
  "A heading section spans to the next heading of the same level."
  (with-temp-buffer
    (insert "# Alpha\nbody one\n\n## Sub\nsub body\n\n# Beta\n")
    (let ((range (scalpel-locate-markdown--top-definition-range "Alpha")))
      (ert-info ((format "Range: %S" range))
        (should range)
        (should (string=
                 (buffer-substring-no-properties (car range) (cdr range))
                 "# Alpha\nbody one\n\n## Sub\nsub body"))))))

(ert-deftest scalpel-locate-markdown-test-range-excludes-trailing-blanks ()
  "Trailing blank lines before the next section are not part of the range."
  (with-temp-buffer
    (insert "# Alpha\nbody\n\n\n# Beta\n")
    (let ((range (scalpel-locate-markdown--top-definition-range "Alpha")))
      (should (string=
               (buffer-substring-no-properties (car range) (cdr range))
               "# Alpha\nbody")))))

(ert-deftest scalpel-locate-markdown-test-top-definition-range-not-found ()
  "An absent heading returns nil."
  (with-temp-buffer
    (insert "# Alpha\n")
    (should (null (scalpel-locate-markdown--top-definition-range "Beta")))))

(ert-deftest scalpel-locate-markdown-test-list-symbols ()
  "Heading texts are listed in document order."
  (with-temp-buffer
    (insert "# Alpha\n## Sub\n# Beta\n")
    (should (equal (scalpel-locate-markdown-list-symbols nil)
                   '("Alpha" "Sub" "Beta")))))

(ert-deftest scalpel-locate-markdown-test-single-definition-p ()
  "One heading section is accepted; several or none are rejected."
  (should (scalpel-locate-markdown--single-definition-p "# Alpha\nbody\n"))
  (should (scalpel-locate-markdown--single-definition-p
           "# Alpha\n## Sub\nsub body\n"))
  (should-not (scalpel-locate-markdown--single-definition-p
               "# Alpha\n# Beta\n"))
  (should-not (scalpel-locate-markdown--single-definition-p
               "plain prose\n")))

(ert-deftest scalpel-locate-markdown-test-range-dispatched-by-file ()
  "The public API dispatches `.md' files to the Markdown provider."
  (scalpel-utils-test-with-temp-file ".md"
    (with-temp-file this-file
      (insert "# Alpha\nbody\n\n# Beta\n"))
    (let ((range (scalpel-locate-range this-file "Alpha")))
      (should (string=
               (with-current-buffer (get-file-buffer this-file)
                 (buffer-substring-no-properties (car range) (cdr range)))
               "# Alpha\nbody")))))

(provide 'scalpel-locate-markdown-test)

;;; scalpel-locate-markdown-test.el ends here
