;;; scalpel-user-prompt-test.el --- Tests for scalpel-user-prompt -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the user prompt fragments read from ~/.scalpel.

;;; Code:

(require 'ert)
(require 'scalpel-user-prompt)

(ert-deftest scalpel-user-prompt-test-file-name-nil-for-missing-value
  ()
  "A nil or empty dimension contributes no prompt file name."
  (should (null (scalpel-user-prompt--file-name nil)))
  (should (null (scalpel-user-prompt--file-name ""))))

(ert-deftest scalpel-user-prompt-test-language-comes-from-the-extension
  ()
  "The language name is translated from the file's extension."
  (should (equal (scalpel-user-prompt--language "/tmp/foo/bar.el")
                 "elisp"))
  (should (equal (scalpel-user-prompt--language "/tmp/foo/README.md")
                 "markdown"))
  (should (null (scalpel-user-prompt--language "/tmp/foo/noext"))))

(ert-deftest scalpel-user-prompt-test-for-file-attaches-the-language-match
  ()
  "A language-matching prompt file is attached for the file."
  (let* ((dir (make-temp-file "scalpel-uprompt" t))
         (scalpel-user-prompt-dir dir)
         (file (expand-file-name "foo.el" dir)))
    (with-temp-file (expand-file-name "prompt.elisp" dir)
      (insert "keep forms balanced"))
    (should (equal (scalpel-user-prompt-for-file file)
                   "<!-- Scalpel user prompt: prompt.elisp -->\n\nkeep forms balanced"))))

(ert-deftest scalpel-user-prompt-test-for-file-is-nil-without-a-match ()
  "No prompt is attached when no prompt file matches."
  (let* ((dir (make-temp-file "scalpel-uprompt" t))
         (scalpel-user-prompt-dir dir))
    (should (null (scalpel-user-prompt-for-file
                   (expand-file-name "foo.el" dir))))))

(ert-deftest scalpel-user-prompt-test-for-files-deduplicates-a-shared-match
  ()
  "Several files of one language attach the prompt only once."
  (let* ((dir (make-temp-file "scalpel-uprompt" t))
         (scalpel-user-prompt-dir dir))
    (with-temp-file (expand-file-name "prompt.elisp" dir)
      (insert "one"))
    (should (equal (scalpel-user-prompt-for-files
                    (list (expand-file-name "a.el" dir)
                          (expand-file-name "b.el" dir)))
                   "<!-- Scalpel user prompt: prompt.elisp -->\n\none"))))

(ert-deftest scalpel-user-prompt-test-for-files-unions-distinct-matches ()
  "A mixed round attaches every matching prompt, each once."
  (let* ((dir (make-temp-file "scalpel-uprompt" t))
         (scalpel-user-prompt-dir dir))
    (with-temp-file (expand-file-name "prompt.elisp" dir)
      (insert "one"))
    (with-temp-file (expand-file-name "prompt.markdown" dir)
      (insert "two"))
    (should (equal (scalpel-user-prompt-for-files
                    (list (expand-file-name "a.el" dir)
                          (expand-file-name "README.md" dir)))
                   "<!-- Scalpel user prompt: prompt.elisp -->\n\none\n\n<!-- Scalpel user prompt: prompt.markdown -->\n\ntwo"))))

(ert-deftest scalpel-user-prompt-test-with-language-rule-concatenates-both
  ()
  "The per-file rule carries the language rule and the user prompt."
  (let* ((dir (make-temp-file "scalpel-uprompt" t))
         (scalpel-user-prompt-dir dir)
         (file (expand-file-name "foo.el" dir)))
    (with-temp-file (expand-file-name "prompt.elisp" dir)
      (insert "user rule"))
    (let ((scalpel-prompt-language-rules
           '(("\\.el\\'" . "builtin rule"))))
      (should (equal (scalpel-user-prompt-with-language-rule file)
                     "builtin rule\n\n<!-- Scalpel user prompt: prompt.elisp -->\n\nuser rule"))
      (let ((scalpel-prompt-language-rules nil))
        (should (equal (scalpel-user-prompt-with-language-rule file)
                       "<!-- Scalpel user prompt: prompt.elisp -->\n\nuser rule"))))))

(provide 'scalpel-user-prompt-test)

;;; scalpel-user-prompt-test.el ends here
