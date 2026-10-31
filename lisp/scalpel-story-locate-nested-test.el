;;; scalpel-story-locate-nested-test.el --- Nested block note tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the nested-definition diagnostics:
;; `scalpel-locate-elisp--nested-definition-block' and
;; `scalpel-agent--nested-block-note'.

;;; Code:

(require 'ert)
(require 'scalpel-locate-elisp)
(require 'scalpel-agent)

(defconst scalpel-story-locate-nested-test-sample
  ";; sample
(require 'foo)
(use-package bar
  :config
  (defun bar-helper ()
    \"Doc.\"
    nil))
(defconst top-name 1)
(provide 'sample)
"
  "A file whose bar-helper sits in a `use-package' :config body.")

(defmacro scalpel-story-locate-nested-test-with-sample (&rest body)
  "Write the sample to a temp file and run BODY with it visited.
Cleans the buffer and file either way."
  (declare (indent 0))
  `(let ((file (make-temp-file "scalpel-nested-" nil ".el")))
     (unwind-protect
         (progn
           (write-region scalpel-story-locate-nested-test-sample nil
                         file nil 'silent)
           (with-current-buffer (find-file-noselect file)
             ,@body))
       (let ((buffer (find-buffer-visiting file)))
         (when buffer (kill-buffer buffer)))
       (delete-file file))))

(ert-deftest scalpel-story-locate-nested-test-block-found ()
  (ert-info ("A defun inside use-package :config reports use-package bar")
    (scalpel-story-locate-nested-test-with-sample
     (should (equal (scalpel-locate-elisp--nested-definition-block
                     "bar-helper")
                    "use-package bar")))))

(ert-deftest scalpel-story-locate-nested-test-top-level-nil ()
  (ert-info ("A top-level definition reports nil, not a block")
    (scalpel-story-locate-nested-test-with-sample
     (should (null (scalpel-locate-elisp--nested-definition-block
                    "top-name"))))))

(ert-deftest scalpel-story-locate-nested-test-note-suggests-remedies ()
  (ert-info ("The agent note names the block and both remedies")
    (scalpel-story-locate-nested-test-with-sample
     (let ((note (scalpel-agent--nested-block-note
                  (buffer-file-name) "bar-helper")))
       (should (string-match-p "use-package bar" note))
       (should (string-match-p "hoist" note))))))

(ert-deftest scalpel-story-locate-nested-test-note-nil-for-top-level ()
  (ert-info ("A top-level name adds no note")
    (scalpel-story-locate-nested-test-with-sample
     (should (null (scalpel-agent--nested-block-note
                    (buffer-file-name) "top-name"))))))

(provide 'scalpel-story-locate-nested-test)

;;; scalpel-story-locate-nested-test.el ends here
