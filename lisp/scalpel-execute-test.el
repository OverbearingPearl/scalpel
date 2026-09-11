;;; scalpel-execute-test.el --- Tests for scalpel-execute -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-execute.

;;; Code:

(require 'ert)
(require 'scalpel-execute)
(require 'scalpel-utils-test)

(defun scalpel-execute-test--delete-form (form)
  "Delete FORM from the current buffer, as a locator would resolve it.
FORM is the exact text of one whole top-level form.  The deletion
runs through `scalpel-execute-delete' with the range a locator hands
it: from the beginning of the form's line to the end of the form."
  (goto-char (point-min))
  (let ((end (progn (unless (search-forward form nil t)
                      (ert-fail (format "Form %S not found in:\n%S"
                                        form (buffer-string))))
                    (point))))
    (scalpel-execute-delete (line-beginning-position) end)))

(ert-deftest scalpel-execute-test-brackets-balanced-p ()
  "Balanced parens should be accepted, unbalanced rejected."
  (should (scalpel-execute--brackets-balanced-p "()"))
  (should-not (scalpel-execute--brackets-balanced-p "(")))

(ert-deftest scalpel-execute-test-replace ()
  "Replace a region with balanced text; reject unbalanced text."
  (with-temp-buffer
    (insert "(defun foo (x)\n  (+ x 1))\n")
    (let ((beg (point-min))
          (end (progn (goto-char (point-min))
                      (line-end-position 1))))
      (scalpel-execute-replace beg end "(defun foo (x)\n  (+ x 2))")
      (should (string= (buffer-string)
                       "(defun foo (x)\n  (+ x 2))\n  (+ x 1))\n")))
    (should-error
     (let ((beg (point-min))
           (end (progn (goto-char (point-min))
                       (line-end-position 1))))
       (scalpel-execute-replace beg end "(defun foo (x"))
     :type 'error)))

(ert-deftest scalpel-execute-test-replace-saves-the-file ()
  "A replacement reaches disk before `scalpel-execute-replace' returns.
Regression: edits lived only in the buffer until the user saved by
hand, so the shell commands the agent ran next read the old file."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ()\n  nil)\n"))
    (with-current-buffer (find-file-noselect this-file)
      (scalpel-execute-replace (point-min) (point-max)
                               "(defun bar ()\n  nil)\n"))
    (let ((on-disk (with-temp-buffer
                     (insert-file-contents this-file)
                     (buffer-string))))
      (ert-info ((format "On disk:\n%S" on-disk))
        (should (string= on-disk "(defun bar ()\n  nil)\n"))))))

(ert-deftest scalpel-execute-test-delete-collapses-joined-blank-lines ()
  "A block with a blank line on each side leaves exactly one behind.
Regression: the block's own line terminator survived the deletion, so
the blank line before the block and the blank line after it stacked
up as two blank lines where the block had been."
  (with-temp-buffer
    (insert "(defun a ())\n\n(defun b ())\n\n(defun c ())\n")
    (scalpel-execute-test--delete-form "(defun b ())")
    (ert-info ((format "Buffer:\n%S" (buffer-string)))
      (should (string= (buffer-string)
                       "(defun a ())\n\n(defun c ())\n")))))

(ert-deftest scalpel-execute-test-delete-adds-no-blank-line ()
  "A block with no blank line around it leaves none behind either.
The deleted line goes whole, so its neighbours become adjacent lines
instead of gaining a blank line between them."
  (with-temp-buffer
    (insert "(defun a ())\n(defun b ())\n(defun c ())\n")
    (scalpel-execute-test--delete-form "(defun b ())")
    (ert-info ((format "Buffer:\n%S" (buffer-string)))
      (should (string= (buffer-string)
                       "(defun a ())\n(defun c ())\n")))))

(ert-deftest scalpel-execute-test-delete-drops-blank-lines-at-buffer-end ()
  "Blank lines a deletion would leave at the end of the buffer go away."
  (with-temp-buffer
    (insert "(defun a ())\n\n(defun b ())\n")
    (scalpel-execute-test--delete-form "(defun b ())")
    (ert-info ((format "Buffer:\n%S" (buffer-string)))
      (should (string= (buffer-string) "(defun a ())\n")))))

(ert-deftest scalpel-execute-test-delete-drops-blank-lines-at-buffer-start ()
  "Blank lines a deletion would leave at the start of the buffer go away."
  (with-temp-buffer
    (insert "(defun a ())\n\n(defun b ())\n")
    (scalpel-execute-test--delete-form "(defun a ())")
    (ert-info ((format "Buffer:\n%S" (buffer-string)))
      (should (string= (buffer-string) "(defun b ())\n")))))

(provide 'scalpel-execute-test)

;;; scalpel-execute-test.el ends here
