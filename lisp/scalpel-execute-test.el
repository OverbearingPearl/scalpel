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

(ert-deftest scalpel-execute-test-insert-after-separates-tight-neighbours ()
  "An insertion between two tight definitions gains blank lines.
It gets one blank line on each side, and never two."
  (with-temp-buffer
    (insert "(defun a ())\n(defun c ())\n")
    (goto-char (point-min))
    (let ((end (progn (search-forward "(defun a ())") (point))))
      (scalpel-execute-insert-after end "(defun b ())"))
    (ert-info ((format "Buffer:\n%S" (buffer-string)))
      (should (string= (buffer-string)
                       (concat "(defun a ())\n\n(defun b ())\n\n"
                               "(defun c ())\n"))))))

(ert-deftest scalpel-execute-test-insert-after-keeps-existing-blanks ()
  "Blank lines already present at a join are kept, never doubled."
  (with-temp-buffer
    (insert "(defun a ())\n\n(defun c ())\n")
    (goto-char (point-min))
    (let ((end (progn (search-forward "(defun a ())") (point))))
      (scalpel-execute-insert-after end "(defun b ())"))
    (ert-info ((format "Buffer:\n%S" (buffer-string)))
      (should (string= (buffer-string)
                       (concat "(defun a ())\n\n(defun b ())\n\n"
                               "(defun c ())\n"))))))

(ert-deftest scalpel-execute-test-insert-after-at-buffer-end ()
  "An insertion after the last definition still gets a blank line.
The separating blank line is added and the insertion ends with a
terminated line."
  (with-temp-buffer
    (insert "(defun a ())\n")
    (goto-char (point-min))
    (let ((end (progn (search-forward "(defun a ())") (point))))
      (scalpel-execute-insert-after end "(defun b ())"))
    (ert-info ((format "Buffer:\n%S" (buffer-string)))
      (should (string= (buffer-string)
                       "(defun a ())\n\n(defun b ())\n")))))

(ert-deftest scalpel-execute-test-insert-after-refuses-unbalanced ()
  "An unbalanced insertion is refused, as a replacement is."
  (with-temp-buffer
    (insert "(defun a ())\n")
    (goto-char (point-min))
    (let ((end (progn (search-forward "(defun a ())") (point))))
      (should-error
       (scalpel-execute-insert-after end "(defun b (")
       :type 'user-error))))

(ert-deftest scalpel-execute-test-delete-takes-the-autoload-cookie ()
  "Deleting a definition takes its autoload cookie with it.
Regression: `block-delete' removed the form the locator reported and
left the `;;;###autoload' line standing alone, where it autoloads a
symbol the file no longer defines.  The cookie is a separate line, so
the locator never covered it; only the execute provider can carry it
out.  The buffer must visit a `.el' file for the provider to apply,
which is why this test does not use a bare `with-temp-buffer'."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert ";;;###autoload\n(defun foo ())\n\n(defun bar ())\n"))
    (with-current-buffer (find-file-noselect this-file)
      (goto-char (point-min))
      (search-forward "(defun foo ())")
      (scalpel-execute-delete (line-beginning-position) (line-end-position))
      (ert-info ((format "Buffer:\n%S" (buffer-string)))
        (should (string= (buffer-string) "(defun bar ())\n"))))))

(ert-deftest scalpel-execute-test-collapse-refuses-to-eat-a-definition ()
  "A collapse position inside a definition refuses instead of deleting it.
Regression: `block-delete' collapsed at the block's own start rather
than the start the deletion used; with the autoload cookie carried up,
that position sat at the end of the *next* definition's line, the blank
run found above it reached the buffer start, and the whole remaining
buffer -- definition and all -- was deleted."
  (with-temp-buffer
    (insert "\n(defun bar ())\n")
    (ert-info ("POS 16 is the trailing newline, not the join at 1")
      (should-error (scalpel-execute--collapse-blank-lines 16) :type 'error)
      (ert-info ((format "Buffer:\n%S" (buffer-string)))
        (should (string= (buffer-string) "\n(defun bar ())\n"))))))

(provide 'scalpel-execute-test)

;;; scalpel-execute-test.el ends here
