;;; scalpel-utils-test.el --- Shared test infrastructure for Scalpel -*- lexical-binding: t; -*-
;;; Commentary:

;; Common helpers for Scalpel tests: temporary file creation and
;; guaranteed cleanup of buffers and files, even on test failure.

;;; Code:

(require 'cl-lib)

(defun scalpel-utils-test-kill-file-buffer (file)
  "Kill the buffer visiting FILE, if any, without prompting.
FILE is a file name string.  The buffer's modified flag is cleared
first so that killing never asks for confirmation.  Does nothing
when no live buffer visits FILE."
  (when (get-file-buffer file)
    (with-current-buffer (get-file-buffer file)
      (set-buffer-modified-p nil))
    (kill-buffer (get-file-buffer file))))

(defun scalpel-utils-test-kill-buffer (buffer-name)
  "Kill the buffer named BUFFER-NAME, if any, without prompting.
BUFFER-NAME is a buffer name string, not a buffer object.  The
buffer's modified flag is cleared first so that killing never asks
for confirmation.  Does nothing when no live buffer has that
name."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (set-buffer-modified-p nil))
    (kill-buffer buffer-name)))

(defun scalpel-utils-test-delete-file (file)
  "Delete FILE from disk when it exists.
FILE is a file name string.  Does nothing when FILE is absent."
  (when (file-exists-p file) (delete-file file)))

(defmacro scalpel-utils-test-with-temp-file (suffix &rest body)
  "Create a temporary file and evaluate BODY with cleanup guaranteed.
SUFFIX is the file extension string (e.g. \".el\").  The created
file name is bound to the local variable `this-file' inside BODY.
Cleanup always runs, even when BODY signals or is interrupted: it
kills the file's buffer and deletes the file."
  (declare (indent 1))
  (let ((file-var (gensym "scalpel-temp-file-")))
    `(let* ((,file-var (make-temp-file "scalpel-test-" nil ,suffix))
            (this-file ,file-var))
       (unwind-protect
           (progn ,@body)
         (scalpel-utils-test-kill-file-buffer ,file-var)
         (scalpel-utils-test-delete-file ,file-var)))))

(provide 'scalpel-utils-test)

;;; scalpel-utils-test.el ends here
