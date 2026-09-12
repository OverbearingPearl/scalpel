;;; scalpel-utils-test.el --- Shared test infrastructure for Scalpel -*- lexical-binding: t; -*-

;;; Commentary:

;; Common helpers for Scalpel tests: temporary file creation and
;; guaranteed cleanup of buffers and files, even on test failure.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defun scalpel-utils-test-kill-file-buffer (file)
  "Kill the buffer visiting FILE, if any, without prompting.
FILE is a file name string.  The buffer is looked up under both the
name as given and its truename: the code under test resolves
temporary paths with `file-truename' before visiting them, and the
variable `temporary-file-directory' is, on macOS, a \"/var\" path
that symlinks to \"/private/var\", so `get-file-buffer' on the name
`make-temp-file' returned never matches a buffer visited under the
resolved name and that buffer would outlive the run.  The buffer's
modified flag is
cleared first so that killing never asks for confirmation.  Does
nothing when no live buffer visits FILE."
  (let* ((resolved (unless (file-remote-p file) (file-truename file)))
         (buf (or (get-file-buffer file)
                  (and resolved (get-file-buffer resolved)))))
    (when buf
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (kill-buffer buf))))

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
Backups are disabled for the body, because saving the file -- which
the edit primitives do -- would otherwise leave a `~' file behind
for every test that writes.  Cleanup always runs, even when BODY
signals or is interrupted: it kills the file's buffer and deletes
the file."
  (declare (indent 1))
  (let ((file-var (gensym "scalpel-temp-file-")))
    `(let* ((,file-var (make-temp-file "scalpel-test-" nil ,suffix))
            (this-file ,file-var)
            (make-backup-files nil))
       (unwind-protect
           (progn ,@body)
         (scalpel-utils-test-kill-file-buffer ,file-var)
         (scalpel-utils-test-delete-file ,file-var)))))

(ert-deftest scalpel-utils-test-kill-file-buffer-kills-truename-buffer ()
  "A buffer visited under the resolved name is killed as well.
Regression: cleanup looked the buffer up with `get-file-buffer' on
the name `make-temp-file' returned, but the code under test resolves
temporary paths with `file-truename'; where that name differs --
macOS reports \"/private/var/...\" for a \"/var/...\" temp directory
-- the lookup missed and every buffer the read tests visited
outlived their run."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let ((resolved (file-truename this-file)))
      ;; Visit through the resolved name, the way `scalpel-agent-read'
      ;; does, so the two spellings really differ on this platform.
      (find-file-noselect resolved)
      (ert-info ((format "Resolved name: %S" resolved))
        (should (get-file-buffer resolved)))
      (scalpel-utils-test-kill-file-buffer this-file)
      (ert-info ((format "Survivors: %S"
                         (mapcar #'buffer-name (buffer-list))))
        (should-not (get-file-buffer resolved))))))

(defun scalpel-utils-test--lisp-directory ()
  "Return the directory holding the Scalpel test files.
Derived from where this function itself was defined, so the answer
holds whether this file arrived through `load-path' or was loaded
by name; a `lisp' test file may not require the root test entry, so
this file must work the directory out on its own."
  (file-name-directory
   (or (symbol-file 'scalpel-utils-test--lisp-directory 'defun)
       (locate-library "scalpel-utils-test")
       (error "Scalpel: cannot locate the Scalpel test directory"))))

(defun scalpel-utils-test--test-files ()
  "Return the `*-test.el' file names under the Scalpel Lisp directory."
  (cl-remove-if-not (lambda (file) (string-match-p "-test\\.el\\'" file))
                    (directory-files (scalpel-utils-test--lisp-directory)
                                     nil "^[^.]+\\.el\\'")))

(ert-deftest scalpel-utils-test-every-test-file-provides-its-feature ()
  "Every test file provides the feature named after it.
The runner decides whether a test file still has to be loaded by
asking `featurep' on that name, because a sibling test file may
have required it first; such a file is skipped, so one that never
provides its feature is not skipped and the pass loads it a second
time, which makes ERT refuse the tests it defines.  Every file in
this directory is loaded by that pass, so all of them must provide."
  (let ((files (scalpel-utils-test--test-files)))
    ;; A directory derivation that silently returned nothing would make
    ;; this test vacuous, which is worse than not having it at all.
    (ert-info ((format "Test files: %S" files))
      (should files))
    (dolist (file files)
      (ert-info ((format "Test file: %s" file))
        (should (featurep (intern (file-name-base file))))))))

(provide 'scalpel-utils-test)

;;; scalpel-utils-test.el ends here
