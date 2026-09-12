;;; scalpel-test.el --- Test entry for Scalpel -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the test entry point for Scalpel.  Loading this file adds the
;; package root and the Lisp directory to `load-path' and provides
;; `scalpel-test-run' to run all ERT tests.

;;; Code:

(require 'cl-lib)
(require 'package)
(package-initialize)
(require 'ert)

(defvar scalpel-test--package-root
  (or (and load-file-name (file-name-directory load-file-name))
      default-directory)
  "Root directory of the Scalpel package source.")

(add-to-list 'load-path scalpel-test--package-root)
(add-to-list 'load-path (expand-file-name "lisp" scalpel-test--package-root))

(defun scalpel-test--lisp-files ()
  "Return Scalpel `lisp' directory file names (no directory)."
  (sort
   (directory-files (expand-file-name "lisp" scalpel-test--package-root)
                    nil "^[^.]+\\.el$")
   #'string<))

(defun scalpel-test--file-feature (file)
  "Return the feature symbol FILE provides.
FILE is a bare file name from the `lisp' directory, such as
\"scalpel-utils-test.el\".  A module is named after the feature it
provides, so the name alone identifies it."
  (intern (file-name-base file)))

(defun scalpel-test--module-features ()
  "Derive feature symbols of all Scalpel modules from lisp/ file names."
  (mapcar #'scalpel-test--file-feature (scalpel-test--lisp-files)))

(defun scalpel-test--test-files ()
  "Return the `*-test.el' file names under `lisp'."
  (cl-remove-if-not (lambda (file) (string-match-p "-test\\.el$" file))
                    (scalpel-test--lisp-files)))

(defun scalpel-test--skip-test-file-p (feature preloaded)
  "Return non-nil when the test file providing FEATURE is loaded already.
PRELOADED is the list of features provided before this run's load
pass began.  A feature in that list may still hold code from the
previous run -- an unload that did not take -- so its file is loaded
again and the run sees what is on disk.  Any other provided feature
was loaded by a sibling test file's `require' during this pass, and
loading that file again would hand ERT the same test twice."
  (and (featurep feature) (not (memq feature preloaded))))

(defun scalpel-test--kill-temp-file-buffers ()
  "Kill all file buffers visiting files under the temp directory.
The directory is the value of the variable `temporary-file-directory'.
Clear the modified flag first so killing never prompts.  This is a
safety net for tests interrupted before their own cleanup ran."
  (dolist (buf (buffer-list))
    (let ((file (buffer-file-name buf)))
      (when (and file
                 (file-name-absolute-p file)
                 (string-prefix-p (file-name-as-directory
                                   (expand-file-name temporary-file-directory))
                                  (expand-file-name file)))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(defun scalpel-test-reload-modules ()
  "Reload all Scalpel modules so that ERT can exercise the latest code."
  (interactive)
  ;; Unload every module feature derived from the lisp directory.
  (dolist (feat (scalpel-test--module-features))
    (when (featurep feat)
      (condition-case nil
          (unload-feature feat t)
        (error nil))))
  (when (featurep 'scalpel)
    (condition-case nil
        (unload-feature 'scalpel t)
      (error nil)))
  ;; Clear only keymaps: reloading re-creates them via defvar, and stale
  ;; bindings would otherwise survive.  User configuration (defcustom etc.)
  ;; is intentionally left untouched.
  (mapatoms (lambda (sym)
              (when (and (string-match-p "^scalpel-.*-map$" (symbol-name sym))
                         (boundp sym))
                (makunbound sym))))
  ;; Load main module, then lisp sources (tests are excluded here; they are
  ;; loaded after all implementation modules).
  (let ((scalpel-el (expand-file-name "scalpel.el" scalpel-test--package-root)))
    (when (file-exists-p scalpel-el)
      (load-file scalpel-el)))
  (dolist (file (scalpel-test--lisp-files))
    (unless (string-match-p "-test\\.el$" file)
      (load-file (expand-file-name file
                                   (expand-file-name "lisp"
                                                     scalpel-test--package-root)))))
  (message "Scalpel modules reloaded."))

(defun scalpel-test--load-test-files ()
  "Load every `*-test.el' under `lisp', each file once and from disk.
A test file may `require' a sibling: `scalpel-agent-test' needs the
`scalpel-utils-test-with-temp-file' macro at expansion time, so it
cannot wait for this loop -- which visits files in name order -- to
reach `scalpel-utils-test'.  ERT signals when it is handed a test it
already knows, so that `require' counts as the file's load and the
loop does not load it again.  Call this once per run, with the
previous run's tests already deleted."
  (let ((preloaded
         (cl-remove-if-not
          #'featurep
          (mapcar #'scalpel-test--file-feature
                  (scalpel-test--test-files)))))
    (dolist (file (scalpel-test--test-files))
      (let ((feature (scalpel-test--file-feature file)))
        (unless (scalpel-test--skip-test-file-p feature preloaded)
          (load-file (expand-file-name
                      file (expand-file-name "lisp"
                                             scalpel-test--package-root))))))))

(defun scalpel-test-run ()
  "Reload Scalpel modules, then run every Scalpel ERT test."
  (interactive)
  (ert-delete-all-tests)
  (scalpel-test--kill-temp-file-buffers)
  (scalpel-test-reload-modules)
  (scalpel-test--load-test-files)
  (scalpel-test--kill-temp-file-buffers)
  (if noninteractive
      (ert-run-tests-batch-and-exit "scalpel-")
    (ert "scalpel-")
    (scalpel-test--kill-temp-file-buffers)))

(provide 'scalpel-test)

;;; scalpel-test.el ends here
