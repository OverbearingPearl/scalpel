;;; scalpel-test.el --- Test entry for Scalpel -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the test entry point for Scalpel.  Loading this file adds the
;; package root and the Lisp directory to `load-path' and provides
;; `scalpel-test-run' to run all ERT tests.

;;; Code:

(require 'ert)

(defvar scalpel-test--package-root
  (or (and load-file-name (file-name-directory load-file-name))
      default-directory)
  "Root directory of the Scalpel package source.")

(add-to-list 'load-path scalpel-test--package-root)
(add-to-list 'load-path (expand-file-name "lisp" scalpel-test--package-root))

(defun scalpel-test--lisp-files ()
  "Return Scalpel `lisp' directory file names (no directory)."
  (directory-files (expand-file-name "lisp" scalpel-test--package-root)
                   nil "^[^.]+\\.el$"))

(defun scalpel-test--module-features ()
  "Derive feature symbols of all Scalpel modules from lisp/ file names."
  (mapcar (lambda (file)
            (intern (file-name-base file)))
          (scalpel-test--lisp-files)))

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
              (when (and (string-match-p "^scalpel-.*-mode-map$" (symbol-name sym))
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

(defun scalpel-test-run ()
  "Reload Scalpel modules, then run every Scalpel ERT test."
  (interactive)
  (ert-delete-all-tests)
  (scalpel-test--kill-temp-file-buffers)
  (scalpel-test-reload-modules)
  (dolist (file (scalpel-test--lisp-files))
    (when (string-match-p "-test\\.el$" file)
      (load-file (expand-file-name file
                                   (expand-file-name "lisp"
                                                     scalpel-test--package-root)))))
  (scalpel-test--kill-temp-file-buffers)
  (if noninteractive
      (ert-run-tests-batch-and-exit "scalpel-")
    (ert "scalpel-")
    (scalpel-test--kill-temp-file-buffers)))

(provide 'scalpel-test)

;;; scalpel-test.el ends here
