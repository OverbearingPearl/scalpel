;;; scalpel-sandbox-test.el --- Tests for scalpel-sandbox -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for sandbox policy construction and execution.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-sandbox)
(require 'scalpel-utils-test)

(ert-deftest scalpel-sandbox-test-rejects-empty-context ()
  "An empty context must not execute a command."
  (should-error
   (scalpel-sandbox--bwrap-argv "true" nil nil)
   :type 'user-error))

(ert-deftest scalpel-sandbox-test-generates-readonly-and-writable-binds ()
  "Readonly files use ro-bind and writable files use bind."
  (scalpel-utils-test-with-temp-file ".txt"
    (with-temp-file this-file (insert "data"))
    (let* ((readonly-file (make-temp-file "scalpel-sandbox-readonly-"))
           (argv (progn
                   (with-temp-file readonly-file (insert "readonly"))
                   (scalpel-sandbox--bwrap-argv
                    "cat /context/0"
                    (list this-file)
                    (list readonly-file))))
           (ro (cl-position "--ro-bind" argv :test #'string=))
           (rw (cl-position "--bind" argv :test #'string=)))
      (unwind-protect
          (progn
            (should ro)
            (should rw))
        (delete-file readonly-file)))))

(ert-deftest scalpel-sandbox-test-dynamic-context-is-read-per-call ()
  "Policy construction reflects the current context arguments."
  (let ((first (make-temp-file "scalpel-sandbox-a-"))
        (second (make-temp-file "scalpel-sandbox-b-")))
    (unwind-protect
        (progn
          (with-temp-file first (insert "a"))
          (with-temp-file second (insert "b"))
          (let ((a (scalpel-sandbox--bwrap-argv "true" nil (list first)))
                (b (scalpel-sandbox--bwrap-argv "true" nil (list second))))
            (should (member first a))
            (should-not (member second a))
            (should (member second b))
            (should-not (member first b))))
      (delete-file first)
      (delete-file second))))

(ert-deftest scalpel-sandbox-test-rejects-symlink ()
  "Symlink context files are rejected to prevent path escape."
  (skip-unless (fboundp 'make-symbolic-link))
  (let ((target (make-temp-file "scalpel-sandbox-target-"))
        (link (make-temp-file "scalpel-sandbox-link-")))
    (unwind-protect
        (progn
          (with-temp-file target (insert "target"))
          (delete-file link)
          (make-symbolic-link target link)
          (should-error
           (scalpel-sandbox--bwrap-argv "true" nil (list link))
           :type 'user-error))
      (when (file-exists-p target) (delete-file target))
      (when (file-exists-p link) (delete-file link)))))

(ert-deftest scalpel-sandbox-test-run-mocks-bwrap ()
  "Running a sandbox command invokes bubblewrap with generated argv."
  (let ((scalpel-sandbox-program "bwrap")
        (called nil)
        (orig-supported (symbol-function 'scalpel-sandbox-supported-p))
        (orig-call-process (symbol-function 'call-process))
        (system-type 'gnu/linux))
    (unwind-protect
        (progn
          (fset 'scalpel-sandbox-supported-p (lambda () t))
          (fset 'call-process
                (lambda (program _in _buffer _display &rest args)
                  (setq called (cons program args))
                  ;; DESTINATION may be t (current buffer) in the real
                  ;; `call-process', so insert into the current buffer
                  ;; instead of using BUFFER as a buffer object.
                  (insert "ok")
                  0))
          (let ((file (make-temp-file "scalpel-sandbox-run-")))
            (unwind-protect
                (progn
                  (with-temp-file file (insert "data"))
                  (let ((result (scalpel-sandbox-run
                                 "true" temporary-file-directory nil (list file))))
                    (should (= (car result) 0))
                    (should (string= (cdr result) "ok"))
                    (should (equal (car called) "bwrap"))
                    (should (member "--unshare-net" (cdr called)))))
              (delete-file file))))
      (fset 'scalpel-sandbox-supported-p orig-supported)
      (fset 'call-process orig-call-process))))

(ert-deftest scalpel-sandbox-test-supported-p-platform-backs ()
  "Supported-p detects platform-specific sandbox executables."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program) (string= program "sandbox-exec"))))
    (let ((system-type 'darwin)
          (scalpel-sandbox-macos-program "sandbox-exec")
          (scalpel-sandbox-program "bwrap"))
      (should (scalpel-sandbox-supported-p))))
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program) (string= program "bwrap"))))
    (let ((system-type 'gnu/linux)
          (scalpel-sandbox-macos-program "sandbox-exec")
          (scalpel-sandbox-program "bwrap"))
      (should (scalpel-sandbox-supported-p)))))

(ert-deftest scalpel-sandbox-test-macos-profile-context-paths ()
  "Check that macOS profile grants write only to writable context files."
  (let ((writable-file (make-temp-file "scalpel-sandbox-w-"))
        (readonly-file (make-temp-file "scalpel-sandbox-ro-")))
    (unwind-protect
        (progn
          (with-temp-file writable-file (insert "w"))
          (with-temp-file readonly-file (insert "r"))
          (let ((profile (scalpel-sandbox--macos-profile
                          (list writable-file)
                          (list readonly-file))))
            (let ((writepath (expand-file-name writable-file))
                  (readpath (expand-file-name readonly-file)))
              (should (string-match-p
                       (regexp-quote (format "(allow file-read* (literal \"%s\"))" readpath))
                       profile))
              (should (string-match-p
                       (regexp-quote (format "(allow file-read* (literal \"%s\"))" writepath))
                       profile))
              (should (string-match-p
                       (regexp-quote (format "(allow file-write* (literal \"%s\"))" writepath))
                       profile))
              (should-not (string-match-p
                           (regexp-quote (format "(allow file-write* (literal \"%s\"))" readpath))
                           profile)))))
      (delete-file writable-file)
      (delete-file readonly-file))))

(provide 'scalpel-sandbox-test)

;;; scalpel-sandbox-test.el ends here
