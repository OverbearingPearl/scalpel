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
   (scalpel-sandbox--bwrap-argv "true" nil)
   :type 'user-error))

(ert-deftest scalpel-sandbox-test-failures-use-the-sandbox-error-type ()
  "Sandbox failures signal `scalpel-sandbox-error', a `user-error'.
The console keys off this type to keep a message that names the
backend out of the conversation sent to the planner; the type must
stay a `user-error', so existing handlers keep catching it."
  (should (memq 'user-error
                (get 'scalpel-sandbox-error 'error-conditions)))
  (should-error (scalpel-sandbox--bwrap-argv "true" nil)
                :type 'scalpel-sandbox-error))

(ert-deftest scalpel-sandbox-test-binds-context-files-read-only ()
  "Every context file is bound with --ro-bind and none with --bind.
Shell commands must not be able to write a context file: all file
changes go through `scalpel-execute'."
  (scalpel-utils-test-with-temp-file ".txt"
    (with-temp-file this-file (insert "data"))
    (let ((argv (scalpel-sandbox--bwrap-argv "cat /context/0"
                                             (list this-file))))
      (should (cl-position "--ro-bind" argv :test #'string=))
      (should-not (cl-position "--bind" argv :test #'string=)))))

(ert-deftest scalpel-sandbox-test-dynamic-context-is-read-per-call ()
  "Policy construction reflects the current context arguments."
  (let ((first (make-temp-file "scalpel-sandbox-a-"))
        (second (make-temp-file "scalpel-sandbox-b-")))
    (unwind-protect
        (progn
          (with-temp-file first (insert "a"))
          (with-temp-file second (insert "b"))
          (let ((a (scalpel-sandbox--bwrap-argv "true" (list first)))
                (b (scalpel-sandbox--bwrap-argv "true" (list second))))
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
           (scalpel-sandbox--bwrap-argv "true" (list link))
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
                  ;; The preflight probe must come back with its
                  ;; sentinel; the real command answers "ok".
                  (insert (if (string-match-p scalpel-sandbox--probe-sentinel
                                              (car (last args)))
                              scalpel-sandbox--probe-sentinel
                            "ok"))
                  0))
          (let ((file (make-temp-file "scalpel-sandbox-run-")))
            (unwind-protect
                (progn
                  (with-temp-file file (insert "data"))
                  (let ((result (scalpel-sandbox-run
                                 "true" temporary-file-directory (list file))))
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
  "Check that the macOS profile grants context files read but never write."
  (let ((first-file (make-temp-file "scalpel-sandbox-w-"))
        (second-file (make-temp-file "scalpel-sandbox-ro-")))
    (unwind-protect
        (progn
          (with-temp-file first-file (insert "w"))
          (with-temp-file second-file (insert "r"))
          (let ((profile (scalpel-sandbox--macos-profile
                          (list first-file second-file))))
            (dolist (file (list first-file second-file))
              (let ((path (expand-file-name file)))
                (should (string-match-p
                         (regexp-quote (format "(allow file-read* (literal \"%s\"))" path))
                         profile))
                (should-not (string-match-p
                             (regexp-quote (format "(allow file-write* (literal \"%s\"))" path))
                             profile))))))
      (delete-file first-file)
      (delete-file second-file))))

(ert-deftest scalpel-sandbox-test-invoke-refuses-signalled-runtime ()
  "A runtime killed by a signal is refused, not reported as output."
  (let ((scalpel-sandbox-program "bwrap")
        (system-type 'gnu/linux)
        (orig-call-process (symbol-function 'call-process))
        (file (make-temp-file "scalpel-sandbox-signal-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "x"))
          (fset 'call-process (lambda (&rest _ignore) "Abort trap: 6"))
          (should-error (scalpel-sandbox--invoke "true" (list file))
                        :type 'user-error))
      (fset 'call-process orig-call-process)
      (delete-file file))))

(ert-deftest scalpel-sandbox-test-run-refuses-when-probe-fails ()
  "A failing sandbox probe blocks the command instead of running it."
  (let ((system-type 'gnu/linux)
        (orig-supported (symbol-function 'scalpel-sandbox-supported-p))
        (orig-invoke (symbol-function 'scalpel-sandbox--invoke))
        (ran 0)
        (file (make-temp-file "scalpel-sandbox-probe-fail-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "x"))
          (fset 'scalpel-sandbox-supported-p (lambda () t))
          (fset 'scalpel-sandbox--invoke
                (lambda (&rest _ignore)
                  (setq ran (1+ ran))
                  (cons 0 "not the sentinel")))
          (should-error (scalpel-sandbox-run "true" nil (list file))
                        :type 'user-error)
          (ert-info ((format "invoke calls: %d" ran))
            ;; Only the probe ran; the real command never did.
            (should (= ran 1))))
      (fset 'scalpel-sandbox-supported-p orig-supported)
      (fset 'scalpel-sandbox--invoke orig-invoke)
      (delete-file file))))

(ert-deftest scalpel-sandbox-test-run-proceeds-when-probe-passes ()
  "A passing probe lets the command run through the sandbox."
  (let ((system-type 'gnu/linux)
        (orig-supported (symbol-function 'scalpel-sandbox-supported-p))
        (orig-invoke (symbol-function 'scalpel-sandbox--invoke))
        (commands nil)
        (file (make-temp-file "scalpel-sandbox-probe-ok-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "x"))
          (fset 'scalpel-sandbox-supported-p (lambda () t))
          (fset 'scalpel-sandbox--invoke
                (lambda (command _files)
                  (push command commands)
                  (if (string-match-p scalpel-sandbox--probe-sentinel command)
                      (cons 0 scalpel-sandbox--probe-sentinel)
                    (cons 0 "ok"))))
          (let ((result (scalpel-sandbox-run "true" nil (list file))))
            (should (= (car result) 0))
            (should (string= (cdr result) "ok"))
            (should (= (length commands) 2))
            (should (string= (car commands) "true"))))
      (fset 'scalpel-sandbox-supported-p orig-supported)
      (fset 'scalpel-sandbox--invoke orig-invoke)
      (delete-file file))))

(ert-deftest scalpel-sandbox-test-sandbox-argv-p ()
  "Argv validation rejects anything that is not a sandbox invocation."
  (let ((scalpel-sandbox-program "bwrap")
        (scalpel-sandbox-macos-program "sandbox-exec"))
    (should (scalpel-sandbox--sandbox-argv-p
             '("bwrap" "--unshare-net" "/bin/sh" "-c" "true")))
    (should (scalpel-sandbox--sandbox-argv-p
             '("sandbox-exec" "-p" "(version 1)" "/bin/sh" "-c" "true")))
    (should-not (scalpel-sandbox--sandbox-argv-p
                 '("/bin/sh" "-c" "true")))
    (should-not (scalpel-sandbox--sandbox-argv-p nil))))

(ert-deftest scalpel-sandbox-test-macos-profile-names-resolved-paths ()
  "The macOS profile names the resolved path of a context file.
Regression: the sandbox matches its rules against resolved paths,
so a rule written only for a path reached through a symlinked
directory never matched, and the sandboxed shell could not resolve
its own working directory."
  (skip-unless (fboundp 'make-symbolic-link))
  (let* ((root (make-temp-file "scalpel-sandbox-real-" t))
         (link (make-temp-file "scalpel-sandbox-link-")))
    (unwind-protect
        (progn
          (delete-file link)
          (make-symbolic-link (directory-file-name root) link)
          (let* ((file (expand-file-name "ctx.el" link))
                 (resolved (file-truename file)))
            (with-temp-file file (insert "(defun foo ())"))
            (let ((profile (scalpel-sandbox--macos-profile (list file))))
              (ert-info ((format "Profile:\n%s" profile))
                (should (string-match-p
                         (regexp-quote
                          (format "(allow file-read* (literal \"%s\"))"
                                  file))
                         profile))
                (should (string-match-p
                         (regexp-quote
                          (format "(allow file-read* (literal \"%s\"))"
                                  resolved))
                         profile))))))
      (delete-directory root t)
      (when (file-symlink-p link) (delete-file link)))))

(ert-deftest scalpel-sandbox-test-macos-profile-grants-ancestors ()
  "Every ancestor directory of a context file is readable.
Regression: only the files themselves were granted, so `cd' into a
context file's parent reported ENOTDIR and `getcwd' failed inside
it, and the shell error leaked the sandbox to the user."
  (let ((file (make-temp-file "scalpel-sandbox-anc-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "x"))
          (let* ((resolved (file-truename file))
                 (profile (scalpel-sandbox--macos-profile
                           (list resolved))))
            (ert-info ((format "Profile:\n%s" profile))
              (dolist (dir (scalpel-sandbox--ancestor-dirs resolved))
                (should
                 (string-match-p
                  (regexp-quote
                   (format "(allow file-read* (literal %S))"
                           (file-truename dir)))
                  profile))))))
      (delete-file file))))

(ert-deftest scalpel-sandbox-test-macos-profile-grants-no-directory-read ()
  "The macOS profile never grants reads beyond the context files.
Regression risk: a `(allow file-read* (subpath DIR))' clause for a
context file's parent directory exposes every sibling file, so a
command could read files the user never added to the context.
The Linux backend binds individual files and has no such exposure."
  (let ((file (make-temp-file "scalpel-sandbox-boundary-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(defun foo ())"))
          (let* ((resolved (expand-file-name file))
                 (dir (file-name-directory resolved))
                 (profile (scalpel-sandbox--macos-profile (list file))))
            (ert-info ((format "Profile:\n%s" profile))
              (dolist (candidate (list dir (directory-file-name dir)))
                (should-not
                 (string-match-p
                  (regexp-quote
                   (format "(allow file-read* (subpath %S))" candidate))
                  profile)))
              ;; The context file itself stays readable.
              (should
               (string-match-p
                (regexp-quote
                 (format "(allow file-read* (literal %S))" resolved))
                profile)))))
      (delete-file file))))

(ert-deftest scalpel-sandbox-test-workdir-is-readable-tmp ()
  "The child's cwd is the temp directory, which every profile grants.
Regression: the cwd was a context file's parent, whose directory
read was removed to stop exposing sibling files, so `/bin/sh'
could no longer resolve its cwd and wrote a getcwd warning to
stderr -- stderr that `call-process' captures together with stdout."
  (should (string= (scalpel-sandbox--workdir)
                   (file-name-as-directory
                    (file-truename
                     (expand-file-name temporary-file-directory))))))

(ert-deftest scalpel-sandbox-test-macos-reads-context-file ()
  "The macOS profile must read but never write a context file.
Regression: the profile named both spellings of the temp directory, but
the sandbox matches the resolved path, so a command naming the
unresolved `/var/folders/...' spelling was denied with EPERM."
  (skip-unless (and (eq system-type 'darwin)
                    (executable-find scalpel-sandbox-macos-program)))
  (scalpel-utils-test-with-temp-file ".txt"
    (with-temp-file this-file (insert "ORIGINAL"))
    (let ((result (scalpel-sandbox-run
                   (format "cat %s"
                           (shell-quote-argument (file-truename this-file)))
                   nil (list (file-truename this-file)))))
      (ert-info ((format "Result: %S" result))
        (should (= (car result) 0))
        (should (string-match-p "ORIGINAL" (cdr result)))))))

(ert-deftest scalpel-sandbox-test-preflight-touches-a-file ()
  "The probe must read and write a file, not only print a sentinel.
Regression: a profile whose path rules were entirely wrong passed the
probe, because printing a sentinel needs no file access at all."
  (let ((system-type 'gnu/linux)
        (orig-supported (symbol-function 'scalpel-sandbox-supported-p))
        (orig-invoke (symbol-function 'scalpel-sandbox--invoke))
        (commands nil)
        (file (make-temp-file "scalpel-sandbox-probe-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "x"))
          (fset 'scalpel-sandbox-supported-p (lambda () t))
          (fset 'scalpel-sandbox--invoke
                (lambda (command _files)
                  (push command commands)
                  (if (string-match-p scalpel-sandbox--probe-sentinel command)
                      (cons 0 scalpel-sandbox--probe-sentinel)
                    (cons 0 "ok"))))
          (scalpel-sandbox-run "true" nil (list file))
          (let ((probe (car (last commands))))
            (ert-info ((format "Probe command: %S" probe))
              (should (string-match-p ">" probe))
              (should (string-match-p "cat" probe)))))
      (fset 'scalpel-sandbox-supported-p orig-supported)
      (fset 'scalpel-sandbox--invoke orig-invoke)
      (delete-file file))))

(provide 'scalpel-sandbox-test)

;;; scalpel-sandbox-test.el ends here
