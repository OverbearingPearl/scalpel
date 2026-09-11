;;; scalpel-sandbox.el --- Command sandbox for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Executes shell commands through bubblewrap.  The sandbox policy is rebuilt
;; for every invocation from the current context file lists.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom scalpel-sandbox-program "bwrap"
  "Executable used for Linux command sandboxing."
  :type 'file
  :group 'scalpel)

(defcustom scalpel-sandbox-macos-program "sandbox-exec"
  "Executable used for the experimental macOS command sandbox.
This backend uses the deprecated `sandbox-exec' tool; treat it as
experimental and keep it out of default trust decisions."
  :type 'file
  :group 'scalpel)

(defconst scalpel-sandbox--probe-sentinel "SCALPEL-SANDBOX-PROBE-OK"
  "Output a working sandbox must print back from its probe run.
Structural contract shared by `scalpel-sandbox--preflight' and the
tests that fake a sandbox executable.")

;; Sandbox failures carry their own condition type so that callers can
;; keep the mechanism out of the conversation sent to the planner.
;; Every message below names bubblewrap or sandbox-exec, and the
;; planner is meant to see an ordinary filesystem with a smaller reach,
;; not the boundary drawn around it.  The type inherits from
;; `user-error', so existing handlers and tests that catch `user-error'
;; still catch it.
(define-error 'scalpel-sandbox-error
  "Scalpel: command sandbox cannot run this command"
  'user-error)

(defun scalpel-sandbox--fail (format &rest args)
  "Signal `scalpel-sandbox-error', built from FORMAT and ARGS."
  (signal 'scalpel-sandbox-error (list (apply #'format format args))))

(defun scalpel-sandbox--file-paths (writable-files readonly-files)
  "Return normalized context files from WRITABLE-FILES and READONLY-FILES.
Reject missing files, directories, symlinks, and overlapping permissions."
  (let ((all (append writable-files readonly-files))
        (writable (delete-dups (mapcar #'expand-file-name writable-files)))
        (readonly (delete-dups (mapcar #'expand-file-name readonly-files))))
    (when (null all)
      (scalpel-sandbox--fail
       "Scalpel: shell sandbox requires at least one context file"))
    (dolist (file all)
      (unless (file-regular-p file)
        (scalpel-sandbox--fail
         "Scalpel: sandbox context is not a regular file: %s" file))
      (when (file-symlink-p file)
        (scalpel-sandbox--fail
         "Scalpel: sandbox rejects symlink context file: %s" file)))
    (when (cl-intersection writable readonly :test #'string=)
      (scalpel-sandbox--fail
       "Scalpel: sandbox context has conflicting file permissions"))
    (list writable readonly)))

(defun scalpel-sandbox--runtime-bindings ()
  "Return existing system runtime directories as read-only bind arguments."
  (cl-loop for directory in '("/usr" "/bin" "/lib" "/lib64")
           when (file-directory-p directory)
           append (list "--ro-bind" directory directory)))

(defun scalpel-sandbox--bwrap-argv (command writable-files readonly-files)
  "Build bubblewrap ARGV for COMMAND with context files.
WRITABLE-FILES and READONLY-FILES are lists of context files."
  (pcase-let ((`(,writable ,readonly)
               (scalpel-sandbox--file-paths writable-files readonly-files)))
    (append
     (list scalpel-sandbox-program
           "--die-with-parent"
           "--unshare-pid"
           "--unshare-uts"
           "--unshare-ipc"
           "--unshare-net"
           "--tmpfs" "/tmp")
     (scalpel-sandbox--runtime-bindings)
     (cl-loop for file in readonly
              append (list "--ro-bind" file file))
     (cl-loop for file in writable
              append (list "--bind" file file))
     (list "--chdir" "/tmp"
           "--setenv" "HOME" "/nonexistent"
           "--setenv" "PATH" "/usr/bin:/bin"
           "--"
           "/bin/sh" "-c" command))))

(defun scalpel-sandbox--path-forms (file)
  "Return every path string a sandbox profile must name for FILE.
Both the name as given and its canonical `file-truename' come back,
deduplicated.  The sandbox matches its filters against resolved
paths, so a path reached through a symlinked directory -- on macOS
\"/var\" is a symlink to \"/private/var\" -- stays invisible to a
rule that names only the unresolved form."
  (delete-dups
   (list (expand-file-name file)
         (file-truename (expand-file-name file)))))

(defun scalpel-sandbox--literal-clauses (operation file)
  "Return OPERATION clauses naming FILE, one per path form.
OPERATION is a filter such as \"allow file-read*\"; FILE is a
context file."
  (mapconcat (lambda (path) (format "(%s (literal %S))" operation path))
             (scalpel-sandbox--path-forms file)
             "\n"))

(defun scalpel-sandbox--subpath-clauses (operation path)
  "Return OPERATION subpath clauses for PATH, one per path form.
OPERATION is a filter such as \"allow file-read*\"; PATH is a
directory whose whole subtree is granted.  Every spelling of PATH
must be named: macOS keeps `/tmp' and `/var' as symlinks into
`/private', and the sandbox matches a rule against the path the
process actually names, so granting only one spelling leaves the
other denied."
  (mapconcat (lambda (dir) (format "(%s (subpath %S))" operation dir))
             (scalpel-sandbox--path-forms path)
             "\n"))

(defun scalpel-sandbox--ancestor-dirs (file)
  "Return every ancestor directory of FILE, outermost first.
FILE is an absolute path.  The filesystem root is included, in its
own spelling; FILE itself is not.  Names carry no trailing slash,
so they can be passed straight to `file-truename'.  Only `literal'
grants are derived from these: an ancestor is granted so that a
path through it can be resolved, never so its subtree can be read,
because a `subpath' grant would expose every sibling of the context
files the user actually added."
  (let ((dir (directory-file-name
              (file-name-directory (expand-file-name file))))
        out)
    (while (not (equal dir "/"))
      (push dir out)
      (setq dir (directory-file-name (file-name-directory dir))))
    (cons "/" out)))

(defun scalpel-sandbox--macos-profile (writable-files readonly-files)
  "Return a `sandbox-exec' profile for the context files.
WRITABLE-FILES and READONLY-FILES are lists of context files."
  (scalpel-sandbox--file-paths writable-files readonly-files)
  (let ((readonly (mapcar #'expand-file-name readonly-files))
        (writable (mapcar #'expand-file-name writable-files)))
    (concat
     "(version 1)\n"
     "(deny default)\n"
     "(allow process-fork)\n"
     "(allow process-exec)\n"
     "(allow sysctl-read)\n"
     "(allow mach-lookup)\n"
     ;; A `literal' read of the filesystem root is required for path
     ;; resolution: dyld stats `/` before loading a binary, and without
     ;; it the process aborts with SIGABRT.  `literal' grants only the
     ;; root directory entry itself, not recursive access to every path,
     ;; so this does not amount to allowing a whole-disk read.
     "(allow file-read* (literal \"/\"))\n"
     "(allow file-read* (literal \"/dev/null\"))\n"
     ;; `/bin/sh' consults this symlink to pick its shell personality;
     ;; denying the read is harmless but makes every shell startup
     ;; write a stderr warning, and that warning would be captured as
     ;; command output and break the preflight sentinel comparison.
     "(allow file-read* (subpath \"/private/var/select\"))\n"
     ;; Path resolution and `getcwd' walk every component of a path by
     ;; name, so each intermediate directory a granted path is reached
     ;; through needs its own literal grant: `/private' and
     ;; `/private/var' are not covered by the `/private/var/folders'
     ;; subpath, and without them `getcwd' fails inside the sandbox
     ;; ("getcwd: cannot access parent directories"), which leaves the
     ;; shell unable to refresh PWD after `cd'.  `literal' grants the
     ;; directory vnode only, never its contents.
     "(allow file-read* (literal \"/private\"))\n"
     "(allow file-read* (literal \"/private/var\"))\n"
     ;; `/var' and `/tmp' are symlinks into `/private'; resolving a
     ;; symlink is a read of the link itself, which no subpath rule
     ;; covers.
     "(allow file-read* (literal \"/var\"))\n"
     "(allow file-read* (literal \"/tmp\"))\n"
     ;; Every directory a context file sits under, at every level,
     ;; gets a `literal' read grant.  Without them the shell cannot
     ;; resolve a context file's own parent: `cd' into it reports
     ;; ENOTDIR and `getcwd' fails, so globs such as `lisp/*.el'
     ;; expand to nothing -- and the error names the sandbox to the
     ;; user, which the whole design is meant to hide.  The grant is
     ;; `literal', never `subpath': the directory entry itself is
     ;; readable, so paths through it resolve and the shell can list
     ;; it, but opening a sibling file still needs a grant of its
     ;; own, which only the context files get.  Only the canonical
     ;; spelling is emitted, because the sandbox canonicalizes its
     ;; rules at compile time and the other spelling would be the
     ;; same rule twice.
     (mapconcat
      (lambda (dir)
        (format "(allow file-read* (literal %S))" (file-truename dir)))
      (delete-dups
       (cl-loop for f in (append readonly writable)
                append (scalpel-sandbox--ancestor-dirs f)))
      "\n")
     "\n"
     "(allow file-write* (literal \"/dev/null\"))\n"
     ;; Temporary directories are readable as well as writable: the
     ;; child uses TMPDIR for its scratch files, and opening a file
     ;; with O_CREAT -- which is what a shell redirection does --
     ;; needs write permission on the directory, not only on the
     ;; file.  Both spellings of each directory are named, because
     ;; `/tmp' and `/var' are symlinks into `/private' and a rule
     ;; written for only one spelling does not cover the other.
     (mapconcat
      (lambda (dir)
        (concat (scalpel-sandbox--subpath-clauses "allow file-read*" dir)
                "\n"
                (scalpel-sandbox--subpath-clauses "allow file-write*" dir)))
      (list "/tmp" "/var/folders")
      "\n")
     "\n"
     ;; System runtimes the shell and its tools need in order to start.
     ;; These are read-only; writes stay bounded to context files and
     ;; the temporary directories above.
     (mapconcat #'identity
                (list "(allow file-read* (subpath \"/bin\"))"
                      "(allow file-read* (subpath \"/usr/bin\"))"
                      "(allow file-read* (subpath \"/usr/sbin\"))"
                      "(allow file-read* (subpath \"/sbin\"))"
                      "(allow file-read* (subpath \"/usr/lib\"))"
                      "(allow file-read* (subpath \"/usr/share\"))"
                      "(allow file-read* (subpath \"/System/Library\"))"
                      "(allow file-read* (subpath \"/Library/Apple\"))"
                      "(allow file-read* (subpath \"/private/etc\"))")
                "\n")
     "\n"
     ;; Only the context files themselves are readable: each file is
     ;; named with a `literal' filter, never its parent directory.
     ;; A directory-level read would expose every sibling file,
     ;; including ones the user never added to the context; the
     ;; Linux backend binds individual files for the same reason, so
     ;; the two backends must agree on this boundary.
     (if readonly
         (concat (mapconcat
                  (lambda (f) (scalpel-sandbox--literal-clauses
                              "allow file-read*" f))
                  readonly "\n")
                 "\n")
       "")
     (if writable
         (concat
          (mapconcat
           (lambda (f)
             (concat (scalpel-sandbox--literal-clauses
                      "allow file-read*" f)
                     "\n"
                     (scalpel-sandbox--literal-clauses
                      "allow file-write*" f)))
           writable "\n")
          "\n")
       "")
     "\n")))

(defun scalpel-sandbox--macos-argv (command writable-files readonly-files)
  "Build `sandbox-exec' ARGV for COMMAND on macOS.
WRITABLE-FILES and READONLY-FILES are lists of context files."
  (list scalpel-sandbox-macos-program
        "-p"
        (scalpel-sandbox--macos-profile writable-files readonly-files)
        "/bin/sh" "-c" command))

(defun scalpel-sandbox-supported-p ()
  "Return non-nil when a sandbox backend is available for this system."
  (cond
   ((eq system-type 'gnu/linux)
    (and (executable-find scalpel-sandbox-program) t))
   ((eq system-type 'darwin)
    (and (executable-find scalpel-sandbox-macos-program) t))
   (t nil)))

(defun scalpel-sandbox--sandbox-argv-p (argv)
  "Return non-nil when ARGV is a configured sandbox invocation.
ARGV must start with `scalpel-sandbox-program' or
`scalpel-sandbox-macos-program' and end with the shell wrapper
\"/bin/sh\" \"-c\" COMMAND.  This invariant is what keeps a shell
command from ever reaching the operating system unserialized."
  (and (consp argv)
       (cl-member (car argv)
                  (list scalpel-sandbox-program scalpel-sandbox-macos-program)
                  :test #'string=)
       (let ((n (length argv)))
         (and (>= n 4)
              (equal (nth (- n 3) argv) "/bin/sh")
              (equal (nth (- n 2) argv) "-c")))))

(defun scalpel-sandbox--workdir ()
  "Return the working directory for the sandboxed child.
macOS `sandbox-exec' has no `--chdir' equivalent, so the child
inherits the caller's `default-directory'.  When that directory is
not readable under the profile, `/bin/sh' cannot resolve its own
cwd and writes a getcwd warning to stderr; because `call-process'
captures stderr together with stdout, that warning lands in the
captured output and the preflight sentinel comparison fails.
A context file's parent cannot serve as the cwd: its directory
read is deliberately not granted, because that would expose every
sibling file.  The temporary directory is the one directory both
profiles grant, so it is the only cwd the child can always
resolve.  The result is canonicalized, because the sandbox
resolves symlinks and would not match an allow rule written for
the unresolved name."
  (file-name-as-directory
   (file-truename (expand-file-name temporary-file-directory))))

(defun scalpel-sandbox--invoke (command writable-files readonly-files)
  "Run COMMAND through this system's sandbox; return (EXIT . OUTPUT).
WRITABLE-FILES and READONLY-FILES are the context files to expose.
Signal `user-error' when no backend exists, when the built ARGV is
not a sandbox invocation, or when the sandbox runtime dies from a
signal: a command whose sandbox failed must never be reported as if
it had run."
  (let ((argv (cond
               ((eq system-type 'gnu/linux)
                (scalpel-sandbox--bwrap-argv
                 command writable-files readonly-files))
               ((eq system-type 'darwin)
                (scalpel-sandbox--macos-argv
                 command writable-files readonly-files))
               (t (scalpel-sandbox--fail
                   "Scalpel: no supported command sandbox for %s"
                   system-type)))))
    (unless (scalpel-sandbox--sandbox-argv-p argv)
      (scalpel-sandbox--fail
       "Scalpel: refusing to run a command outside the sandbox"))
    (with-temp-buffer
      (let* ((default-directory (scalpel-sandbox--workdir))
             (status (apply #'call-process (car argv) nil t nil (cdr argv))))
        ;; A string status is Emacs' report of a signal (for example
        ;; "Abort trap: 6"): the sandbox runtime died before the command
        ;; could run.  Refuse instead of handing back a result the caller
        ;; would mistake for command output.
        (unless (integerp status)
          (scalpel-sandbox--fail
           "Scalpel: sandbox failed (%S); refusing to run shell command outside the sandbox"
           status))
        (cons status (buffer-string))))))

(defun scalpel-sandbox--preflight ()
  "Prove the sandbox can run commands before any real command is sent.
Runs a sentinel-printing command through the same backend and policy
builder used for real commands.  Signal `user-error' when the probe
does not come back with exit 0 and the sentinel, so a broken sandbox
blocks shell actions instead of degrading them."
  (let ((probe-file (file-truename (make-temp-file "scalpel-sandbox-probe-"))))
    (unwind-protect
        (let* ((result (scalpel-sandbox--invoke
                        (format "printf %s > %s && cat %s"
                                scalpel-sandbox--probe-sentinel
                                (shell-quote-argument probe-file)
                                (shell-quote-argument probe-file))
                        (list probe-file)
                        nil))
               (output (string-trim (cdr result))))
          (unless (and (= (car result) 0)
                       (string= output scalpel-sandbox--probe-sentinel))
            (scalpel-sandbox--fail
             "Scalpel: sandbox probe failed (exit %s, output %S); refusing to run shell command"
             (car result) output)))
      (when (file-exists-p probe-file)
        (delete-file probe-file)))))

(defun scalpel-sandbox-run (command _root writable-files readonly-files)
  "Run COMMAND in a platform sandbox.
ROOT is intentionally ignored: access is defined only by the context files.
WRITABLE-FILES and READONLY-FILES are lists of context files.
Return (EXIT . OUTPUT).  Signal `user-error' when no backend exists,
when the sandbox probe fails, or when the sandbox runtime dies: a
command that cannot be proven to run inside the sandbox is refused
rather than run outside it."
  (unless (scalpel-sandbox-supported-p)
    (scalpel-sandbox--fail
     "Scalpel: no supported command sandbox for %s" system-type))
  (scalpel-sandbox--preflight)
  (scalpel-sandbox--invoke command writable-files readonly-files))

(provide 'scalpel-sandbox)

;;; scalpel-sandbox.el ends here
