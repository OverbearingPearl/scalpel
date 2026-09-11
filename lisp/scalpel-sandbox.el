;;; scalpel-sandbox.el --- Linux command sandbox for Scalpel -*- lexical-binding: t; -*-

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

(defun scalpel-sandbox--file-paths (writable-files readonly-files)
  "Return normalized context files from WRITABLE-FILES and READONLY-FILES.
Reject missing files, directories, symlinks, and overlapping permissions."
  (let ((all (append writable-files readonly-files))
        (writable (delete-dups (mapcar #'expand-file-name writable-files)))
        (readonly (delete-dups (mapcar #'expand-file-name readonly-files))))
    (when (null all)
      (user-error "Scalpel: shell sandbox requires at least one context file"))
    (dolist (file all)
      (unless (file-regular-p file)
        (user-error "Scalpel: sandbox context is not a regular file: %s" file))
      (when (file-symlink-p file)
        (user-error "Scalpel: sandbox rejects symlink context file: %s" file)))
    (when (cl-intersection writable readonly :test #'string=)
      (user-error "Scalpel: sandbox context has conflicting file permissions"))
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

(defun scalpel-sandbox--path-literal (file)
  "Return a sandbox profile literal clause for FILE."
  (format "(literal %S)" (expand-file-name file)))

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
     "(allow file-read* (subpath \"/\"))\n"
     "(allow file-read* (literal \"/dev/null\"))\n"
     "(allow file-write* (literal \"/dev/null\"))\n"
     "(allow file-write* (subpath \"/private/tmp\"))\n"
     "(allow file-write* (subpath \"/var/folders\"))\n"
     (if readonly
         (concat (mapconcat
                  (lambda (f) (format "(allow file-read* %s)"
                                      (scalpel-sandbox--path-literal f)))
                  readonly "\n")
                 "\n")
       "")
     (if writable
         (mapconcat
          (lambda (f)
            (format "(allow file-read* %s)\n(allow file-write* %s)"
                    (scalpel-sandbox--path-literal f)
                    (scalpel-sandbox--path-literal f)))
          writable "\n")
       "")
     ;; Allow directory traversal for context file parent directories,
     ;; so recursive commands like `grep -r` can list and read files
     ;; within those directories.
     (let ((dirs (delete-dups
                  (mapcar (lambda (f)
                            (directory-file-name
                             (file-name-directory (expand-file-name f))))
                          (append readonly writable)))))
       (mapconcat
        (lambda (dir)
          (format "(allow file-read* (subpath %S))\n" dir))
        dirs
        ""))
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

(defun scalpel-sandbox-run (command _root writable-files readonly-files)
  "Run COMMAND in a platform sandbox.
ROOT is intentionally ignored: access is defined only by the context files.
WRITABLE-FILES and READONLY-FILES are lists of context files.
Return (EXIT . OUTPUT), or signal `user-error' when unavailable."
  (unless (scalpel-sandbox-supported-p)
    (user-error "Scalpel: no supported command sandbox for %s" system-type))
  (let ((argv (cond
               ((eq system-type 'gnu/linux)
                (scalpel-sandbox--bwrap-argv
                 command writable-files readonly-files))
               ((eq system-type 'darwin)
                (scalpel-sandbox--macos-argv
                 command writable-files readonly-files))
               (t nil))))
    (with-temp-buffer
      (let ((status (apply #'call-process (car argv) nil t nil (cdr argv))))
        (cons status (buffer-string))))))

(provide 'scalpel-sandbox)

;;; scalpel-sandbox.el ends here
