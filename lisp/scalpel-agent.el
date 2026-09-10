;;; scalpel-agent.el --- LLM planning and action execution -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Provides context, structured-plan parsing, and action dispatch.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'scalpel-llm)
(require 'scalpel-locate)
(require 'scalpel-execute)

(defcustom scalpel-agent-system-prompt
  (if (boundp 'scalpel-system-prompt)
      scalpel-system-prompt
    "You are a precise code transformation tool. The user gives you
context and an instruction. Return ONLY a JSON array of actions.
The top-level response must be a JSON array, never a single object.
Each action is either {\"tool\":\"edit\",\"file\":\"/abs/path.el\",\"symbol\":\"name\",\"instruction\":\"...\"}
or {\"tool\":\"reply\",\"text\":\"...\"}. Never emit code or diff text in this response.
Files shown as \"FILE (READONLY)\" are references only: never emit an
edit action for them.")
  "System prompt for the Scalpel agent planner."
  :type 'string
  :group 'scalpel)

(defcustom scalpel-agent-context-readonly-max-bytes 20000
  "Maximum bytes of a read-only file included in the LLM context.
Larger files are truncated with an explicit marker."
  :type 'integer
  :group 'scalpel)

(defvar scalpel-agent--context-files nil
  "Writable files in the session context, as absolute names.")

(defvar scalpel-agent--context-readonly-files nil
  "Read-only reference files in the session context, as absolute names.")

(defun scalpel-agent--git-environment ()
  "Return `process-environment' with effectful git overrides removed.
Git exports GIT_DIR and friends to hooks; inheriting them makes
repository discovery succeed for directories that are not actually
inside a repository."
  (cl-remove-if
   (lambda (entry)
     (string-match-p
      "\\`GIT_\\(DIR\\|WORK_TREE\\|INDEX_FILE\\|OBJECT_DIRECTORY\\|ALTERNATE_OBJECT_DIRECTORIES\\|COMMON_DIR\\)="
      entry))
   process-environment))

(defun scalpel-agent--git-toplevel (dir)
  "Return the absolute git working-tree root containing DIR, or nil."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let ((default-directory dir)
            (process-environment (scalpel-agent--git-environment))
            (status (condition-case nil
                        (call-process "git" nil t nil
                                      "rev-parse" "--show-toplevel")
                      (error nil))))
        (when (and (numberp status) (= status 0) (> (buffer-size) 0))
          (let ((top (file-name-as-directory (string-trim (buffer-string)))))
            ;; Defensive: only accept TOP when DIR really is inside it.
            (when (file-in-directory-p dir top)
              top)))))))

(defun scalpel-agent--git-listed-files (dir)
  "Return files under DIR that git does not ignore.
Paths are absolute.  Return nil when DIR is not inside a git tree."
  (when (scalpel-agent--git-toplevel dir)
    (let ((default-directory (file-name-as-directory (expand-file-name dir)))
          (process-environment (scalpel-agent--git-environment)))
      (with-temp-buffer
        (let ((status (condition-case nil
                          (call-process "git" nil t nil
                                        "ls-files" "--cached" "--others"
                                        "--exclude-standard" "-z")
                        (error nil))))
          (when (and (numberp status) (= status 0))
            (mapcar (lambda (rel) (expand-file-name rel default-directory))
                    (split-string (buffer-string) "\0" t))))))))

(defun scalpel-agent--walk-all-files (dir)
  "Return every regular file at or below DIR, skipping `.git'."
  (let (out)
    (dolist (name (directory-files dir nil nil t))
      (let ((base (file-name-nondirectory (directory-file-name name))))
        (unless (member base '("." ".."))
          (let ((entry (expand-file-name base dir)))
            (cond
             ((and (file-directory-p entry) (not (string= base ".git")))
              (setq out (nconc out (scalpel-agent--walk-all-files entry))))
             ((file-regular-p entry)
              (push entry out)))))))
    (nreverse out)))

(defun scalpel-agent--expanded-files (path &optional ignore-gitignore require-locator)
  "Return the file list that PATH expands to.
PATH is a regular file or a directory.  Directory contents respect
gitignore unless IGNORE-GITIGNORE is non-nil.  When REQUIRE-LOCATOR
is non-nil, files without a registered provider are dropped."
  (let* ((path (expand-file-name path))
         (files
          (cond
           ((file-regular-p path) (list path))
           ((file-directory-p path)
            (cond
             (ignore-gitignore (scalpel-agent--walk-all-files path))
             ((scalpel-agent--git-toplevel path)
              (scalpel-agent--git-listed-files path))
             (t (scalpel-agent--walk-all-files path))))
           (t nil))))
    (if require-locator
        (cl-remove-if-not #'scalpel-locate-provider-for-file files)
      files)))

(defun scalpel-agent-context-reset ()
  "Clear the session context."
  (setq scalpel-agent--context-files nil)
  (setq scalpel-agent--context-readonly-files nil))

(defun scalpel-agent-context-add (path &optional ignore-gitignore)
  "Add PATH (a file or directory) as writable context.
A directory expands to files matching a registered locator, with
gitignored files excluded unless IGNORE-GITIGNORE is non-nil."
  (let ((files (scalpel-agent--expanded-files path ignore-gitignore t)))
    (unless files
      (user-error "Scalpel: no addable files under %s (try C-u to include gitignored)" path))
    (setq scalpel-agent--context-readonly-files
          (cl-set-difference scalpel-agent--context-readonly-files files
                             :test #'string=))
    (setq scalpel-agent--context-files
          (cl-union files scalpel-agent--context-files :test #'string=))
    files))

(defun scalpel-agent-context-add-readonly (path &optional ignore-gitignore)
  "Add PATH (a file or directory) as read-only reference.
Read-only files are shown to the LLM with their full contents and
cannot be edited.  Directory contents respect gitignore unless
IGNORE-GITIGNORE is non-nil."
  (let ((files (scalpel-agent--expanded-files path ignore-gitignore nil)))
    (unless files
      (user-error "Scalpel: no addable files under %s (try C-u to include gitignored)" path))
    (setq scalpel-agent--context-files
          (cl-set-difference scalpel-agent--context-files files
                             :test #'string=))
    (setq scalpel-agent--context-readonly-files
          (cl-union files scalpel-agent--context-readonly-files :test #'string=))
    files))

(defun scalpel-agent-readonly-p (file)
  "Return non-nil when FILE is a read-only context file."
  (and file
       (member (expand-file-name file) scalpel-agent--context-readonly-files)))

(defun scalpel-agent-context-remove (path)
  "Remove PATH from the session context.
PATH is a file or a directory; a directory removes every file under
it.  No-op with a message when nothing matched."
  (let* ((path (expand-file-name path))
         (prefix (file-name-as-directory path))
         (match-p (lambda (f) (or (string= f path) (string-prefix-p prefix f))))
         (removed (cl-remove-if-not match-p
                                    (append scalpel-agent--context-files
                                            scalpel-agent--context-readonly-files))))
    (if (null removed)
        (message "Scalpel: %s is not in the context" path)
      (setq scalpel-agent--context-files
            (cl-remove-if match-p scalpel-agent--context-files))
      (setq scalpel-agent--context-readonly-files
            (cl-remove-if match-p scalpel-agent--context-readonly-files))
      (message "Scalpel: removed %d file(s)" (length removed)))))

(defun scalpel-agent-context-summary ()
  "Return a one-line summary of the session context files."
  (let ((w scalpel-agent--context-files)
        (r scalpel-agent--context-readonly-files))
    (if (and (null w) (null r))
        "none"
      (string-join
       (append w (mapcar (lambda (f) (concat f " (read-only)")) r))
       ", "))))

(defun scalpel-agent--readonly-block (file)
  "Return the LLM context block for read-only FILE."
  (let* ((max scalpel-agent-context-readonly-max-bytes)
         (size (or (file-attribute-size (file-attributes file)) 0))
         (truncated (> size max))
         (body (with-temp-buffer
                 (insert-file-contents file nil 0 max)
                 (buffer-string))))
    (format "FILE (READONLY): %s\nCONTENT:\n%s%s"
            file
            (if truncated (format "[truncated at %d bytes]\n" max) "")
            body)))

(defun scalpel-agent-context ()
  "Return the LLM context text from the explicit session file lists."
  (let ((writable scalpel-agent--context-files)
        (readonly scalpel-agent--context-readonly-files))
    (if (and (null writable) (null readonly))
        "No files in context."
      (string-join
       (append
        (mapcar (lambda (file)
                  (format "FILE: %s\nSYMBOLS: %s"
                          file
                          (string-join (scalpel-locate-list-symbols file) ", ")))
                writable)
        (mapcar #'scalpel-agent--readonly-block readonly))
       "\n\n"))))

(defun scalpel-agent--strip-fences (raw)
  "Strip markdown code fences surrounding RAW, if present."
  (let ((text (string-trim raw)))
    (when (string-match-p "```" text)
      (setq text (string-trim
                  (replace-regexp-in-string
                   "```[a-zA-Z]*\n?\\|\n?```\\'" "" text))))
    text))

(defun scalpel-agent--parse-json (raw)
  "Parse RAW to a list of action plists.
Signal `user-error' when RAW is not valid JSON or not a JSON array
of objects."
  (let ((parsed (condition-case nil
                    (json-parse-string (scalpel-agent--strip-fences raw)
                                       :object-type 'plist
                                       :array-type 'list)
                  (error
                   (user-error "Scalpel: planner returned invalid JSON: %S"
                               raw)))))
    (when (and (plistp parsed) (plist-get parsed :tool))
      (setq parsed (list parsed)))
    (unless (and (listp parsed)
                 (cl-every (lambda (item) (plist-get item :tool)) parsed))
      (user-error
       (concat "Scalpel: planner returned unexpected structure "
               "(expected a JSON array of action objects): %S")
       raw))
    parsed))

(defun scalpel-agent-plan (instruction)
  "Ask the LLM for a structured plan for INSTRUCTION.
Return a list of plists with keys :tool :file :symbol :instruction :text."
  (let* ((prompt (format "%s\n\nUser instruction:\n%s"
                         (scalpel-agent-context) instruction))
         (raw (scalpel-llm-request prompt scalpel-agent-system-prompt))
         (actions (scalpel-agent--parse-json raw)))
    (mapcar
     (lambda (item)
       (list :tool (plist-get item :tool)
             :file (plist-get item :file)
             :symbol (plist-get item :symbol)
             :instruction (plist-get item :instruction)
             :text (plist-get item :text)))
     actions)))

(defun scalpel-agent--apply-if-unchanged (file symbol expected-body new-text)
  "Replace SYMBOL in FILE with NEW-TEXT only if EXPECTED-BODY is unchanged.
Return a human-readable report string.  Signal `user-error' if the target
region was modified while an LLM request was in flight."
  (with-current-buffer (find-file-noselect file)
    (let* ((current-range (scalpel-locate-range file symbol))
           (current-body (buffer-substring-no-properties
                          (car current-range) (cdr current-range))))
      (unless (string= expected-body current-body)
        (user-error
         (concat "Scalpel: target region changed while editing %s; "
                 "aborting.  Re-run after reviewing the buffer")
         symbol))
      (scalpel-execute-replace (car current-range) (cdr current-range) new-text)
      (format "Edited %s in %s" symbol (buffer-name (current-buffer))))))

(defun scalpel-agent--single-definition-p (text)
  "Return non-nil when TEXT is exactly one top-level defining form."
  (condition-case nil
      (let* ((parsed (read-from-string text))
             (form (car parsed))
             (end (cdr parsed)))
        (and (listp form)
             (memq (car form)
                   '(defun defmacro defvar defcustom defconst))
             (= end (length text))))
    (error nil)))

(defun scalpel-agent-edit (file symbol instruction)
  "Edit SYMBOL in FILE per INSTRUCTION using boundary-locked apply.
Return human-readable report string."
  (unless (and file symbol instruction)
    (user-error "Scalpel: malformed edit action"))
  (when (scalpel-agent-readonly-p file)
    (user-error "Scalpel: %s is a read-only context file" file))
  (let ((range (scalpel-locate-range file symbol)))
    (unless range
      (user-error "Scalpel: can't locate %s in %s" symbol file))
    (with-current-buffer (find-file-noselect file)
      (let* ((beg (car range))
             (end (cdr range))
             (body (buffer-substring-no-properties beg end))
             (signature (save-excursion
                          (goto-char beg)
                          (buffer-substring-no-properties
                           beg (line-end-position))))
             (prompt (concat "Signature: %s\n\nCurrent block:\n%s\n\n"
                             "Instruction: %s\n\n"
                             "Return only the full replacement definition, as plain "
                             "Emacs Lisp text. Do not include markdown fences or "
                             "explanations. If the requested change is impossible or "
                             "unnecessary for this block, return exactly: NO_CHANGE"))
             (new-text (string-trim (scalpel-llm-request
                                     (format prompt signature body instruction)))))
        (cond
         ((string= new-text "NO_CHANGE")
          (format "No change needed: %s in %s" symbol
                  (buffer-name (current-buffer))))
         ((scalpel-agent--single-definition-p new-text)
          (scalpel-agent--apply-if-unchanged
           file symbol body new-text))
         (t
          (user-error
           (concat "Scalpel: planner returned no usable replacement for %s. "
                   "Refusing to edit. Reply was: %S")
           symbol new-text)))))))

(defun scalpel-agent-execute-action (action)
  "Execute a single ACTION plist and return a report string."
  (let ((tool (plist-get action :tool)))
    (pcase tool
      ("edit"
       (scalpel-agent-edit
        (plist-get action :file)
        (plist-get action :symbol)
        (plist-get action :instruction)))
      ("reply"
       (format "%s" (or (plist-get action :text) "")))
      (_
       (format "Unknown action: %S" tool)))))

(defun scalpel-agent-run (instruction)
  "Run a full agent cycle for INSTRUCTION and return the combined report."
  (mapconcat #'scalpel-agent-execute-action
             (scalpel-agent-plan instruction)
             "\n"))

(provide 'scalpel-agent)

;;; scalpel-agent.el ends here
