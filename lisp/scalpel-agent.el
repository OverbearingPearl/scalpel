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

(defconst scalpel-agent--tool-vocabulary '("edit" "reply")
  "Tool names the planner may emit.
Structural contract, not user configuration: dispatch in
`scalpel-agent-execute-action' must stay in sync with it.")

(defconst scalpel-agent--action-fields
  '(:tool :file :symbol :instruction :text)
  "Fields carried through the action round-trip.
Structural contract consumed by `scalpel-agent-plan'; it is not
user configuration.")

(defconst scalpel-agent--no-change-sentinel "NO_CHANGE"
  "Literal the LLM returns when the requested edit is unnecessary.
Structural contract shared by the replacement prompt in
`scalpel-agent-edit' and its no-op check.")

(defcustom scalpel-agent-system-prompt
  "You are a precise code transformation tool. The user gives you
context and an instruction. Return ONLY a JSON array of actions.
The top-level response must be a JSON array, never a single object.
Each action is either {\"tool\":\"edit\",\"file\":\"/abs/path.el\",\"symbol\":\"name\",\"instruction\":\"...\"}
or {\"tool\":\"reply\",\"text\":\"...\"}. Never emit code or diff text in this response.
Files shown as \"FILE (READONLY)\" are references only: never emit an
edit action for them."
  "System prompt for the Scalpel agent planner.
This controls only the wording sent to the LLM; the action schema
is fixed by `scalpel-agent--action-fields' and
`scalpel-agent--tool-vocabulary' and must not be overridden here."
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
  "Return the absolute git working-tree root containing DIR, or nil.
DIR may be the repository root itself, so containment must not be
tested by path prefix: a directory is always inside the root that
git reports for it."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      ;; A plain `let' evaluates its value forms before installing the
      ;; bindings, so `call-process' below would run with the caller's
      ;; `default-directory' and unstripped environment.  `let*' binds
      ;; sequentially, making DIR and the sanitized env actually apply.
      (let* ((default-directory dir)
             (process-environment (scalpel-agent--git-environment))
             (status (condition-case nil
                         (call-process "git" nil t nil
                                       "rev-parse" "--show-toplevel")
                       (error nil))))
        (when (and (numberp status) (= status 0) (> (buffer-size) 0))
          (file-name-as-directory (string-trim (buffer-string))))))))

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
  "Return every regular file at or below DIR.
`.git' is skipped whether it is a directory (plain clone) or a
gitfile (worktree or submodule)."
  (let (out)
    (dolist (name (directory-files dir nil nil t))
      (let ((base (file-name-nondirectory (directory-file-name name))))
        (unless (or (member base '("." ".."))
                    (string= base ".git"))
          (let ((entry (expand-file-name base dir)))
            (cond
             ((file-directory-p entry)
              (setq out (nconc out (scalpel-agent--walk-all-files entry))))
             ((file-regular-p entry)
              (push entry out)))))))
    (nreverse out)))

(defun scalpel-agent--expanded-files (path &optional ignore-gitignore)
  "Return the file list that PATH expands to.
PATH is a regular file or a directory.  Directory contents respect
gitignore unless IGNORE-GITIGNORE is non-nil."
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
    files))

(defun scalpel-agent-context-reset ()
  "Clear the session context."
  (setq scalpel-agent--context-files nil)
  (setq scalpel-agent--context-readonly-files nil))

(defun scalpel-agent-context-add (path &optional ignore-gitignore)
  "Add PATH (a file or directory) as writable context.
A directory expands to files matching a registered locator, with
gitignored files excluded unless IGNORE-GITIGNORE is non-nil."
  (let ((files (scalpel-agent--expanded-files path ignore-gitignore)))
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
  (let ((files (scalpel-agent--expanded-files path ignore-gitignore)))
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

(defun scalpel-agent--path-components (path)
  "Split PATH into tree components for the context tree.
Split on \"/\" with empty components dropped.  Absolute paths keep
a root component: \"~\" for home-relative paths, \"/\" otherwise,
so the tree still shows nesting below the root."
  (let ((parts (split-string path "/" t)))
    (if (string-prefix-p "/" path)
        (cons "/" parts)
      parts)))

(defun scalpel-agent--context-entry-lessp (a b)
  "Return non-nil when context tree entry A should sort before B.
Entries are (NAME . NODE) pairs.  Directories sort before files;
names compare case-insensitively."
  (let ((fa (plist-get (cdr a) :file))
        (fb (plist-get (cdr b) :file)))
    (cond
     ((and fa (not fb)) nil)
     ((and fb (not fa)) t)
     (t (string< (downcase (car a)) (downcase (car b)))))))

(defun scalpel-agent--context-marker (node)
  "Return the attribute suffix for file NODE, e.g. \" (read-only)\"."
  (let ((tags (delq nil (list (and (plist-get node :readonly) "read-only")
                              (and (plist-get node :ignored) "gitignored")))))
    (if tags (format " (%s)" (string-join tags ", ")) "")))

(defun scalpel-agent--context-tree-insert (tree components flags)
  "Insert COMPONENTS into TREE, marking the leaf with FLAGS.
TREE is an alist of names to nodes; a node is either
\(:children ALIST) for a directory or a file node carrying :file
plus FLAGS.  Return the updated TREE."
  (if (null components)
      tree
    (let* ((name (car components))
           (rest (cdr components))
           (cell (assoc name tree)))
      (cond
       ((null cell)
        (if rest
            (cons (cons name
                        (list :children
                              (scalpel-agent--context-tree-insert
                               nil rest flags)))
                  tree)
          (cons (cons name (append (list :file t) flags)) tree)))
       (rest
        (setcdr cell (list :children
                           (scalpel-agent--context-tree-insert
                            (plist-get (cdr cell) :children) rest flags)))
        tree)
       (t tree)))))

(defun scalpel-agent--context-tree-compact (tree)
  "Collapse single-child directory chains in TREE into one node.
A directory with exactly one child that is itself a directory is
merged with it, joining names with \"/\".  This keeps shared path
prefixes (e.g. an external tree's \"~/Projects/elisp/...\") from
occupying one line per component."
  (let ((entries
         (mapcar
          (lambda (entry)
            (let ((node (cdr entry)))
              (if (plist-get node :file)
                  entry
                (let ((children (scalpel-agent--context-tree-compact
                                 (plist-get node :children))))
                  (if (and (= (length children) 1)
                           (not (plist-get (cdar children) :file)))
                      (cons (if (string-suffix-p "/" (car entry))
                                (concat (car entry) (caar children))
                              (concat (car entry) "/" (caar children)))
                            (cdr (car children)))
                    (cons (car entry)
                          (list :children children)))))))
          tree)))
    entries))

(defun scalpel-agent--context-tree-render (tree prefix)
  "Return TREE rendered as display cells prefixed with PREFIX.
Each cell is a plist with :text (the full line), :name-start (the
index at which the entry name begins, so callers can highlight the
name without touching the tree graphics) and :status (nil when the
node is unmarked)."
  (let* ((entries (sort (copy-sequence tree)
                        #'scalpel-agent--context-entry-lessp))
         (n (length entries)))
    (cl-loop
     for entry in entries
     for i from 1
     append
     (let* ((name (car entry))
            (node (cdr entry))
            (last-p (= i n))
            (connector (if last-p "└── " "├── "))
            (text (concat prefix connector name
                          (if (plist-get node :file)
                              (scalpel-agent--context-marker node)
                            (if (string-suffix-p "/" name) "" "/")))))
       (cons (list :text text
                   :name-start (+ (length prefix) (length connector))
                   :status (plist-get node :status))
             (unless (plist-get node :file)
               (scalpel-agent--context-tree-render
                (plist-get node :children)
                (concat prefix (if last-p "    " "│   ")))))))))

(defun scalpel-agent--git-ignored-in-repo (root files)
  "Return the members of FILES that git would ignore in repository ROOT.
FILES are absolute names at or below ROOT.  Return nil and log a
warning when git fails, so callers degrade to \"nothing ignored\"."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory (expand-file-name root)))
          (process-environment (scalpel-agent--git-environment)))
      (insert (string-join
               (mapcar (lambda (file) (file-relative-name file root)) files)
               "\n")
              "\n")
      ;; core.quotePath=false keeps non-ASCII names unquoted in the output.
      (let ((status (condition-case err
                        (call-process-region
                         (point-min) (point-max)
                         "git" t t nil
                         "-c" "core.quotePath=false"
                         "check-ignore" "--stdin")
                      (error (format "%S" err)))))
        (if (member status '(0 1))
            (mapcar (lambda (rel) (expand-file-name rel root))
                    (split-string (buffer-string) "\n" t))
          (message
           (concat "Scalpel: git check-ignore failed in %s (%S); "
                   "not marking gitignored files")
           root status)
          nil)))))

(defun scalpel-agent--git-ignored-files (files)
  "Return the subset of FILES that git would ignore.
FILES is a list of absolute file names.  Files without a `.git'
ancestor are never reported, so files outside any repository cost
no subprocess; each repository is queried once."
  (let (groups)
    (dolist (file files)
      (let ((root (locate-dominating-file file ".git")))
        (when root
          (let* ((root (file-name-as-directory (expand-file-name root)))
                 (cell (assoc root groups)))
            (if cell
                (setcdr cell (cons file (cdr cell)))
              (push (cons root (list file)) groups))))))
    (cl-loop for (root . members) in groups
             append (scalpel-agent--git-ignored-in-repo
                     root (nreverse members)))))

(defun scalpel-agent--context-entries (ignored-files)
  "Return one (FILE . FLAGS) entry per file in the session context.
IGNORED-FILES lists absolute names that git ignores; matching
entries get :ignored set.  FLAGS is the plist consumed by the tree
builder: :readonly and :ignored, plus :status once a caller has
annotated the entry."
  (let ((entry (lambda (file readonly)
                 (let ((file (expand-file-name file)))
                   (cons file
                         (list :readonly readonly
                               :ignored (and (member file ignored-files) t)))))))
    (append
     (mapcar (lambda (file) (funcall entry file nil))
             scalpel-agent--context-files)
     (mapcar (lambda (file) (funcall entry file t))
             scalpel-agent--context-readonly-files))))

(defun scalpel-agent--context-tree-mark-dirs (tree)
  "Mark directory nodes of TREE from their descendants' :status.
A directory takes the common status of its descendants when they
all agree (`added', `removed' or `same'); mixed subtrees stay
unmarked.  Return the updated TREE."
  (mapcar
   (lambda (entry)
     (let ((node (cdr entry)))
       (if (plist-get node :file)
           entry
         (let* ((children (scalpel-agent--context-tree-mark-dirs
                           (plist-get node :children)))
                (statuses (delq nil
                                (mapcar (lambda (child)
                                          (plist-get (cdr child) :status))
                                        children)))
                (status (cond
                         ((and statuses
                               (cl-every (lambda (s) (eq s 'added)) statuses))
                          'added)
                         ((and statuses
                               (cl-every (lambda (s) (eq s 'removed))
                                         statuses))
                          'removed)
                         ((and statuses
                               (cl-every (lambda (s) (eq s 'same)) statuses))
                          'same))))
           (cons (car entry)
                 (list :children children :status status))))))
   tree))

(defun scalpel-agent--context-tree-lines (entries)
  "Return display cells for ENTRIES.
ENTRIES is a list of (FILE . FLAGS) whose :status was set by the
caller.  Return a list of cell plists in display order, as built
by `scalpel-agent--context-tree-render'."
  (let ((tree nil))
    (dolist (entry entries)
      (setq tree (scalpel-agent--context-tree-insert
                  tree
                  (scalpel-agent--path-components (car entry))
                  (cdr entry))))
    (scalpel-agent--context-tree-render
     (scalpel-agent--context-tree-mark-dirs
      (scalpel-agent--context-tree-compact tree))
     "")))

(defun scalpel-agent--context-tree-from-entries (entries)
  "Render ENTRIES, a list of (FILE . FLAGS), as a tree string."
  (string-join (mapcar (lambda (cell) (plist-get cell :text))
                       (scalpel-agent--context-tree-lines entries))
               "\n"))

(defun scalpel-agent-context-update (previous ignored-files)
  "Return (LINES . BASELINE) describing the current session context.
LINES is a list of display cells as built by
`scalpel-agent--context-tree-lines'; BASELINE is the value to pass
as PREVIOUS on the next call.  IGNORED-FILES lists absolute names
that git ignores.  PREVIOUS is a BASELINE from an earlier call, or
any non-list value (e.g. a symbol) when no baseline exists yet, in
which case nothing is marked as changed.  Files that PREVIOUS held
but the context no longer does are carried into LINES with status
`removed', so a caller can render them struck through at their
tree position."
  (let ((current (scalpel-agent--context-entries ignored-files)))
    (if (not (listp previous))
        (cons (scalpel-agent--context-tree-lines
               (mapcar (lambda (entry)
                         (cons (car entry)
                               (plist-put (copy-sequence (cdr entry))
                                          :status 'same)))
                       current))
              current)
      (let* ((previous-files (mapcar #'car previous))
             (current-files (mapcar #'car current))
             (tagged (mapcar (lambda (entry)
                               (cons (car entry)
                                     (plist-put
                                      (copy-sequence (cdr entry))
                                      :status
                                      (if (member (car entry) previous-files)
                                          'same
                                        'added))))
                             current))
             (dropped (mapcar (lambda (entry)
                                (cons (car entry)
                                      (plist-put (copy-sequence (cdr entry))
                                                 :status 'removed)))
                              (cl-remove-if
                               (lambda (entry)
                                 (member (car entry) current-files))
                               previous))))
        (cons (scalpel-agent--context-tree-lines (append tagged dropped))
              current)))))

(defun scalpel-agent-context-summary (&optional _root ignored-files)
  "Return the session context as a tree of files.
Paths are always rendered in full from the filesystem root, so
files inside and outside the project share a single tree instead
of being split into separate ones.  IGNORED-FILES lists absolute
names that git ignores, marked \"(gitignored)\"; read-only entries
are marked \"(read-only)\".  Return the string \"none\" when the
context is empty."
  (let ((entries (scalpel-agent--context-entries ignored-files)))
    (if (null entries)
        "none"
      (scalpel-agent--context-tree-from-entries entries))))

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
                  (if (scalpel-locate-provider-for-file file)
                      (format "FILE: %s\nSYMBOLS: %s"
                              file
                              (string-join (scalpel-locate-list-symbols file) ", "))
                    (format "FILE: %s" file)))
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
       (cl-loop for key in scalpel-agent--action-fields
                append (list key (plist-get item key))))
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
         ((string= new-text scalpel-agent--no-change-sentinel)
          (format "No change needed: %s in %s" symbol
                  (buffer-name (current-buffer))))
         ((scalpel-locate-single-definition-p file new-text)
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
    (unless (member tool scalpel-agent--tool-vocabulary)
      (user-error "Scalpel: unknown action tool %S" tool))
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
