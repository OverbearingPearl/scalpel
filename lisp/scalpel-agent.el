;;; scalpel-agent.el --- LLM planning and action execution -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Provides context, structured-plan parsing, and action dispatch.
;;
;; The context is a list of files, and it is this module's own state: the
;; file-level tools keep it true to the disk, so a created file joins it, a
;; renamed file's entry moves with it, and a deleted file's entry goes.  The
;; console draws that list, so it redraws whenever a round changes it.
;;
;; Dispatch applies each action as it is parsed: edits are not queued for
;; approval, since `scalpel-execute' pins each replacement to a verified range.
;; Every action returns a human-readable report instead, which is the trace the
;; console keeps -- applying without asking is only defensible because the
;; result is reported.  The only prompt this module raises is for a long-running
;; shell command, whose cost is the frozen editor the user cannot work in while
;; it runs.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'scalpel-llm)
(require 'scalpel-llm-dialect)
(require 'scalpel-llm-deepseek)
(require 'scalpel-llm-laguna)
(require 'scalpel-locate)
(require 'scalpel-execute)
(require 'scalpel-sandbox)

(defconst scalpel-agent--tool-vocabulary '("file-peek" "block-edit" "block-insert" "block-delete" "file-create" "file-rename" "file-delete" "file-substitute" "shell" "reply" "confirm")
  "Tool names the planner may emit.
Structural contract, not user configuration: dispatch in
`scalpel-agent-execute-action' must stay in sync with it.")

(require 'scalpel-prompt-elisp)

(require 'scalpel-prompt)

(defconst scalpel-agent--tool-fields
  '(("file-peek" . (:tool :file))
    ("block-edit" . (:tool :file :symbol :instruction))
    ("block-insert" . (:tool :file :symbol :instruction :after))
    ("block-delete" . (:tool :file :symbol))
    ("file-create" . (:tool :file :text))
    ("file-rename" . (:tool :file :to))
    ("file-delete" . (:tool :file))
    ("file-substitute" . (:tool :files :pattern :replacement))
    ("shell" . (:tool :command :reason :long-running))
    ("reply" . (:tool :text))
    ("confirm" . (:tool :text)))
  "Per-tool field contracts.
Each entry is (TOOL . FIELDS).  `scalpel-agent-plan' validates
each parsed action against its tool's field list, so a missing or
extra field fails loudly instead of silently degrading.")

(defconst scalpel-agent--tool-optional-fields
  '(("file-peek" . (:symbol))
    ("file-substitute" . (:reason)))
  "Fields a tool accepts but does not require.
Each entry is (TOOL . FIELDS), matching the shape of
`scalpel-agent--tool-fields'.  `scalpel-agent--validate-action'
checks the required list only, while `scalpel-agent--project-actions'
keeps required and optional fields alike; a field listed here may
therefore be omitted by the planner and still survive projection.")

(defconst scalpel-agent--change-tools
  '("block-edit" "block-insert" "block-delete" "file-create" "file-rename"
    "file-delete" "file-substitute")
  "Tools whose action changes the files on disk.
Structural contract shared by `scalpel-agent-run', which records one
entry per such action in the round's :changes, and
`scalpel-console--run-rounds', which continues the loop while a round
has changes to read back.  Every writing tool belongs here: a round
that wrote something is a round whose report the planner still has to
act on, and a caller that stopped after it would abandon the rest of a
request the planner split across rounds -- three files created one per
round is the same shape as three edits.  Reading tools stay out: their
output is already the round's report, which reaches the caller through
the shell and read lists.  A new writing tool belongs in this list, or
loop ends after it without ever telling the planner what landed.")

(defconst scalpel-agent--no-change-sentinel "NO_CHANGE"
  "Literal the LLM returns when the requested edit is unnecessary.
Structural contract shared by the replacement prompt in
`scalpel-agent-block-edit' and its no-op check.")

(defcustom scalpel-agent-cod-enabled nil
  "Non-nil means append `scalpel-prompt-cod-prompt' to the system prompt."
  :type 'boolean
  :group 'scalpel)

(defcustom scalpel-agent-shell-max-bytes 20000
  "Maximum bytes of shell command output included in the LLM context.
Larger outputs are truncated with an explicit marker."
  :type 'integer
  :group 'scalpel)

(defcustom scalpel-agent-file-read-max-bytes 40000
  "Maximum bytes a read action may put into the LLM context.
This budget is separate from `scalpel-agent-shell-max-bytes':
a shell report is an inspection whose size the planner does not
control, while a read is a retrieval the planner asked for by
name.  A whole-file read is truncated at this limit with a marker
stating the true size.  A single definition is never truncated:
one that exceeds the limit is refused outright, because a partial
definition can still parse as a complete form and be applied
silently."
  :type 'integer
  :group 'scalpel)

(defcustom scalpel-agent-context-max-files 200
  "Maximum number of files the session context may hold.
Each context file costs a line in the planner prompt, a read-only
bind in the Linux sandbox policy, and read grants in the macOS
profile, so adding a directory costs whatever lies under it.  An add
that would cross this limit is refused whole, with the context left
exactly as it was, rather than accepted in part: a partially
applied file list would make the sandbox's reach differ from what
the user asked for, and nothing downstream could tell that it had.
Raise it deliberately, or add a subdirectory instead."
  :type 'integer
  :group 'scalpel)

(defconst scalpel-agent--file-level-tools '("file-rename" "file-delete")
  "Tools that decide which files exist.
Their confirmation is not waivable: the boundary lock cannot
predict a file-level action's reach, so the prompt must survive
any setting of `scalpel-agent-confirm-tools'.  `file-create' is
deliberately absent: the planner delivers a finished whole-file
draft and the report names the path, so the creation is visible
without a prompt.")

(defcustom scalpel-agent-confirm-tools '("shell")
  "Tools that require user confirmation before execution.
Each entry is a tool name string.  When the planner emits an action
whose :tool is in this list, the user is prompted to confirm before
the action is executed.  This is a safety gate for effectful tools
that operate outside the boundary lock.
A shell action the planner did not flag as long-running skips the
prompt: the sandbox already bounds what a command may touch, so
only the editor-freezing case needs an answer.  Removing \"shell\"
from this list disables the prompt for every shell action,
including a long-running one.
The file-level tools (`file-rename', `file-delete') are confirmed
regardless of this list: a file-level action changes which files
exist rather than bytes inside a file, so the boundary lock cannot
predict its reach and the user must always approve it."
  :type '(repeat string)
  :group 'scalpel)

(defvar scalpel-agent-unattended-confirm nil
  "When non-nil, every confirmable action runs without asking.
Set by an unattended console run and cleared when it settles, so
shell, file-rename, file-delete and file-substitute all proceed
while the user is away.  The sandbox still bounds what a shell
command may touch, and every executed action is reported in the
console record, so the user reviews the transcript afterwards
instead of answering prompts during the run.")

(defvar-local scalpel-agent--context-files nil
  "Files in this buffer's session context, as absolute names.
Buffer-local: each console buffer is one session, so two console
sessions -- two worktrees, say -- keep separate file lists.  Code
that touches this must run in the console buffer; the agent's
callbacks do, because `scalpel-agent-run' re-selects the buffer it
was called in.")

(defvar-local scalpel-agent--shell-output nil
  "Metadata plist for the most recent shell action in this session.
Set by `scalpel-agent-shell' right after the command runs and read
by `scalpel-agent-run' before the next action.  Keys: :bytes is the
raw output size; :truncated is non-nil when the report holds only a
prefix of it; :binary is non-nil when the raw output held a NUL byte.
Nil until a shell action runs.  Buffer-local, like the context
file list.")

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
  (let* ((path (file-truename (expand-file-name path)))
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
  (setq scalpel-agent--context-files nil))

(defun scalpel-agent-context-add (path &optional ignore-gitignore)
  "Add PATH (a file or directory) to the session context.
A directory expands to every regular file under it, whether or not
a locator handles it, so a log or a config that is only meant to be
read can be added alongside code.  Gitignored files are excluded
unless IGNORE-GITIGNORE is non-nil.  Signal `user-error' when PATH
expands to no file, or when the result would hold more than
`scalpel-agent-context-max-files' files; the refusal is whole, so
the context is left exactly as it was.  Return the files PATH
expanded to."
  (let ((files (scalpel-agent--expanded-files path ignore-gitignore)))
    (unless files
      (user-error "Scalpel: no addable files under %s (try C-u to include gitignored)" path))
    ;; The limit is checked on the merged list, not on FILES: several
    ;; adds that each fit must not be able to walk the context past it.
    (let ((merged (cl-union files scalpel-agent--context-files
                            :test #'string=)))
      (when (> (length merged) scalpel-agent-context-max-files)
        (user-error
         (concat "Scalpel: adding %s would put %d files in the context, "
                 "over the limit of %d; add a subdirectory, or raise "
                 "`scalpel-agent-context-max-files'")
         path (length merged) scalpel-agent-context-max-files))
      ;; Only after the check: a refused add must leave the context
      ;; untouched, not partially applied.
      (setq scalpel-agent--context-files merged))
    files))

(defun scalpel-agent-context-remove (path)
  "Remove PATH from the session context.
PATH is a file or a directory; a directory removes every file under
it.  No-op with a message when nothing matched."
  (let* ((path (file-truename (expand-file-name path)))
         (prefix (file-name-as-directory path))
         (match-p (lambda (f) (or (string= f path) (string-prefix-p prefix f))))
         (removed (cl-remove-if-not match-p scalpel-agent--context-files)))
    (if (null removed)
        (message "Scalpel: %s is not in the context" path)
      (setq scalpel-agent--context-files
            (cl-remove-if match-p scalpel-agent--context-files))
      (message "Scalpel: removed %d file(s)" (length removed)))))

(defun scalpel-agent--context-track (file)
  "Add FILE to the session context, as a file-level tool's own change.
Return nil when FILE was added or the context already held it, and a
report suffix when `scalpel-agent-context-max-files' refused it.
FILE enters the context as its truename, the spelling
`scalpel-agent-context-add' stores, so both ways in agree on identity:
two spellings of one path must not put one file in the context twice.

A refusal is a note rather than the `user-error'
`scalpel-agent-context-add' signals, because the tool that produced
FILE has already run and a create reported as a failure would be a lie.
What must not happen is silence: FILE exists on disk, so a limit that
keeps it out of the context keeps it out of the planner's reach, and
the note names the file and the limit that did it."
  (let ((resolved (file-truename (expand-file-name file))))
    (cond
     ((member resolved scalpel-agent--context-files) nil)
     ((< (length scalpel-agent--context-files)
         scalpel-agent-context-max-files)
      (setq scalpel-agent--context-files
            (cons resolved scalpel-agent--context-files))
      nil)
     (t
      (format (concat "\nNote: %s was not added to the context; it already "
                      "holds %d file(s), its limit "
                      "`scalpel-agent-context-max-files'")
              resolved (length scalpel-agent--context-files))))))

(defun scalpel-agent--context-untrack (file)
  "Drop FILE from the session context when it is there.
FILE is matched by its truename, the spelling the context stores, so a
file the session does not hold is a no-op.  Only the exact entry goes:
unlike `scalpel-agent-context-remove', a directory's contents are not
implied, because the caller acts on one file whose identity it has
already resolved."
  (let ((resolved (file-truename (expand-file-name file))))
    (setq scalpel-agent--context-files
          (cl-remove-if (lambda (entry) (string= entry resolved))
                        scalpel-agent--context-files))))

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
  "Return the attribute suffix for file NODE, e.g. \" (gitignored)\"."
  (if (plist-get node :ignored) " (gitignored)" ""))

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
builder: :ignored, plus :status once a caller has annotated the
entry."
  (mapcar
   (lambda (file)
     (let ((file (expand-file-name file)))
       (cons file
             (list :ignored (and (member file ignored-files) t)))))
   scalpel-agent--context-files))

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
names that git ignores, marked \"(gitignored)\".  Return the string
\"none\" when the context is empty."
  (let ((entries (scalpel-agent--context-entries ignored-files)))
    (if (null entries)
        "none"
      (scalpel-agent--context-tree-from-entries entries))))

(defun scalpel-agent-context ()
  "Return the LLM context text from the explicit session file list."
  (if (null scalpel-agent--context-files)
      "No files in context."
    (string-join
     (mapcar (lambda (file)
               (if (scalpel-locate-provider-for-file file)
                   (format "FILE: %s\nSYMBOLS: %s"
                           file
                           (string-join (scalpel-locate-list-symbols file) ", "))
                 (format "FILE: %s" file)))
             scalpel-agent--context-files)
     "\n\n")))

(defun scalpel-agent--project-actions (actions)
  "Project parsed ACTIONS plists onto each tool's field contract.
Return a new list of plists holding only the fields declared for
each action's tool -- required and optional alike -- so the
planner's extra keys never reach dispatch."
  (mapcar
   (lambda (item)
     (let* ((tool (plist-get item :tool))
            (fields (append
                     (cdr (assoc tool scalpel-agent--tool-fields))
                     (cdr (assoc tool scalpel-agent--tool-optional-fields)))))
       (cl-loop for key in fields
                append (list key (plist-get item key)))))
   actions))

(defun scalpel-agent--validate-action (action)
  "Validate ACTION plist against its tool's field contract.
Signal `user-error' when the tool is unknown or a required field
is missing."
  (let* ((tool (plist-get action :tool))
         (fields (cdr (assoc tool scalpel-agent--tool-fields))))
    (unless fields
      (user-error "Scalpel: unknown action tool %S" tool))
    (dolist (field fields)
      (unless (plist-get action field)
        (user-error "Scalpel: action %s missing required field %s" tool field)))
    action))

(defun scalpel-agent--prompt (instruction history)
  "Return the LLM prompt for INSTRUCTION given HISTORY.
HISTORY is the conversation text recorded before INSTRUCTION, or
nil on the first turn.  History goes before the instruction so the
instruction stays the last thing the LLM reads.  The agent holds no
state of its own: everything the LLM may rely on arrives here."
  ;; Outbound redaction: the assembled prompt (context, history and
  ;; instruction) is passed through scalpel-redact-apply so secrets are
  ;; scrubbed before anything leaves the agent.
  (scalpel-redact-apply
   (concat (scalpel-agent-context)
           "\n\n"
           (when (and history (not (string-empty-p history)))
             (format "Conversation so far:\n%s\n\n" history))
           "User instruction:\n"
           instruction)))

(defconst scalpel-agent--dialect-error-types
  '((scalpel-llm-dialect-tool-call-error . tool-call)
    (scalpel-llm-dialect-prose-reply-error . prose))
  "Reply-dialect conditions, and the planner error type each becomes.
Both conditions carry their whole message as their single data
element, so it is read through
`scalpel-llm-dialect-error-message' rather than
`error-message-string'.  The types stay apart because they degrade
differently: a prose reply is delivered as a reply action, while a
tool-call reply stays an error the console advises on.")

(defun scalpel-agent-plan (instruction history on-success on-error)
  "Ask the LLM for a structured plan for INSTRUCTION, without blocking.
HISTORY is the conversation text recorded before INSTRUCTION, or
nil.  ON-SUCCESS receives the projected action list.  ON-ERROR
receives a plist (:type SYMBOL :message STRING): `parse' when the
reply does not yield a valid action array, `tool-call' when it
answered in another convention, `prose' when the reply is pure
prose -- the conditions `scalpel-agent--dialect-error-types' names,
whose messages are read through `scalpel-llm-dialect-error-message'
-- otherwise the type forwarded by `scalpel-llm-request-async'.
Forwarding a prose reply to ON-ERROR instead of delivering it keeps
the loop honest: the console can self-heal-retry it like any other
planner error and only reports failure once the retry budget is
spent, instead of the round looking complete and ending silently.
Only the parse step is guarded, so an error raised inside
ON-SUCCESS escapes to the caller rather than being re-framed as a
planner error."
  (scalpel-llm-request-async
   (scalpel-agent--prompt instruction history)
   (lambda (raw)
     (let ((parsed (condition-case err
                       (cons t (scalpel-agent--project-actions
                                (mapcar #'scalpel-agent--validate-action
                                        (scalpel-llm-dialect-parse raw))))
                     (error
                      (let* ((dialect (assq (car err)
                                            scalpel-agent--dialect-error-types))
                             ;; A dialect condition carries its whole
                             ;; message as its data, so it is read
                             ;; verbatim: `error-message-string' would
                             ;; prefix the condition's class sentence and
                             ;; re-escape the reply.
                             (text (if dialect
                                       (scalpel-llm-dialect-error-message err)
                                     (error-message-string err))))
                        (funcall on-error
                                 (list :type (or (cdr dialect) 'parse)
                                       :message text))
                        nil)))))
       (when parsed
         (funcall on-success (cdr parsed)))))
   on-error
   (if scalpel-agent-cod-enabled
       (concat scalpel-prompt-system-prompt "\n\n" scalpel-prompt-cod-prompt)
     scalpel-prompt-system-prompt)))

(defun scalpel-agent--locate-candidates (symbol)
  "Return one (FILE . RANGE) per context file that define SYMBOL.
Files without a locator provider, and files where SYMBOL is absent,
are skipped: `scalpel-locate-range' signals rather than returning
nil, so every lookup runs inside its own guard."
  (let (hits)
    (dolist (file scalpel-agent--context-files)
      (let ((hit (condition-case nil
                     (cons file (scalpel-locate-range file symbol))
                   (error nil))))
        (when hit (push hit hits))))
    (nreverse hits)))

(defun scalpel-agent--symbol-skeleton (name)
  "Return NAME with its separator characters removed.
`llm-pick-view-cache-dir' and `llm-pick-view--cache-dir' come back
as one string, which is what makes the difference between them
visible: the names this failure kept turning on were written with the
wrong number of hyphens, and a comparison that kept the hyphens could
only report the prefix every name of that file shares -- never the
name the planner was aiming at.

Only `-' and `_' are removed, the two characters that separate the
words of a name.  A name differing in a letter keeps its difference:
the comparison catches a separator, it does not guess at a typo."
  (replace-regexp-in-string "[-_]" "" name))

(defconst scalpel-agent--resolve-hint-max-symbols 20
  "Maximum number of definitions a zero-hit refusal names.
The list is evidence about what the locator can read, not a file
listing; a longer dump would cost the planner tokens on every
prompt the refusal is part of.")

(defun scalpel-agent--resolve-hint (file symbol)
  "Return a report line describing what the locator can read in FILE.
FILE is the file SYMBOL was asked for and not found in.  The line
separates the causes a bare \"not found\" merges: the file is in the
context and carries no definition the locator recognises, the locator
cannot read it at all, or none of the definitions it does read is
SYMBOL -- which is what a symbol written with a form the locator does
not know looks like, and what nobody can see without the list.
Return the empty string when FILE cannot be described.

A definition whose spelling starts with SYMBOL, or that SYMBOL starts
with, is named on a line of its own: \"asked for x, the file defines
x-groups\" is the shape this failure keeps taking, and it is decidable
by comparing names rather than by guessing at what was meant.

The comparison ignores `-' and `_', because the names this failure
kept turning on differ from the file's by separators alone: asked for
`llm-pick-view--cache-dir', the file held
`llm-pick-view-cache-dir', and a prefix comparison saw only the part
both spellings share.  A name that matches under that reading is
reported first and as the file writes it, since it is the spelling
the planner can actually use."
  (let ((provider (scalpel-locate-provider-for-file file)))
    (cond
     ((null provider)
      (format (concat "\nNote: no locator handles %s, so no definition in it "
                      "can be located or listed.")
              file))
     (t
      (let ((result (condition-case err
                        (cons t (scalpel-locate-list-symbols file))
                      (error (cons nil err)))))
        (cond
         ((null (car result))
          (format "\nNote: the locator cannot read %s: %s"
                  file (error-message-string (cdr result))))
         ((null (cdr result))
          (format (concat "\nNote: %s is in the context and the locator reads "
                          "no definition in it, so %s is either absent from "
                          "disk or written with a form the locator does not "
                          "know.")
                  file symbol))
         (t
          (let* ((symbols (cdr result))
                 (skeleton (scalpel-agent--symbol-skeleton symbol))
                 (same (cl-remove-if-not
                        (lambda (name)
                          (string= (scalpel-agent--symbol-skeleton name)
                                   skeleton))
                        symbols))
                 (near (cl-remove-if
                        (lambda (name) (member name same))
                        (cl-remove-if-not
                         (lambda (name)
                           (let ((other
                                  (scalpel-agent--symbol-skeleton name)))
                             (or (string-prefix-p skeleton other)
                                 (string-prefix-p other skeleton))))
                         symbols)))
                 (shown (cl-subseq symbols 0
                                   (min (length symbols)
                                        scalpel-agent--resolve-hint-max-symbols))))
            (concat
             (format (concat "\nNote: %s is in the context and defines %d "
                             "definition(s); %s is not one of them.")
                     file (length symbols) symbol)
             (when same
               (format (concat "\n  The file spells %s where %s was asked "
                               "for; only the separators differ, and a name "
                               "is taken literally.")
                       (string-join same ", ") symbol))
             ;; The line is printed only when it narrows the list.  Its
             ;; point is to make the gap visible without reading the
             ;; list; a file whose every name is spelled like the asked
             ;; one is a file the list below already describes, in the
             ;; same words, so the two lines carried the same names
             ;; twice -- and this message joins the conversation, so the
             ;; duplication was re-sent on every later round.  The
             ;; `llm-pick-fetch-get' refusal did exactly that, with nine
             ;; names on each of the two lines.
             (when (and near (< (length near) (length symbols)))
               (format "\n  Definitions spelled like %s: %s"
                       symbol (string-join near ", ")))
             (format "\n  It defines: %s%s"
                     (string-join shown ", ")
                     (if (< (length shown) (length symbols)) ", ..." "")))))))))))

(defun scalpel-agent--namesake-hint (file symbol)
  "Return hint lines for the context files named after SYMBOL.
FILE is the file that was asked; it is skipped, because its own hint
is already on the message.  A namesake is a context file whose base
name reads as SYMBOL -- `llm-pick-fetch-get.el' for the name
`llm-pick-fetch-get' -- which is where a planner naming a symbol after
the module that holds it is pointing, so what such a file really
defines is the fact the zero-hit refusal is otherwise missing: the
scan that found nothing walked every context file, and only the file
the planner asked for was described.  The comparison drops `-' and
`_', as `scalpel-agent--symbol-skeleton' does, so a file whose name
differs from the symbol by separators alone counts too.  Return the
empty string when the context holds no such file."
  (let ((skeleton (scalpel-agent--symbol-skeleton symbol))
        (asked (and file
                    (condition-case nil
                        (file-truename (expand-file-name file))
                      (error nil)))))
    (apply #'concat
           (cl-loop for candidate in scalpel-agent--context-files
                    for resolved = (file-truename (expand-file-name candidate))
                    when (and (not (and asked (string= asked resolved)))
                              (string= skeleton
                                       (scalpel-agent--symbol-skeleton
                                        (file-name-base resolved))))
                    collect (scalpel-agent--resolve-hint resolved symbol)))))

(defun scalpel-agent--resolve-symbol (file symbol)
  "Return (FILE . RANGE) for SYMBOL, falling back to a context search.
FILE is tried first.  A planner that hallucinated the path -- the
observed failure: a symbol named with a sibling file's name -- gets a
deterministic correction instead of a dead end.  When SYMBOL is
absent from FILE, every context file is scanned: a unique hit is
returned with the file it really lives in, and the caller reports the
correction; zero or several hits signal `user-error' naming the
facts, so the next round can act on them instead of guessing again.

A zero-hit refusal also states what the locator can read in FILE,
through `scalpel-agent--resolve-hint'.  The remedy such a message used
to name -- add that file to the context -- is false when the file is
already there, and a definition the locator does not read is invisible
without the list; the two cases have different remedies and must not
read alike.  The refusal describes the context files named after
SYMBOL as well, through `scalpel-agent--namesake-hint': the scan that
found nothing walked every file, and the module a symbol is named
after is where it usually lives."
  (let ((direct (condition-case nil
                    (cons file (scalpel-locate-range file symbol))
                  (error nil))))
    (or direct
        (let ((hits (scalpel-agent--locate-candidates symbol)))
          (pcase (length hits)
            (1
             (message "Scalpel: %s is not in %s; it is defined in %s"
                      symbol file (caar hits))
             (car hits))
            (0
             (user-error
              (concat "Scalpel: symbol %s not found in %s nor anywhere in "
                      "the context (%s)%s%s")
              symbol file
              (if scalpel-agent--context-files
                  (string-join scalpel-agent--context-files ", ")
                "the context is empty")
              (if (and file (scalpel-agent--context-file-p file))
                  ;; The file is already in the context, so the remedy
                  ;; this message used to name -- add its file -- is
                  ;; false; what the planner needs instead is what the
                  ;; locator really reads in it.
                  (scalpel-agent--resolve-hint file symbol)
                (format (concat "\nNote: %s is not in the context; ask the "
                                "user to add it with a confirm action.")
                        file))
              ;; The scan that found nothing walked every context file,
              ;; so the file named after SYMBOL is described too: that is
              ;; where the planner's name came from when the symbol is
              ;; not the spelling the file holds.
              (scalpel-agent--namesake-hint file symbol)))
            (_
             (user-error
              (concat "Scalpel: symbol %s is defined in several context "
                      "files (%s); name one of them explicitly")
              symbol
              (string-join (mapcar #'car hits) ", "))))))))

(defun scalpel-agent--locatable-p (file symbol)
  "Return non-nil when SYMBOL can be located in FILE.
The question is the one the next round asks: locate is how a symbol is
found again, so a definition the locator cannot see is one the planner
will report missing.  Asking it here answers while the action that
created SYMBOL is still on screen.  The locator signals `user-error'
for a file it has no provider for; that is a nil answer, not a failure
of this probe."
  (condition-case nil
      (and (scalpel-locate-range file symbol) t)
    (user-error nil)))

(defun scalpel-agent--verified-range (file symbol expected-body)
  "Return (BEG . END) of SYMBOL in FILE, verified as unchanged.
SYMBOL is re-located here, in the caller's current buffer, so a
buffer the user edited while an LLM request was in flight aborts
the edit instead of being overwritten.  EXPECTED-BODY is the
region text the caller last saw; signal `user-error' when the
region no longer holds it."
  (let* ((current-range (scalpel-locate-range file symbol))
         (current-body (buffer-substring-no-properties
                        (car current-range) (cdr current-range))))
    (unless (string= expected-body current-body)
      (user-error
       (concat "Scalpel: target region changed while editing %s; "
               "aborting.  Re-run after reviewing the buffer")
       symbol))
    current-range))

(defun scalpel-agent--strip-code-fence (text)
  "Return TEXT with a surrounding markdown code fence removed.
Replacement replies are plain text by contract, but models wrap
them in ```lang fences anyway; the fence is not part of the
definition and fails `scalpel-locate-single-definition-p'.  Only a
fence that wraps the whole trimmed text is stripped, so a fence
inside a larger reply is left for the single-definition check to
reject."
  (let ((trimmed (string-trim text)))
    (if (and (string-prefix-p "```" trimmed)
             (string-suffix-p "```" trimmed)
             (> (length trimmed) 6))
        (let* ((body (substring trimmed 3))
               ;; Drop an optional language tag on the opening line.
               (body (if (string-match "\\`[^\n]*\n" body)
                         (substring body (match-end 0))
                       body)))
          (string-trim (substring body 0
                                  (if (string-suffix-p "```" body)
                                      (- (length body) 3)
                                    (length body)))))
      trimmed)))

(defun scalpel-agent--replacement-name (text)
  "Return the name defined by the first top-level form in TEXT, or nil.
Only lisp-shaped text yields a name; other languages return nil and
the report keeps the original symbol.  This closes the loop on a
planner that renames while editing: the report is the only channel
that tells the next round the old symbol no longer exists."
  (condition-case nil
      (let ((form (car (read-from-string text))))
        (when (and (listp form)
                   (symbolp (car form))
                   ;; A definition needs a name and at least one body
                   ;; form; a bare "(defun foo)" is a fragment, not a
                   ;; rename, and must not report a new name.
                   (> (length form) 2)
                   (symbolp (cadr form)))
          (symbol-name (cadr form))))
    (error nil)))

(defun scalpel-agent--unusable-replacement-reason (file text &optional form-p)
  "Return a sentence saying why TEXT is not a usable replacement for FILE.
Structural contract shared by `scalpel-agent-block-edit' and
`scalpel-agent-block-insert', whose refusals both append it, and by the
tests that pin each cause.

FORM-P says which judgement refused TEXT.  Nil, the default, is the
one `block-edit' makes: the reply must read as one complete
definition, because it lands on a located definition's range and a
text that defines nothing would delete what it replaced.  Non-nil is
the one `block-insert' makes, where the reply need only be one
complete top-level form -- a definition, or a file's own registration
call.  The causes that are the same for both keep one wording.  The
ones that are not -- a reply holding no definition, a reply holding
several -- are worded for the judgement that refused it, so an
insertion is never told about a definition it was never meant to
hold.

The refusal had no reason of its own: `scalpel-agent--usable-replacement'
tries the whole reply and every line-bounded prefix and suffix of it, and
when none passes `scalpel-locate-single-definition-p' the caller knows
only that the reply was refused.  Nothing in that tells the planner what
to change, and the observed failure spent round after round re-sending a
defun whose one missing closing bracket nobody had named.

Only facts that decide the refusal are stated.  A reply the file's own
locator reads no definition in is answered first -- prose, a stray form,
or a definition written in a spelling the locator does not know -- so a
reply holding no definition is not explained by the structure of one it
never held.  What the file's own reader makes of the reply is asked
next, through that language's provider rather than through any language
this module knows: a text the reader cannot finish is the one case the
bracket answer can state a cause for, while a text it reads as several
complete forms has no bracket left open, and the bracket sentence would
name a defect that is not there -- a two-form balanced reply was
answered with it, which cost the round.  The definition count is left
for the case it decides, a reply holding several of them.  The reply
itself is quoted by the caller, so this names the cause and leaves the
text to be compared against it."
  (let* ((defs (scalpel-agent--definitions-in-text file text))
         ;; The count is the language provider's answer, never this
         ;; module's: a reply is read in the language of the file it
         ;; would land in, and only that language's provider knows how.
         ;; Nil says the count is unavailable -- either the reader
         ;; cannot finish the text or the provider counts nothing --
         ;; which is why the bracket answer below is asked too.
         (forms (scalpel-locate-form-count file text)))
    (cond
     ;; Nothing to read at all is answered before the walk: the walk asks
     ;; how a definition failed to read, and with no definition in the reply
     ;; there is nothing for that question to be about.  The order is also
     ;; what keeps the walk away from such a reply, which is where it hung
     ;; the editor.
     ((and (null defs) (not form-p))
      (concat "No definition could be read out of it: the locator reads "
              "none in it, or cannot list definitions in this file's "
              "language."))
     ;; A reply the reader cannot finish at all is answered before the
     ;; count, because there is no count to give for it.  The bracket
     ;; question -- the language provider's, the same one
     ;; `scalpel-agent-file-substitute' asks of a rewritten text -- is
     ;; asked only here, and only a language that counts forms can reach
     ;; this line; only Emacs Lisp's provider does, so naming the Emacs
     ;; Lisp reader in the sentence below is a fact about the one
     ;; language that gets this far, not a guess about the file.  A
     ;; language with no bracket answer refuses with the general sentence
     ;; instead, rather than being handed a cause it never gave.
     ((and (null forms)
           (not (scalpel-locate-balanced-p file text)))
      (concat "Its brackets do not balance, so the Emacs Lisp reader "
              "cannot reach the end of it."))
     ;; A reply that reads as several complete forms, every one of them
     ;; finished, is refused for its count.  The bracket walk used to be
     ;; asked first, and answered such a reply -- two balanced forms,
     ;; nothing left open -- with the sentence above, naming a defect
     ;; that was not there and spending the round on it.
     ((and form-p forms (> forms 1))
      (concat "It is not exactly one complete top-level form for this "
              "file's language."))
     ((and (not form-p) (cdr defs))
      (format (concat "It reads as %d top-level definitions (%s), and a "
                      "replacement must be one, because it lands on one "
                      "resolved range.")
              (length defs) (string-join defs ", ")))
     (form-p
      (concat "It is not exactly one complete top-level form for this "
              "file's language."))
     (t
      (concat "It is not one complete definition for this file's "
              "language.")))))

(defun scalpel-agent--usable-replacement (file text)
  "Return the replacement TEXT usable for FILE, or TEXT itself.
A replacement reply is one definition by contract, but models pad
the definition with surrounding text: self-review prose after it,
or a whole-file echo -- headers, Commentary, requires -- before
it.  The fence is stripped first; if the whole text still fails
`scalpel-locate-single-definition-p', the longest line-bounded
prefix that passes is tried, then the longest suffix, which drops
trailing prose and leading file noise respectively while keeping
the definition whole.  When neither qualifies, TEXT is returned
unchanged so the caller's refusal path reports it."
  (let* ((stripped (scalpel-agent--strip-code-fence text))
         (lines (split-string stripped "\n"))
         (candidate (string-join lines "\n")))
    (if (scalpel-locate-single-definition-p file candidate)
        candidate
      ;; Longest prefix first: adding prose after a complete
      ;; definition breaks the single-definition check, so the first
      ;; passing prefix is the definition without the prose.
      (let ((found nil))
        (cl-loop for n from (length lines) downto 2
                 until found
                 do (let ((prefix (string-join (cl-subseq lines 0 n) "\n")))
                      (when (scalpel-locate-single-definition-p file prefix)
                        (setq found prefix))))
        (if found
            found
          ;; The definition can also sit at the end of the reply, under
          ;; whole-file noise: take the longest passing suffix.  Suffixes
          ;; are tried only after prefixes fail, so the existing
          ;; trailing-prose case keeps its answer.
          (cl-loop for n from 1 upto (- (length lines) 2)
                   until found
                   do (let ((suffix (string-join (cl-subseq lines n) "\n")))
                        (when (scalpel-locate-single-definition-p file suffix)
                          ;; The split keeps the reply's trailing newline
                          ;; as an empty final line; whitespace after the
                          ;; validated definition is padding, not code.
                          (setq found (string-trim-right suffix)))))
          (or found stripped))))))

(defun scalpel-agent--apply-if-unchanged (file symbol expected-body new-text)
  "Replace SYMBOL in FILE with NEW-TEXT only if EXPECTED-BODY is unchanged.
Return a human-readable report string.  Signal `user-error' if the target
region was modified while an LLM request was in flight."
  (with-current-buffer (find-file-noselect file)
    (let ((range (scalpel-agent--verified-range file symbol expected-body)))
      (scalpel-execute-replace (car range) (cdr range) new-text)
      (let ((renamed (scalpel-agent--replacement-name new-text)))
        (if (and renamed (not (string= renamed symbol)))
            (format "Edited %s in %s; the definition is now named %s"
                    symbol (buffer-name (current-buffer)) renamed)
          (format "Edited %s in %s" symbol (buffer-name (current-buffer))))))))

(defun scalpel-agent--create-after-anchor (file symbol after expected-body new-text)
  "Insert NEW-TEXT after the anchor AFTER in FILE, verified unchanged.
SYMBOL names the definition being created and appears in the report.
EXPECTED-BODY is the anchor text last seen; the region is re-verified
before anything is applied.  The layout between the anchor, the new
definition and what follows is reconciled by
`scalpel-execute-insert-after', not by the planner.

The report names what the file really holds afterwards.  The definition
NEW-TEXT writes may not be the one SYMBOL names, and a report repeating
SYMBOL would then send the next round after a definition that is not
there -- the failure is not the insert, it is the silence about it.
A form that defines no name at all -- a registration call -- is
reported as unlocatable for the same reason: the next round must not
be sent after a name the file does not hold."
  (with-current-buffer (find-file-noselect file)
    (let ((range (scalpel-agent--verified-range file after expected-body)))
      (scalpel-execute-insert-after (cdr range) new-text)
      (let* ((created (scalpel-agent--replacement-name new-text))
             (other (and created (not (string= created symbol)) created))
             (locatable (scalpel-agent--locatable-p file symbol))
             (note (cond
                    ((and other (not locatable))
                     (format (concat "; the definition that landed is named "
                                     "%s, so %s cannot be located here")
                             other symbol))
                    (other
                     (format "; the definition that landed is named %s" other))
                    ((not locatable)
                     (concat "; it cannot be located by that name, so the "
                             "next round will not find it"))
                    (t ""))))
        (format "Created %s in %s%s" symbol (buffer-name (current-buffer))
                note)))))

(defun scalpel-agent-block-edit (file symbol instruction on-success on-error)
  "Edit SYMBOL in FILE per INSTRUCTION, without blocking.
ON-SUCCESS receives the report string.  ON-ERROR receives a plist
\(:type SYMBOL :message STRING).  The boundary check inside
`scalpel-agent--apply-if-unchanged' runs in the LLM callback, so a
buffer the user edited while the request was in flight still aborts
the edit."
  (cl-block scalpel-agent-block-edit
    (unless (and file symbol instruction)
      (funcall on-error (list :type 'malformed
                              :message "Scalpel: malformed block-edit action"))
      (cl-return-from scalpel-agent-block-edit))
    ;; A symbol the planner names but names in the wrong file -- often
    ;; a hallucinated sibling path -- is corrected here, and a real
    ;; failure must still settle through ON-ERROR like every other
    ;; failure, not escape as a raw `user-error' that would block an
    ;; unattended run on a prompt.
    (let* ((resolved (condition-case err
                         (scalpel-agent--resolve-symbol file symbol)
                       (error
                        (funcall on-error
                                 (list :type 'locate
                                       :message (error-message-string err)))
                        (cl-return-from scalpel-agent-block-edit))))
           (file (car resolved))
           (range (cdr resolved)))
      (with-current-buffer (find-file-noselect file)
        (let* ((beg (car range))
               (end (cdr range))
               (body (buffer-substring-no-properties beg end))
               (signature (save-excursion
                            (goto-char beg)
                            (buffer-substring-no-properties
                             beg (line-end-position))))
               (prompt scalpel-prompt--block-edit-prompt)
               (language-rule
                (scalpel-prompt-language-rule-for-file file)))
          (scalpel-llm-request-async
           (concat (format prompt signature body instruction)
                   (when language-rule
                     (concat "\n\n" language-rule)))
           (lambda (new-text)
             (message "Scalpel-debug: edit reply arrived for %s" symbol)
             (let ((new-text (scalpel-agent--usable-replacement file new-text)))
               (cond
                ((string= new-text scalpel-agent--no-change-sentinel)
                 (funcall on-success
                          (format "No change needed: %s in %s" symbol file)))
                ((scalpel-locate-single-definition-p file new-text)
                 ;; The re-verified range can also fail -- the region
                 ;; changed in flight -- and it must settle through
                 ;; ON-ERROR alone: wrapping the apply as ON-SUCCESS's
                 ;; argument would still deliver a report after the
                 ;; failure was reported.
                 (let ((report (condition-case err
                                   (scalpel-agent--apply-if-unchanged
                                    file symbol body new-text)
                                 (error
                                  (funcall on-error
                                           (list :type 'locate
                                                 :message
                                                 (error-message-string err)))
                                  nil))))
                   (when report
                     (funcall on-success report))))
                (t
                 (funcall on-error
                          (list :type 'no-replacement
                                :message
                                (format (concat "Scalpel: planner returned no usable "
                                                "replacement for %s. "
                                                "Refusing to edit. %s Reply was: %S")
                                        symbol
                                        (scalpel-agent--unusable-replacement-reason
                                         file new-text)
                                        new-text)))))))
           on-error))))))

(defun scalpel-agent-block-insert (file symbol instruction after
                                  on-success on-error)
  "Create SYMBOL in FILE per INSTRUCTION, inserted after AFTER.
The reply must be exactly one complete top-level form of FILE's
language, and it need not define SYMBOL: a file's registration idiom
-- a call that extends the file, such as a provider or dialect
registration -- is a top-level unit of the file and is inserted like
a definition, because refusing it would make the idiom impossible to
reach through any tool.  ON-SUCCESS receives the report string, which
names the definition the file really holds when that is not SYMBOL.
ON-ERROR receives a plist (:type SYMBOL :message STRING)."
  (cl-block scalpel-agent-block-insert
    (unless (and file symbol instruction after)
      (funcall on-error (list :type 'malformed
                              :message "Scalpel: malformed block-insert action"))
      (cl-return-from scalpel-agent-block-insert))
    (let* ((anchor-resolved (scalpel-agent--resolve-symbol file after))
           (file (car anchor-resolved))
           (anchor-range (cdr anchor-resolved)))
      (with-current-buffer (find-file-noselect file)
        (let* ((anchor-end (cdr anchor-range))
               (anchor-body (buffer-substring-no-properties
                             (car anchor-range) anchor-end))
               (prompt scalpel-prompt--block-insert-prompt)
               (language-rule (scalpel-prompt-language-rule-for-file file)))
          (scalpel-llm-request-async
           (concat
            (format prompt
                    (save-excursion
                      (goto-char (car anchor-range))
                      (buffer-substring-no-properties
                       (car anchor-range)
                       (line-end-position)))
                    instruction)
            (when language-rule
              (concat "\n\n" language-rule)))
           (lambda (new-text)
             (let ((new-text (scalpel-agent--usable-replacement file new-text)))
               (cond
                ((string= new-text scalpel-agent--no-change-sentinel)
                 (funcall on-success
                          (format "No change needed: %s in %s" symbol file)))
                ;; The definition search inside
                ;; `scalpel-agent--usable-replacement' hunts through the
                ;; reply's lines for a definition, so it can never place
                ;; a top-level unit that defines no name.  The reply as
                ;; a whole is offered to the form validator next, which
                ;; is what admits a file's own registration idiom.
                ;;
                ;; It is asked only here, after the definition search
                ;; has failed, and never on the search's line-bounded
                ;; prefixes: a padded reply's first complete form is not
                ;; always the one meant -- a whole-file echo opens with
                ;; its `require' -- and the definition search is what
                ;; picks the wanted definition out of such a reply.
                ((or (scalpel-locate-single-definition-p file new-text)
                     (scalpel-locate-single-form-p file new-text))
                 (funcall on-success
                          (scalpel-agent--create-after-anchor
                           file symbol after anchor-body new-text)))
                (t
                 (funcall on-error
                          (list :type 'no-replacement
                                :message
                                (format (concat "Scalpel: planner returned no usable "
                                                "top-level form for %s. "
                                                "Refusing to create. %s Reply was: %S")
                                        symbol
                                        (scalpel-agent--unusable-replacement-reason
                                         file new-text t)
                                        new-text)))))))
           on-error))))))

(defun scalpel-agent-file-create (file text)
  "Create FILE with TEXT as its whole content.
Refuses an existing file --
changing one is `scalpel-agent-block-edit' and `scalpel-agent-block-insert'
work -- and creates missing parent directories.  The new file joins the
session context, because the planner made it and will want to read and
edit it next round; a context already at
`scalpel-agent-context-max-files' leaves it out and the report says so.
Return a human-readable report string.  Signal `user-error' on a
malformed action or an existing file."
  (unless (and file text)
    (user-error "Scalpel: malformed file-create action"))
  (when (file-exists-p file)
    (user-error "Scalpel: can't create %s: already exists" file))
  (let ((dir (file-name-directory (expand-file-name file))))
    (when dir (make-directory dir t)))
  (with-temp-file file (insert text))
  ;; The context follows the disk: FILE exists now, and nothing else would
  ;; ever put it in reach of a read, an edit or the sandbox.
  (concat (format "Created file %s" file)
          (or (scalpel-agent--context-track file) "")))

(defun scalpel-agent-block-delete (file symbol)
  "Delete SYMBOL in FILE through the boundary-locked deletion.
The line the definition occupied goes with it, and the blank lines
the deletion brings together are reconciled, so removing a block
does not leave an extra blank line where it stood.  The file is
saved before this returns.  Return a human-readable report string."
  (unless (and file symbol)
    (user-error "Scalpel: malformed block-delete action"))
  (let* ((asked file)
         (resolved (scalpel-agent--resolve-symbol file symbol))
         (file (car resolved))
         (range (cdr resolved)))
    (with-current-buffer (find-file-noselect file)
      (let* ((beg (car range))
             (end (cdr range))
             (body (buffer-substring-no-properties beg end))
             (verified (scalpel-agent--verified-range file symbol body))
             (report (format "Deleted %s in %s" symbol
                             (buffer-name (current-buffer)))))
        (scalpel-execute-delete (car verified) (cdr verified))
        ;; The correction must be visible, not silent: the report is
        ;; what the planner and the user read back.
        (if (string= asked file)
            report
          (concat report
                  (format "\nNote: the planner named %s; the definition lives in %s"
                          asked file)))))))

(defun scalpel-agent-file-delete (file)
  "Delete FILE.
The file is removed from disk, and its entry leaves the session context
with it.  A buffer visiting it is killed when it has no unsaved
changes; when it does, this refuses rather than discard them.  Return
a human-readable report string.  Signal `user-error' on a malformed
action or a missing file."
  (unless file
    (user-error "Scalpel: malformed file-delete action"))
  (unless (file-exists-p file)
    (user-error "Scalpel: can't delete %s: no such file" file))
  (let ((buffer (find-buffer-visiting file)))
    (when (and buffer (buffer-modified-p buffer))
      (user-error "Scalpel: %s has unsaved changes; not deleting" file))
    (when (and buffer (buffer-live-p buffer))
      (kill-buffer buffer))
    (delete-file file)
    ;; A dead path must not stay in the context: it costs a line in every
    ;; prompt and a read-only bind in the sandbox, and opening it reports
    ;; a file with no symbols rather than the absence the session should
    ;; have recorded.
    (scalpel-agent--context-untrack file)
    (format "Deleted file %s" file)))

(defun scalpel-agent-file-rename (file to)
  "Rename or move FILE to TO.
Only the file is moved: the definitions inside it are untouched,
and no other file's require, import or path string is updated.
The destination must not already exist.  A buffer visiting FILE
is saved first and then follows the rename.  A context entry for
FILE moves with it, and a rename never adds a file the session did
not already hold.  Return a human-readable report string.  Signal
`user-error' on a malformed action, a missing source, or an
existing destination."
  (unless (and file to)
    (user-error "Scalpel: malformed file-rename action"))
  (unless (file-exists-p file)
    (user-error "Scalpel: can't rename %s: no such file" file))
  (when (file-exists-p to)
    (user-error "Scalpel: can't rename %s: %s already exists" file to))
  (let ((buffer (find-buffer-visiting file))
        ;; Asked while the old path still names a file: the session tracks
        ;; files by their resolved path, and nothing can be resolved
        ;; against a path that has moved away.
        (tracked (scalpel-agent--context-file-p file)))
    (when buffer
      (with-current-buffer buffer
        (when (buffer-modified-p)
          (save-buffer))))
    (rename-file file to)
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (set-visited-file-name to t)))
    ;; The context entry moves with the file.  Left behind, it would keep
    ;; a path the session cannot open in every later prompt, while the
    ;; path that now exists -- the only readable one -- stayed out of
    ;; reach.  A rename is not a way into the context: a file the session
    ;; never held adds nothing.  The track below cannot report a refusal,
    ;; because the swap removes one entry before it adds one and the
    ;; count never grows.
    (when tracked
      (scalpel-agent--context-untrack file)
      (scalpel-agent--context-track to))
    (format "Renamed %s to %s" file to)))

(defun scalpel-agent--printable-output (text)
  "Return TEXT with only tab, newline and printable characters.
Command output can carry control bytes, such as the bell a batch
child Emacs may write.  Newline and tab stay; every other control
byte is display junk."
  (mapconcat #'char-to-string
             (cl-remove-if-not
              (lambda (char)
                (or (memq char '(?\n ?\t))
                    (and (<= 32 char)
                         (not (<= 127 char 159)))))
              (string-to-list text))
             ""))

(defun scalpel-agent-shell (command reason)
  "Run COMMAND through a shell in the console root.
The command's reach is bounded by the sandbox, whose file scope is
exactly the current context files and which binds each of them
read-only: a shell command may inspect the context but never write to
it, so file changes always pass through block-edit, block-insert,
block-delete, file-create, file-rename, file-delete and
file-substitute.  The
confirmation gate is driven by `scalpel-agent-confirm-tools'.
COMMAND may use pipes, redirection and quoting.  The report names
the command, always states the exit status, and wraps the output in
explicit markers, so a reader (human or LLM) can tell which command
produced what.  Output is truncated to
`scalpel-agent-shell-max-bytes' bytes, and control characters other
than newline and tab are dropped from it.  REASON is the planner's
stated intent, echoed in the report.
The report always states the true output size, so a command that
dumped far more than it should is visible to the user and to the
planner.  Output holding a NUL byte is reported as binary and its
contents are dropped.
Signal `user-error' when the sandbox is unavailable or fails its
probe, so a command is never run outside the sandbox."
  (unless (and command reason)
    (user-error "Scalpel: malformed shell action"))
  (let* ((root (or (and (bound-and-true-p scalpel-console--root)
                        scalpel-console--root)
                   default-directory))
         (max-bytes scalpel-agent-shell-max-bytes)
         (result
          ;; No `condition-case' here: a sandbox refusal must abort the
          ;; action loudly, never degrade into a shell report that looks
          ;; like the command ran.
          (scalpel-sandbox-run
           command
           root
           scalpel-agent--context-files))
         (exit (car result))
         (raw (cdr result))
         ;; Measure and classify RAW before sanitizing: the sanitizer
         ;; drops NUL bytes, so binary detection must happen here.
         (raw-bytes (string-bytes raw))
         (binary (and (cl-position 0 raw) t))
         (output (scalpel-agent--printable-output raw))
         (truncated (> (length output) max-bytes))
         (body (cond
                (binary
                 (format "[binary output suppressed: %d bytes]" raw-bytes))
                (truncated
                 (concat (substring output 0 max-bytes)
                         (format "\n[truncated: showing first %d of %d bytes]"
                                 max-bytes raw-bytes)))
                (t output))))
    (setq scalpel-agent--shell-output
          (list :bytes raw-bytes :truncated truncated :binary binary))
    (format (concat "Shell: %s\nReason: %s\nExit: %s\nOutput: %d bytes\n"
                    "--- output ---\n%s\n--- end output ---")
            command reason (or exit "unknown") raw-bytes body)))

(defun scalpel-agent--context-file-p (file)
  "Return non-nil when FILE is a member of the session context.
Names are compared as truenames, which is how
`scalpel-agent--expanded-files' stores them."
  (and (member (file-truename (expand-file-name file))
               scalpel-agent--context-files)
       t))

(defconst scalpel-agent--context-near-miss-min-prefix 1
  "Minimum number of path components that must agree for a near miss.
With 1, an entry sharing only the home directory still counts as a
near miss, which is exactly the single-user-machine case this covers
-- a mistyped user name like \"madajuan\" vs \"madachuan\" shares
only Users.  Callers rank by prefix length and basename, so
unrelated entries elsewhere on the filesystem do not flood the
note.")

(defun scalpel-agent--context-near-miss (file &optional min-prefix)
  "Suggest the context entries closest to FILE for a refusal note.

Resolve FILE the same way `scalpel-agent--context-file-p' does (via
`file-truename' and `expand-file-name'), then score each entry in
`scalpel-agent--context-files' by the length of the common
directory-style prefix, comparing path components split on \"/\" so
that /Users/madachuan and /Users/madaduan stop differing at the
component level, not mid-word.  As a tiebreak within the same prefix
length, prefer the entry whose basename is closest.

Only entries sharing at least MIN-PREFIX path components (default
`scalpel-agent--context-near-miss-min-prefix') are considered.
Return nil when no entry qualifies, or the list of qualifying
absolute path strings (all ties) otherwise.  This function never
decides a correction, it only ranks facts for the caller to print as
\"closest context entries (for comparison)\"."
  (let* ((min (or min-prefix scalpel-agent--context-near-miss-min-prefix))
         (target (file-truename (expand-file-name file)))
         (target-parts (split-string target "/" t))
         scored)
    (dolist (entry scalpel-agent--context-files)
      (let* ((entry-path (file-truename (expand-file-name entry)))
             (entry-parts (split-string entry-path "/" t))
             (common 0)
             (limit (min (length target-parts) (length entry-parts))))
        (while (and (< common limit)
                    (string-equal (nth common target-parts)
                                  (nth common entry-parts)))
          (setq common (1+ common)))
        (when (>= common min)
          (push (cons entry common) scored))))
    (when scored
      (let ((top (apply #'max (mapcar #'cdr scored))))
        (mapcar #'car
                (nreverse (cl-remove-if-not
                           (lambda (pair) (= (cdr pair) top))
                           scored)))))))

(defun scalpel-agent--byte-prefix (text max-bytes)
  "Return the longest prefix of TEXT within MAX-BYTES bytes.
Return TEXT itself when it already fits.  The cut never lands
inside a character, so the result stays a valid string."
  (if (<= (string-bytes text) max-bytes)
      text
    (let ((low 0)
          (high (length text)))
      (while (< low high)
        (let ((mid (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring text 0 mid)) max-bytes)
              (setq low mid)
            (setq high (1- mid)))))
      (substring text 0 low))))

(defun scalpel-agent-file-read (file symbol)
  "Read FILE from the session context, whole or as one SYMBOL.
FILE must be a member of `scalpel-agent--context-files': a shell
command is bounded by the sandbox, but a read runs inside Emacs
itself, so the context list is the only boundary there is.  With
SYMBOL, return that definition; with SYMBOL nil, return the whole
file.  Return a human-readable report.

A definition is never truncated.  A partial definition is worse
than none: a replacement built from one can still parse as a
complete form and be applied, so the failure would be silent.
When a definition exceeds `scalpel-agent-file-read-max-bytes', signal
`user-error' with its true size instead, and leave the caller to
ask for the whole file.  A whole-file read is a partial view by
construction, so it is truncated at that limit with a marker that
states the true size.  Output holding a NUL byte is withheld."
  (unless file
    (user-error "Scalpel: malformed file-peek action"))
  (unless (scalpel-agent--context-file-p file)
    (user-error
     (concat "Scalpel: %s is not in the context; ask the user to add it "
             "with a confirm action instead of reading it%s")
     file
     (let ((near (scalpel-agent--context-near-miss file)))
       (if near
           (format "\nNote: the closest context entries are: %s (for comparison only; verify the exact spelling against the context listing above)"
                   (string-join near ", "))
         ""))))
  (let ((resolved (file-truename (expand-file-name file))))
    (if symbol
        (let* ((hit (scalpel-agent--resolve-symbol resolved symbol))
               (resolved (car hit))
               (range (cdr hit))
               (body (with-current-buffer (find-file-noselect resolved)
                       (buffer-substring-no-properties
                        (car range) (cdr range))))
               (bytes (string-bytes body)))
          (when (> bytes scalpel-agent-file-read-max-bytes)
            (user-error
             (concat "Scalpel: definition of %s in %s is %d bytes, over the "
                     "read limit of %d; read the whole file instead of "
                     "accepting a truncated definition")
             symbol resolved bytes scalpel-agent-file-read-max-bytes))
          (when (cl-position 0 body)
            (user-error "Scalpel: %s in %s is binary; contents withheld"
                        symbol resolved))
          (format (concat "Read: %s in %s\nOutput: %d bytes\n"
                          "--- output ---\n%s\n--- end output ---")
                  symbol resolved bytes body))
      (let* ((raw (with-current-buffer (find-file-noselect resolved)
                    (buffer-substring-no-properties (point-min) (point-max))))
             (bytes (string-bytes raw))
             (binary (and (cl-position 0 raw) t))
             (truncated (and (not binary)
                             (> bytes scalpel-agent-file-read-max-bytes)))
             (body (cond
                    (binary
                     (format "[binary file withheld: %d bytes]" bytes))
                    (truncated
                     (let ((prefix (scalpel-agent--byte-prefix
                                    raw scalpel-agent-file-read-max-bytes)))
                       (format "%s\n[truncated: showing first %d of %d bytes]"
                               prefix (string-bytes prefix) bytes)))
                    (t raw))))
        (format (concat "Read: %s\nOutput: %d bytes\n"
                        "--- output ---\n%s\n--- end output ---")
                resolved bytes body)))))

(defun scalpel-agent--definitions-in-text (file text)
  "Return the top-level definition names TEXT would give FILE.
FILE selects the locator provider, so the names are read in the
language the file is written in; TEXT is read in a temporary buffer,
because the providers work on the current buffer and the question
here is about text that is not on disk yet.  Return nil for a file no
provider handles, or one whose provider cannot list definitions: a
caller must read nil as \"cannot tell\", never as \"defines nothing\"."
  (let* ((provider (scalpel-locate-provider-for-file file))
         (list-symbols (and provider (plist-get provider :list-symbols))))
    (when list-symbols
      (with-temp-buffer
        (insert text)
        (delete-dups (funcall list-symbols file))))))

(defun scalpel-agent--rewrite-definition-note (file before after)
  "Return a report line naming the definitions a rewrite changed in FILE.
BEFORE and AFTER are the definition names FILE's text held before and
after the rewrite, as `scalpel-agent--definitions-in-text' reads them.
Return nil when the two agree.

The removals are named first because they are what a later round trips
over: a definition renamed by a bulk rewrite leaves its old name in
the planner's hands, and the locate failure that follows says only
\"not found\", which explains nothing about where the name went.  This
is report text, not a refusal: dropping a name is what a rename is,
and refusing it would refuse the bulk rename the tool exists for."
  (let ((gone (cl-set-difference before after :test #'string=))
        (fresh (cl-set-difference after before :test #'string=)))
    (when (or gone fresh)
      (concat (format "Note: definitions changed in %s:" file)
              (when gone
                (format " no longer defined: %s" (string-join gone ", ")))
              (when fresh
                (format "%snow defined: %s"
                        (if gone "; " " ")
                        (string-join fresh ", ")))))))

(defconst scalpel-agent--substitute-hint-max-lines 5
  "Maximum number of near-miss lines a zero-match refusal shows.
The lines are evidence, not the answer, and a refusal that pasted a
whole file would cost the planner the tokens the rewrite was meant
to save.")

(defconst scalpel-agent--substitute-hint-min-prefix 12
  "Shortest pattern prefix a near-miss search will look for.
Below this length a hit says nothing: a pattern's opening
characters are punctuation shared by most lines of a file.  Twelve
characters is about one identifier, the shortest run that can still
be about the text the planner was aiming at.")

(defconst scalpel-agent--substitute-hint-attempts 100
  "Maximum number of prefix lengths a near-miss search tries.
One character is dropped per step, so a pattern no longer than this
many characters is walked one length at a time; a longer one steps
further, because the walk runs only on a zero-match refusal and only
over text already in memory, and a hundred lengths cover far more of
a line than any pattern a rewrite is built from.")

(defconst scalpel-agent--substitute-bracket-reading-note
  (concat "Note: in Emacs regexp syntax \"\\(\" and \"\\)\" open and close "
          "a group and match no brackets, while a literal bracket is written "
          "\"(\" and \")\"; the lines above are the file's own text.")
  "Sentence naming the reading the refusal's quoted lines came from.
Structural contract shared by
`scalpel-agent--substitute-near-miss-note', which appends it, and the
test that pins it.  It is appended when the lines quoted above it were
placed by reading the pattern's bracket escapes as literal brackets,
which is the fact the planner needs for its next attempt: it wrote the
escaped form of a bracket it wanted to match.

The sentence states the syntax rule and where the quoted lines come
from -- both true whenever it is printed -- instead of claiming that no
other reading could reach them.  That stronger claim is not decidable
here: an escape both readings share is searched for as a backslash the
file does not hold, so it truncates both walks at the same point, and
the earlier wording was withheld on exactly that account while the
quoted line held no reason the planner could act on.

The worked example in `scalpel-prompt--substitute-pattern-rule' is the
prevention this sentence is the fallback for: the prose rule alone did
not stop the escaped spelling, which came back three rounds running,
each time refusing for matching nothing.")

(defun scalpel-agent--substitute-literal-brackets (pattern)
  "Return PATTERN read with its bracket escapes as literal brackets.
Emacs regexp syntax spells a group \"\\(\" and \"\\)\" and a literal
bracket \"(\" and \")\", so a pattern that writes the escaped form
where a bracket was meant matches the text between them and never
the brackets themselves -- which usually matches nothing at all.
Return nil when PATTERN escapes no bracket: then both readings are
the same text, and there is nothing to tell apart."
  (when (string-match-p "\\\\[()]" pattern)
    (replace-regexp-in-string "\\\\\\([()]\\)" "\\1" pattern)))

(defun scalpel-agent--lines-holding (text literal)
  "Return the lines of TEXT that hold LITERAL, up to a cap.
LITERAL is searched for as text, never as a regular expression, so
a line carrying the same characters as the pattern's opening run is
found however the pattern would have read them.  Lines come back
trimmed, deduplicated and in file order, so the caller can print
them as the file's own text rather than as an analysis of it.  The
cap is `scalpel-agent--substitute-hint-max-lines': the lines are
evidence, and a refusal that pasted a whole file would cost the
planner the tokens the rewrite was meant to save."
  (let ((pattern (regexp-quote literal))
        lines)
    (dolist (line (split-string text "\n"))
      (when (and (< (length lines) scalpel-agent--substitute-hint-max-lines)
                 (string-match-p pattern line))
        (let ((trimmed (string-trim line)))
          (unless (or (string-empty-p trimmed)
                      (member trimmed lines))
            (push trimmed lines)))))
    (nreverse lines)))

(defun scalpel-agent--line-at (text offset)
  "Return the whole line of TEXT holding OFFSET.
The substitute report pairs the line each match sat on before the
rewrite with the line the same occurrence sits on after it, so a
damaging replacement is quoted rather than only counted."
  (let ((beg (save-match-data
               ;; An empty match at position zero and a one-character
               ;; match elsewhere both end just after a line start, so
               ;; match-end names the beginning of OFFSET's line.
               (if (string-match "\\`\\|." text (max 0 (1- offset)))
                   (match-end 0)
                 0)))
        (end (save-match-data
               (string-match "\\n" text offset))))
    (substring text beg (or end (length text)))))

(defun scalpel-agent--excerpt-line (text)
  "Return line TEXT for display.
Collapse any embedded newlines (or carriage returns) to a literal
\\n sequence, then truncate the result to 60 characters, appending
\"...\" if truncation occurred."
  (let ((line (replace-regexp-in-string "[\n\r]+" "\\\\n" text)))
    (if (> (length line) 60)
        (concat (substring line 0 60) "...")
      line)))

(defun scalpel-agent--substitute-prefix-lines (text pattern)
  "Return the lines of TEXT holding the longest prefix of PATTERN.
Return nil when no prefix of at least
`scalpel-agent--substitute-hint-min-prefix' characters is found.
Successively shorter prefixes are looked for, longest first, so a
stub that matches the file's first characters by coincidence cannot
displace a longer near miss.  Prefixes are taken exactly as PATTERN
spells them, so a \"\\.\" is searched for as a backslash and a dot.
That can only shorten the run the search finds; it never rewrites the
pattern, because the intent behind a regular expression cannot be
read back out of it without guessing.

Each step drops one character, not half the remainder.  Halving
skips lengths, and the length it skips is exactly where a near miss
lives: a pattern of 23 characters was halved -- and lost -- to 11,
under the minimum of twelve, so the twelve-character run its opening
shares with the file was never looked for and the refusal quoted
nothing.  A pattern longer than `scalpel-agent--substitute-hint-attempts'
characters steps further, so the number of searches stays bounded."
  (let* ((pattern (string-remove-prefix "^" pattern))
         (length (length pattern))
         (limit scalpel-agent--substitute-hint-min-prefix)
         (attempts scalpel-agent--substitute-hint-attempts)
         (step (max 1 (ceiling (/ length (float attempts)))))
         found)
    (while (and (not found) (>= length limit))
      (let ((lines (scalpel-agent--lines-holding
                    text (substring pattern 0 length))))
        (if lines
            (setq found lines)
          (setq length (- length step)))))
    found))

(defun scalpel-agent--substitute-hint-lines (text pattern)
  "Return (LINES . BRACKET-READING) for TEXT against PATTERN.
LINES are the lines of TEXT holding the longest run PATTERN shares
with the file, or nil.  BRACKET-READING is non-nil when LINES were
placed by reading PATTERN's bracket escapes as literal brackets.

The reading that takes the brackets as characters is preferred and
is asked first, because it is the one that reaches furthest into the
line the planner was aiming at.  How much of PATTERN each reading
places is deliberately not what decides the sentence: an escape both
readings share -- a \"\\.\" earlier in the same line, say -- is
searched for as a backslash the file does not hold, so it truncates
the walk at the same point either way, and a refusal that compared
lengths stayed silent on that account.  The observed failure quoted
the file's line with no reason attached, leaving the planner with
nothing to change.  What is decidable is whether the bracket-literal
reading places a run at all, and a pattern that matches no brackets
while the file's text holds them is the shape the sentence exists
for.

The second reading is reported, never applied: what runs is the
pattern the planner wrote, so a pattern whose brackets really are
groups still refuses, and this only says which reading would have
landed.  Both answers are nil when neither reading places a run of
`scalpel-agent--substitute-hint-min-prefix' characters."
  (let* ((literal (scalpel-agent--substitute-literal-brackets pattern))
         (by-brackets (and literal
                           (scalpel-agent--substitute-prefix-lines
                            text literal))))
    (if by-brackets
        (cons by-brackets t)
      (cons (scalpel-agent--substitute-prefix-lines text pattern) nil))))

(defconst scalpel-agent--substitute-bracket-correction-matches
  "\nThe same pattern, with its brackets written as literal brackets, is: %s"
  "Sentence handing over the bracket-literal spelling of a pattern.
Structural contract shared by `scalpel-agent--substitute-near-miss-note',
which prints it, and the test that pins it.  The `%s' is the corrected
pattern.  It is printed only after that spelling was matched against
the file's text, so the pattern it names is one the next attempt can
really use; when the spelling matches nothing either, the refusal
prints `scalpel-agent--substitute-bracket-correction-fails' instead.")

(defconst scalpel-agent--substitute-bracket-correction-fails
  (concat "\nThe same pattern read with its brackets as literal brackets "
          "matches nothing in the file either, so the brackets are not "
          "the only difference between it and the text; compare the "
          "pattern with the lines above.")
  "Sentence printed when the bracket-literal spelling matches nothing.
Structural contract shared by the same two places as
`scalpel-agent--substitute-bracket-correction-matches'.  Offering a
corrected pattern here would send the next attempt after one that
fails for the same reason the original did, so the refusal says the
correction does not land and leaves the file's own lines above as the
text to compare against.")

(defconst scalpel-agent--substitute-regexp-construct-escapes
  '(?| ?\( ?\) ?< ?> ?b ?B ?w ?W ?s ?S ?_ ?\` ?\' ?= ?\{ ?\} ?? ?+ ?* ?c ?C
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "Characters a backslash turns into a regexp construct, not a literal.
`\\|' alternates, `\\(' opens a group, `\\s' names a syntax class,
`\\1' is a backreference: after each character here the backslash
carries meaning of its own.  After any other character -- a \"\\&\" or
a \"\\.\", say -- the backslash only makes that character literal,
which is what lets
`scalpel-agent--substitute-escaped-literal-note' name the character a
pattern demands.

The list errs toward including a character, because the two mistakes
do not cost the same: a literal left on the list only silences the
note, while a construct left off it would make the note report a
reading of the pattern that is not true.  A refusal may stay silent
about why a pattern matched nothing; it must never state a reason
that is wrong.  Digits are included because \"\\1\" is a
backreference rather than a literal \"1\".

A member is written with a backslash before the character wherever that
character carries syntax of its own, so that a tool reading this buffer
by syntax finds no structure in the list: an unescaped open-bracket
member, or a member for the character a comment starts with, would make
a checker such as `check-parens' report the defconst as unbalanced even
though the reader reads every member as the character it names.")

(defun scalpel-agent--substitute-escaped-literals (pattern)
  "Return the characters PATTERN demands through a backslash escape.
A backslash before a character that is no regexp construct means that
character literally, so \"\\&\" in a pattern matches only text holding
a real ampersand.  Characters whose escape is a construct are left
out -- see `scalpel-agent--substitute-regexp-construct-escapes' -- and
each character comes back once, in the order first written."
  (let ((pos 0)
        (chars nil))
    (while (string-match "\\\\\\(.\\)" pattern pos)
      (let ((char (aref pattern (1+ (match-beginning 0)))))
        (unless (memq char scalpel-agent--substitute-regexp-construct-escapes)
          (cl-pushnew char chars)))
      (setq pos (match-end 0)))
    (nreverse chars)))

(defun scalpel-agent--substitute-escaped-literal-note (staged pattern)
  "Return a note naming escaped literals absent from every STAGED file.
STAGED is the list of (RESOLVED OLD NEW) the rewrite was built from,
so OLD is each file's text as it was read before anything was
written.  The note names every character PATTERN demands through a
backslash escape and that appears in none of those texts.  Return the
empty string when every such character is present somewhere.

The quoted near-miss lines cannot carry this: they show the file's
nearest text, so a character absent from the whole file -- the
placeholder a planner borrowed from a replacement, say -- looks
present to a reader comparing line against pattern.  A zero match
then has a cause the refusal can state as a fact about the files
rather than as a guess about what was meant."
  (let ((missing
         (cl-remove-if
          (lambda (char)
            (cl-some (lambda (entry)
                       (cl-position char (nth 1 entry)))
                     staged))
          (scalpel-agent--substitute-escaped-literals pattern))))
    (when missing
      (let* ((escaped (string-join
                       (mapcar (lambda (char) (format "\"\\%c\"" char))
                               missing)
                       ", "))
             (plain (string-join
                     (mapcar (lambda (char) (format "\"%c\"" char))
                             missing)
                     ", ")))
        (format (concat "\nNote: the pattern writes %s, which Emacs regexp "
                        "syntax reads as the literal character%s %s; no file "
                        "in the list holds %s.")
                escaped (if (cdr missing) "s" "") plain plain)))))

(defun scalpel-agent--substitute-correction-matches-p (staged corrected)
  "Return non-nil when CORRECTED matches some text in STAGED.
STAGED is the list of (RESOLVED OLD NEW) the rewrite was built from,
so OLD is the file's text as it was read before anything was written.
CORRECTED is the pattern read with its bracket escapes as literal
brackets, and the caller offers it to the planner as the pattern to
use next; it is therefore matched against the bytes on disk first.  A
rewrite refused for matching nothing would otherwise hand over a
spelling that fails for the same reason its predecessor did, and the
next attempt would spend itself on that pattern.

The match runs with case folding off, the way the rewrite itself ran,
so the answer describes the same matching.  A CORRECTED the matcher
refuses -- dropping an escape can leave a regexp that no longer parses
-- reads as \"does not match\": the sentence it replaces exists to say
the correction does not land, which is true of one that cannot be
tried."
  (let ((case-fold-search nil))
    (cl-some (lambda (entry)
               (condition-case nil
                   (and (string-match corrected (nth 1 entry)) t)
                 (error nil)))
             staged)))

(defun scalpel-agent--substitute-near-miss-note (staged pattern)
  "Return a note naming the near-miss lines of STAGED for PATTERN.
STAGED is the list of (RESOLVED OLD NEW) the rewrite was built from,
so OLD is the file's text as it was read before anything was
written, and the note needs no second read.  Return the empty string
when no such line is found in any of them.

The note belongs to the refusal for a pattern that matched nothing.
That refusal is correct and stays -- a rewrite that matched nothing
is a planner mistake, never a success -- but it cannot say why on
its own, and the planner wrote the pattern it expected to find.  The
file's actual text around that expectation is the one fact the
refusal lacks: it is what separates a pattern aimed at a line the
file does not hold from one written as the text the rewrite was
meant to produce.  The lines are quoted, never inferred.

When those lines were placed by reading the pattern's bracket escapes
as literal brackets, the note says so, because that reading is the
fact the planner needs for its next attempt: it wrote the escaped
form of a bracket it wanted to match.  The sentence is evidence like
the lines are -- it appears only when the file really holds them
under that reading -- and the pattern itself is never rewritten.
The corrected spelling is handed over with it, so the next attempt
can be copied rather than translated: turning \"\\(\" back into \"(\"
one escape at a time is the step that was observed to fail, and a
refusal that leaves it to the reader is a refusal that repeats.

That spelling is checked against the file's text before it is
offered, because a correction derived from the pattern alone can
still fail: the brackets may not be the only difference, and a
refusal that presents the next pattern to use would then send the
planner after one that fails the way the last one did.  When the
bracket-literal reading matches nothing either, the refusal says
that instead, and the file's lines quoted above are what the planner
compares against."
  (let (blocks
        bracket-reading)
    (dolist (entry staged)
      (let* ((resolved (nth 0 entry))
             (hint (scalpel-agent--substitute-hint-lines
                    (nth 1 entry) pattern))
             (lines (car hint)))
        (when lines
          (setq bracket-reading (or bracket-reading (cdr hint)))
          (push (format "\nThe closest lines in %s are:\n%s"
                        resolved
                        (string-join
                         (mapcar (lambda (line) (format "  %s" line))
                                 lines)
                         "\n"))
                blocks))))
    (concat (string-join (nreverse blocks) "")
            (when bracket-reading
              (concat "\n"
                      scalpel-agent--substitute-bracket-reading-note
                      ;; The corrected spelling, spelled for the
                      ;; reading that placed the lines: dots stay
                      ;; escaped, because a dot is special to Emacs
                      ;; regexp syntax, and the brackets do not,
                      ;; because they are not.  Nothing is applied
                      ;; under it -- the user confirmed the pattern
                      ;; they were shown, and running another one
                      ;; would be running something they never saw --
                      ;; and it is offered only after being matched
                      ;; against the file's text, so the pattern the
                      ;; planner copies is one that really lands.
                      (let ((corrected
                             (scalpel-agent--substitute-literal-brackets
                              pattern)))
                        (if (scalpel-agent--substitute-correction-matches-p
                             staged corrected)
                            (format
                             scalpel-agent--substitute-bracket-correction-matches
                             corrected)
                          scalpel-agent--substitute-bracket-correction-fails)))))))

(defun scalpel-agent--substitute-invocation (pattern replacement)
  "Return the pattern and the replacement of a file-substitute, on one line.
Both halves are named whatever the refusal's cause was: a pattern and
the replacement it is applied with are one action, and the next attempt
corrects the action.  A refusal that names only the half which failed --
a pattern that matched nothing, a replacement the replace matcher
rejected -- leaves the other half to be restored from memory, and memory
is what wrote the refused half in the first place.

PATTERN and REPLACEMENT are printed with %S, so either may be anything
the action carried, including nil: a malformed action describes itself
instead of raising a second error inside the message builder."
  (format "pattern %S -> replacement %S" pattern replacement))

(define-error 'scalpel-no-validation
  "A rewrite refused because the file's language cannot validate it:
a registered locate provider declares no :balanced-p, so a rewrite
there has no structural check to pass.  It derives from `error' so
existing catchers keep working, but its own name lets the execute
path tag the failure as `no-validation', which the diagnose side
offers a block-edit retry for."
  'error)

(defun scalpel-agent--first-unbalance-offset (text)
  "Return the offset in TEXT where it first becomes bracket-unbalanced.
Scans the text as a plain character string, so the answer is the
same one `scalpel-locate-balanced-p' would be asked about; returns
the length of TEXT when the imbalance is only an unclosed bracket
that never finds its close."
  (let ((depth 0)
        (openers '(?\( ?\[ ?\{))
        (closers '(?\) ?\] ?\})))
    (catch 'result
      (cl-loop for i from 0 below (length text)
               for ch = (aref text i)
               do (cond
                   ((memq ch openers) (setq depth (1+ depth)))
                   ((memq ch closers)
                    (setq depth (1- depth))
                    (when (< depth 0)
                      (throw 'result i))))
               finally (throw 'result (length text))))))

(defun scalpel-agent-file-substitute (files pattern replacement)
  "Apply the mechanical replacement PATTERN -> REPLACEMENT across FILES.
This is the planner's channel for one mechanical batch
transformation -- the job a whole-file shell one-liner would
otherwise be asked to do -- executed by Scalpel itself, so its
effect is enumerable: every file touched and every occurrence
replaced is named in the report.  PATTERN is an Emacs Lisp regexp;
REPLACEMENT is replacement text, where \\N and \\& refer to the
match as `replace-regexp-in-string' reads them.  All FILES must be
in the session context; a path outside it is refused, the same
boundary a read obeys.

Only files whose locate provider is registered but declares no
:balanced-p are refused: the bracket check is the only structural
validation a rewrite gets, so a registered language that cannot
answer it has no safety net, and prose damage like emptied
backtick pairs would pass silently.  Files with no registered
provider are not structured languages and are outside this
contract; they remain governed by the guards below.  Block-edit is
the channel for such structured files, one named definition at a
time.

The whole transformation is computed and validated before anything
reaches disk: new contents are built in memory, an `.el' file whose
new content has unbalanced brackets refuses the whole rewrite, and
zero occurrences anywhere refuses it too -- a rewrite that matched
nothing is a planner mistake, not a success.  An unbalanced refusal
names the first offset where the rewritten text goes wrong, through
`scalpel-agent--first-unbalance-offset', with a short excerpt, so
the next attempt corrects the replacement instead of re-deriving it
from memory.  The report also names the definitions the rewrite
changed in each file, because a bulk rename leaves the old name in
the planner's hands and the next round's locate failure explains
nothing on its own.  A zero-match refusal quotes the lines that
begin like the pattern, so the planner can correct the pattern it
wrote rather than spend a round reading the text it had already
misremembered.  The report quotes before/after lines for the first
occurrences, so a prose-damaging replacement is visible, not just
counted.  Every refusal states the pattern and the replacement
together, through `scalpel-agent--substitute-invocation', because
the two are one action: a refusal naming only the half which failed
leaves the other half to be restored from memory.  Return a
human-readable report.  Signal `user-error' on malformed input, a
file outside the context, a bad replacement, no matches, or an
unbalanced result.  Signal `scalpel-no-validation' for a structured
language whose provider declares no :balanced-p, so the diagnose
side can offer block-edit as the retry."
  (unless (and files pattern (stringp replacement))
    (user-error "Scalpel: malformed file-substitute action: %s, files %S"
                (scalpel-agent--substitute-invocation pattern replacement)
                files))
  (dolist (file files)
    (unless (scalpel-agent--context-file-p file)
      (user-error
       (concat "Scalpel: %s is not in the context, so file-substitute "
               "refuses it: the action never changes a file outside the "
               "context.  %s; ask the user to add it (C-c C-a in the "
               "console) and try again\n%s")
       file
       (if (file-exists-p (expand-file-name file))
           "The file exists on disk"
         "No such file exists on disk")
       (scalpel-agent--substitute-invocation pattern replacement))))
  ;; The balance check is the only structural validation a rewrite
  ;; gets, so a language that cannot answer it has no safety net,
  ;; and prose damage like emptied backtick pairs passes silently;
  ;; steering those files to block-edit is the fix, not a special
  ;; case for markdown.  Files whose provider is not registered are
  ;; not structured languages and are not under this gate; their
  ;; guard is the zero-match refusal below.
  (dolist (file files)
    (let* ((resolved (file-truename (expand-file-name file)))
           (provider (scalpel-locate-provider-for-file resolved)))
      (when (and provider (not (plist-get provider :balanced-p)))
        (signal 'scalpel-no-validation
                (list
                 (format
                  (concat "Scalpel: file-substitute is a structured-language tool "
                          "and %s's provider declares no bracket check "
                          "(:balanced-p), so a rewrite there cannot be validated; "
                          "refused.  Retry next round with block-edit instead, "
                          "one named definition at a time\n%s")
                  resolved
                  (scalpel-agent--substitute-invocation pattern replacement)))))))
  ;; First pass: compute every new content in memory, so a failure in
  ;; the last file cannot leave the first ones half-rewritten.
  (let (staged)
    (dolist (file files)
      (let* ((resolved (file-truename (expand-file-name file)))
             (old (with-current-buffer (find-file-noselect resolved)
                    (buffer-substring-no-properties
                     (point-min) (point-max))))
             (new (condition-case err
                      (let ((case-fold-search nil))
                        (replace-regexp-in-string pattern replacement old))
                   (error
                    (user-error
                     (concat "Scalpel: file-substitute replacement is "
                             "malformed (in %s): %s\n%s")
                     resolved
                     (error-message-string err)
                     (scalpel-agent--substitute-invocation
                      pattern replacement))))))
        ;; The bracket question is the language provider's, asked of the
        ;; rewritten text: one answer for both this check and a refused
        ;; reply, so a language that says nothing about bracket shape is
        ;; never reported as having one.  It used to be an Emacs Lisp walk
        ;; written here, beside the one in the refusal path.
        (when (not (scalpel-locate-balanced-p resolved new))
          (let ((offset (scalpel-agent--first-unbalance-offset new)))
            (user-error
             (concat "Scalpel: file-substitute of %s would leave unbalanced "
                     "brackets; refused whole\n%s%s")
             resolved
             (scalpel-agent--substitute-invocation pattern replacement)
             (if (< offset (length new))
                 (format "\nThe rewritten text first goes wrong at character %d: %S"
                         offset
                         (replace-regexp-in-string
                          "\n" "\\\\n"
                          (substring new
                                     (max 0 (- offset 30))
                                     (min (length new) (+ offset 30)))))
               "\nThe rewritten text left an unclosed bracket."))))
        (push (list resolved old new) staged)))
    (setq staged (nreverse staged))
    ;; Zero matches overall is a planner mistake: refuse instead of
    ;; reporting a successful no-op.  A file whose content changed is
    ;; one that matched at least once.
    (unless (cl-some (lambda (entry)
                       (not (string= (nth 1 entry) (nth 2 entry))))
                     staged)
      (user-error
       (concat "Scalpel: file-substitute matched nothing in any of the "
               "%d file(s) (%s); refusing\n%s%s")
       (length staged)
       (string-join (mapcar (lambda (entry) (nth 0 entry)) staged) ", ")
       (scalpel-agent--substitute-invocation pattern replacement)
       (concat (scalpel-agent--substitute-near-miss-note staged pattern)
               ;; The lines quoted above are the file's nearest text, so a
               ;; character the pattern demands and the file never holds
               ;; cannot be read off them; the note states it.
               (scalpel-agent--substitute-escaped-literal-note
                staged pattern))))
    ;; Second pass: apply through the visiting buffers and save, the
    ;; way `scalpel-execute' writes.
    (let ((lines nil))
      (pcase-dolist (`(,resolved ,old ,new) staged)
        (let ((count 0)
              (pos 0)
              (excerpts nil)
              ;; The count has to read the pattern exactly as the
              ;; replacement did.  The replacement above runs with case
              ;; folding off, so counting with the buffer's default would
              ;; report occurrences the rewrite never made -- and the
              ;; planner reads that number to decide whether the batch is
              ;; complete.
              (case-fold-search nil))
          (while (string-match pattern old pos)
            (setq count (1+ count)
                  pos (match-end 0)))
          ;; Pair the match lines by index: the Nth match in OLD and the
          ;; Nth match in NEW both come from the same scan order, so the
          ;; Nth line of each names the same occurrence before and after
          ;; the rewrite.
          (let ((new-lines
                 (let ((ls nil)
                       (npos 0))
                   (while (string-match pattern new npos)
                     (push (scalpel-agent--line-at new (match-beginning 0))
                           ls)
                     (setq npos (match-end 0)))
                   (nreverse ls)))
                (old-lines
                 (let ((ls nil)
                       (opos 0))
                   (while (string-match pattern old opos)
                     (push (scalpel-agent--line-at old (match-beginning 0))
                           ls)
                     (setq opos (match-end 0)))
                   (nreverse ls))))
            (setq excerpts
                  (seq-take
                   (cl-pairlis old-lines new-lines)
                   5)))
          ;; The definition listing is taken from the two texts before
          ;; anything is written: it is the only record of what the
          ;; rewrite did to the definitions, and the next round has no
          ;; other way to learn that a name it is about to use is gone.
          ;; It is read from the texts rather than from the buffer, so
          ;; the answer does not depend on which file was written first.
          (let ((before (scalpel-agent--definitions-in-text resolved old))
                (after (scalpel-agent--definitions-in-text resolved new)))
            (with-current-buffer (find-file-noselect resolved)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert new))
              (save-buffer))
            (push (format "Rewrote %d occurrence(s) in %s" count resolved)
                  lines)
            (dolist (pair excerpts)
              (push
               (format "  %s -> %s"
                       (scalpel-agent--excerpt-line (car pair))
                       (scalpel-agent--excerpt-line (cdr pair)))
               lines))
            (when (> count (length excerpts))
              (push (format "  ... and %d more" (- count (length excerpts)))
                    lines))
            (let ((note (scalpel-agent--rewrite-definition-note
                         resolved before after)))
              (when note (push note lines))))))
      (string-join (nreverse lines) "\n"))))

(defun scalpel-agent-confirm (text)
  "Return TEXT as a confirmation request to the user.
This action does not execute any side effects; it yields control
back to the user, who must respond in the next console turn.
The planner must emit this as its final action."
  (unless text
    (user-error "Scalpel: malformed confirm action"))
  text)

(defun scalpel-agent--confirm-needed-p (action)
  "Return non-nil when ACTION must be confirmed before execution.
An unattended run (`scalpel-agent-unattended-confirm' non-nil)
confirms nothing: the user is away, so a prompt would only hang
the run.  Otherwise a file-level tool is always confirmed: it
decides which files exist, which no setting can waive.
`file-create' is deliberately excluded here and from the confirm
gate altogether: the creation is reported in full, so it never
runs with a prompt, regardless of that list.  A tool in
`scalpel-agent-confirm-tools' is confirmed, except a shell action
the planner did not flag as long-running: the sandbox already
bounds what a command may touch, so only the editor-freezing case
needs an answer.  JSON booleans arrive as t and :false; only t
counts as true, so a missing or false flag still asks.  The flag
gates the prompt only: it never relaxes the working directory or
the environment the command runs in."
  (and (not scalpel-agent-unattended-confirm)
       (let ((tool (plist-get action :tool)))
         (and (not (equal tool "file-create"))
              (or (member tool scalpel-agent--file-level-tools)
                  ;; A file-substitute's reach spans every file it
                  ;; names, wider than any single edit; the one
                  ;; confirmation is where the user sees the pattern
                  ;; and the file list together.
                  (equal tool "file-substitute")
                  (and (member tool scalpel-agent-confirm-tools)
                       (not (and (equal tool "shell")
                                 (not (eq (plist-get action :long-running)
                                          t))))))))))

(defun scalpel-agent--action-summary (action)
  "Return a one-line description of ACTION for the confirmation prompt.
Prefix the description with the action's tool name so the user can
tell what the prompt is asking about.  Prefer the action's target
over its stated reason; fall back to `:reason' when the action has
no target."
  (let ((tool (plist-get action :tool))
        (detail
         (or (plist-get action :command)
             (when (plist-get action :pattern)
               (format "replace %s in %d file(s)"
                       (plist-get action :pattern)
                       (length (plist-get action :files))))
             (plist-get action :text)
             (let ((file (plist-get action :file))
                   (symbol (plist-get action :symbol))
                   (to (plist-get action :to)))
               (cond ((and file to) (format "%s -> %s" file to))
                     ((and file symbol) (format "%s in %s" symbol file))
                     (file file)))
             (plist-get action :reason)
             "no reason")))
    (if tool
        (format "%s: %s" tool detail)
      detail)))

(defun scalpel-agent-execute-action (action on-success on-error)
  "Execute a single ACTION plist, without blocking.
ON-SUCCESS receives the report string.  ON-ERROR receives a plist
\(:type SYMBOL :message STRING); the type is `sandbox' when a shell
action was refused by the sandbox, so a caller can keep the
boundary out of the conversation.  A declined confirmation is a
successful outcome: ON-SUCCESS receives a report stating that the
action was declined by the user and did not run, so the caller's
loop continues; ON-ERROR is not used for user declines.  Actions
that issue no LLM request (`reply', `confirm', `file-create',
`block-delete', `file-rename', `file-delete', `file-peek',
`shell') settle synchronously; `block-edit' and `block-insert'
settle from their LLM's callback.  ON-SUCCESS and ON-ERROR run
outside the internal error guard, so an error they raise escapes
instead of being re-framed as an action failure."
  (cl-block scalpel-agent-execute-action
    (let ((tool (plist-get action :tool)))
      (unless (member tool scalpel-agent--tool-vocabulary)
        (funcall on-error
                 (list :type 'unknown-tool
                       :message (format "Scalpel: unknown action tool %S" tool)))
        (cl-return-from scalpel-agent-execute-action))
      (when (scalpel-agent--confirm-needed-p action)
        (unless (yes-or-no-p (format "Execute %s action: %s?"
                                     tool
                                     (scalpel-agent--action-summary action)))
          (funcall on-success
                   (format "The %s action was declined by the user and did not run; continue without it or find another way."
                           tool))
          (cl-return-from scalpel-agent-execute-action)))
      (pcase tool
        ("block-edit"
         (scalpel-agent-block-edit
          (plist-get action :file)
          (plist-get action :symbol)
          (plist-get action :instruction)
          on-success on-error))
        ("block-insert"
         (scalpel-agent-block-insert
          (plist-get action :file)
          (plist-get action :symbol)
          (plist-get action :instruction)
          (plist-get action :after)
          on-success on-error))
        ("file-create"
         (let ((report (condition-case err
                         (scalpel-agent-file-create
                          (plist-get action :file)
                          (plist-get action :text))
                       (error
                        (funcall on-error
                                 (list :type 'action
                                       :message (error-message-string err)))
                        nil))))
           (when report (funcall on-success report))))
        ("block-delete"
         (let ((report (condition-case err
                           (scalpel-agent-block-delete
                            (plist-get action :file)
                            (plist-get action :symbol))
                         (error
                          (funcall on-error
                                   (list :type 'action
                                         :message (error-message-string err)))
                          nil))))
           (when report (funcall on-success report))))
        ("file-rename"
         (let ((report (condition-case err
                           (scalpel-agent-file-rename
                            (plist-get action :file)
                            (plist-get action :to))
                         (error
                          (funcall on-error
                                   (list :type 'action
                                         :message (error-message-string err)))
                          nil))))
           (when report (funcall on-success report))))
        ("file-delete"
         (let ((report (condition-case err
                           (scalpel-agent-file-delete
                            (plist-get action :file))
                         (error
                          (funcall on-error
                                   (list :type 'action
                                         :message (error-message-string err)))
                          nil))))
           (when report (funcall on-success report))))
        ("file-peek"
         (let ((report (condition-case err
                           (scalpel-agent-file-read
                            (plist-get action :file)
                            (plist-get action :symbol))
                         (error
                          (funcall on-error
                                   (list :type 'action
                                         :message (error-message-string err)))
                          nil))))
           (when report (funcall on-success report))))
        ("file-substitute"
         (let ((report (condition-case err
                          (scalpel-agent-file-substitute
                           (plist-get action :files)
                           (plist-get action :pattern)
                           (plist-get action :replacement))
                        (scalpel-no-validation
                         (funcall on-error
                                  (list :type 'no-validation
                                        :message (error-message-string err)))
                         nil)
                        (error
                         (funcall on-error
                                  (list :type 'action
                                        :message (error-message-string err)))
                         nil))))
          (when report (funcall on-success report))))
        ("shell"
         (let ((report (condition-case err
                           (scalpel-agent-shell
                            (plist-get action :command)
                            (plist-get action :reason))
                         (error
                          (funcall on-error
                                   (list :type
                                         (if (eq (car err) 'scalpel-sandbox-error)
                                             'sandbox
                                           'action)
                                         :message (error-message-string err)))
                          nil))))
           (when report (funcall on-success report))))
        ("confirm"
         (funcall on-success
                  (scalpel-agent-confirm (plist-get action :text))))
        ("reply"
         (let ((text (plist-get action :text)))
           (if text
               (funcall on-success (format "%s" text))
             (funcall on-error
                      (list :type 'action
                            :message "Scalpel: reply action missing :text")))))
        (_
         (funcall on-success (format "Unknown action: %S" tool)))))))

(defun scalpel-agent-run (instruction history on-done on-error)
  "Run one agent round for INSTRUCTION, without blocking.
HISTORY is the conversation text recorded before INSTRUCTION, or
nil.  ON-DONE receives a plist (:report STRING :shells SHELLS :reads
READS :changes CHANGES);
SHELLS holds one entry per shell action the round executed, as a
plist with :command plus the output metadata recorded by
`scalpel-agent-shell' \(:bytes, :truncated, :binary), so a caller
can tell whether the round produced output worth reading back.
READS holds one entry per file-peek action, with :file and
:symbol,
and is a separate list because a read report is not subject to the
shell rules: there is no size gate, and no binary downgrade beyond
withholding the contents.
CHANGES holds one report per action in
`scalpel-agent--change-tools' -- every action that wrote to a file --
so a caller can tell a round that changed something from one that
only looked, and continue the loop so the planner can read its own
change back.  A round whose only action created, renamed or deleted
a file reports it here, and nothing else.
ON-ERROR receives a plist (:type SYMBOL :message STRING).

Actions are executed in array order; a `block-edit' or
`block-insert' action settles from its own LLM callback, so actions
stay serialized while
the caller's command loop keeps running.  Every callback runs in
the buffer that was current when this function was called, so the
session's buffer-local context and `scalpel-agent--shell-output'
are read and written consistently.  One round is one
request/execute cycle, not a whole conversation: the caller owns
the loop and the history.  If the buffer this function was called
in is killed while a request is in flight, the round stops and
ON-ERROR is called with :type `session', so a caller holding a
busy flag still gets a chance to release it."
  (let ((session (current-buffer)))
    (cl-macrolet
        ((in-session (&rest body)
           ;; Run BODY with SESSION current.  A killed session buffer
           ;; is reported through ON-ERROR instead: a caller's busy
           ;; guard is released only by a terminal callback, and the
           ;; buffer may no longer exist to receive the report.
           `(progn
              (unless (buffer-live-p session)
                (funcall on-error
                         (list :type 'session
                               :message "Scalpel: session buffer was killed")))
              (when (buffer-live-p session)
                (with-current-buffer session ,@body)))))
      (scalpel-agent-plan
       instruction history
       (lambda (actions)
         (in-session
           (let ((reports nil)
                 (shells nil)
                 (reads nil)
                 (changes nil))
             (cl-labels
                 ((finish ()
                    (funcall on-done
                             (list :report
                                   (string-join (nreverse reports) "\n")
                                   :shells (nreverse shells)
                                   :reads (nreverse reads)
                                   :changes (nreverse changes))))
                  (step (rest)
                    (message "Scalpel-debug: step, %d action(s) left" (length rest))
                    (if (null rest)
                        (finish)
                      (let ((action (car rest)))
                        (setq scalpel-agent--shell-output nil)
                        (scalpel-agent-execute-action
                         action
                         (lambda (report)
                           (in-session
                            (push report reports)
                            (cond
                             ((equal (plist-get action :tool) "shell")
                              (push (append (list :command
                                                  (plist-get action :command))
                                            scalpel-agent--shell-output)
                                    shells))
                             ((equal (plist-get action :tool) "file-peek")
                              (push (list :file (plist-get action :file)
                                          :symbol (plist-get action :symbol))
                                    reads))
                            ((member (plist-get action :tool)
                                     scalpel-agent--change-tools)
                             (push report changes)))
                            (step (cdr rest))))
                         (lambda (err)
                           (in-session
                            (funcall on-error err))))))))
               (step actions)))))
       (lambda (err)
         (message "Scalpel-agent: forwarding error: %s" (plist-get err :message))
         (in-session
          (funcall on-error err)))))))

(provide 'scalpel-agent)

;;; scalpel-agent.el ends here
