;;; scalpel-commit.el --- LLM-assisted git commit messages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; One interactive command, `scalpel-commit', prepares a commit from
;; the working tree of the console's project.  The flow:

;; 1. Staged and unstaged changes to tracked files are collected with
;;    `git add -u' plus a full-context diff; untracked files stay out.
;; 2. The first commit from a console asks for a message style
;;    (angular or linux) and a language; the answers become that
;;    console's defaults and survive a module reload as session
;;    variables.
;; 3. The diff is sent to the LLM with a prompt encoding house rules
;;    (imperative subject, subject length, body width) and the reply
;;    is shown in a dedicated *scalpel commit* buffer.
;; 4. The user confirms with C-c C-c, which re-checks the tree before
;;    running `git commit', or tunes the message with the buffer's
;;    local keys.  Nothing is committed without that confirmation.

;; The commit buffer has its own major mode, `scalpel-commit-mode',
;; so its keymap never collides with the console's: C-c C-w, C-c C-s,
;; C-c C-n, C-c C-e and C-c C-t mean detail, brevity, regeneration,
;; style switch and language switch only here.

;;; Code:

(require 'cl-lib)
(require 'scalpel-prompt-commit)

(defun scalpel-commit-run ()
  "Prepare a commit for the current console's project with an LLM message.
Staged and unstaged tracked changes are committed; untracked files
are left alone.  The first commit from a console asks for a style
and a language and keeps both as that console's defaults; later
commits reuse them.  The message is shown in the commit buffer for
confirmation."
  (interactive)
  (unless (bound-and-true-p scalpel-console--root)
    (user-error "Scalpel: run this from a scalpel console"))
  (let ((console (current-buffer))
        (workdir scalpel-console--root))
    (scalpel-commit--style)
    (scalpel-commit--language)
    (with-current-buffer console
      (when scalpel-commit--busy
        (message "Scalpel: request rejected; another commit message generation is already in progress in this console")
        (user-error "Scalpel: a commit is already running in this console"))
      (setq scalpel-commit--busy t))
    (message "Scalpel: preparing commit message...")
    (scalpel-commit--generate console workdir nil)))

(defcustom scalpel-commit-subject-max 72
  "Maximum characters the generated subject line may run to."
  :type 'natnum
  :group 'scalpel-commit)

(defcustom scalpel-commit-body-width 72
  "Maximum width in characters of the generated body."
  :type 'natnum
  :group 'scalpel-commit)

(defcustom scalpel-commit-diff-max-bytes 200000
  "Largest diff handed to the LLM in one request.
When the diff exceeds this, files are split into batches and the
message is generated per batch, so no function name is cut away."
  :type 'natnum
  :group 'scalpel-commit)

(progn
  (defvar scalpel-commit--workdir-cache nil
    "Alist mapping commit-ish to cached workdir info for scalpel-commit buffers.")
  (defvar scalpel-commit--console nil
    "Console buffer for scalpel-commit operations."))

(defun scalpel-commit--buffer-name (&optional root)
  "Return the name of the buffer holding the generated commit message.
The buffer name is per-repository: it embeds the base name of the
repository root, so two repos never share a commit buffer.  With
non-nil ROOT use it directly; otherwise fall back to the root from
`scalpel-commit--workdir-cache', or from `scalpel-console--root'
in the live `scalpel-commit--console' buffer.  When no root is
available the generic fallback name \"*scalpel commit*\" is used."
  (let ((root (or root
                  scalpel-commit--workdir-cache
                  (when (buffer-live-p scalpel-commit--console)
                    (buffer-local-value 'scalpel-console--root
                                        scalpel-commit--console)))))
    (if root
        (format "*scalpel commit: %s*"
                (file-name-nondirectory (directory-file-name root)))
      "*scalpel commit*")))

(defconst scalpel-commit--styles '(angular linux)
  "Supported commit message styles, in preference order.
`angular' is the conventional-commits shape (type(scope): subject
with a body and footer); `linux' is the kernel shape (plain subject
line, prose body, signed-off style lines left to git).")

(defvar-local scalpel-commit--style nil
  "Style for this commit, asked once if needed.
Bound in the console buffer; part of the console's session state.
Nil until the first commit asks for it.")

(defvar-local scalpel-commit--language nil
  "Language the generated commit message is written in.
A string such as \"English\" or \"Chinese\"; bound in the console
buffer and part of its session state.  Nil until the first commit
asks for it.")

(defvar-local scalpel-commit--tree-state nil
  "Snapshot of `git status --porcelain' taken when the diff was read.
The confirm key re-runs the status and compares, so a tree the user
touched while waiting is caught instead of silently committed.")

(defvar-local scalpel-commit--request-extra nil
  "Extra instruction carried on a regeneration request.
A string such as \"more detail\" or \"shorter\", or a style or
language switch; nil on the first generation.")

(defvar-local scalpel-commit--console nil
  "Console buffer this commit was started from.
Answers read and written through here land in the right console
when two consoles commit at once.")

(defvar-local scalpel-commit--busy nil
  "Non-nil while a commit generation is in flight for this console.
Prevents a second `scalpel-commit-run' from the same console.")

(progn
  (defvar scalpel-commit--style)
  (defvar scalpel-commit--language))

(defvar-local scalpel-commit--workdir-cache nil
  "The repository root (a directory string) this commit serves.
Captured in the commit buffer at generation time, so regeneration
and confirming the commit never depend on the console buffer still
being alive.  Nil until a message is generated.")

(defvar-local scalpel-commit--untracked-files nil
  "List of untracked path strings captured when the commit message was generated.
Display-only; these files are never staged or included in the commit.
Nil until a message is generated by `scalpel-commit--generate'.")

(defun scalpel-commit--git (args &optional workdir)
  "Run `git' with ARGS in WORKDIR and return its output, or nil.
WORKDIR defaults to the current buffer's directory.  Output is
trimmed; a non-zero exit, a missing WORKDIR, or any other error
returns nil, so callers can treat any failure as \"not a
repository\" uniformly."
  (condition-case nil
      (let ((default-directory (or workdir default-directory)))
        (with-temp-buffer
          (and (zerop (apply #'call-process "git" nil t nil args))
               (string-trim (buffer-string)))))
    (error nil)))

(defun scalpel-commit--status (workdir)
  "Return the porcelain status of WORKDIR, or nil outside a repo."
  (scalpel-commit--git (list "status" "--porcelain") workdir))

(defun scalpel-commit--untracked (workdir)
  "Return the list of untracked files in WORKDIR.
These paths are shown for information only and are never part
of the commit.  Run `git ls-files --others --exclude-standard'
and split the trimmed output on newlines, returning nil when
there is no output."
  (let ((output (scalpel-commit--git (list "ls-files" "--others" "--exclude-standard") workdir)))
    (when (and output (not (string-empty-p (string-trim output))))
      (split-string (string-trim output) "\n"))))

(defun scalpel-commit--tree-changed-p (workdir)
  "Return non-nil when WORKDIR's status differs from the snapshot.
The snapshot was taken when the commit message was generated, so a
difference means the user edited something while waiting and the
message may no longer describe the tree."
  (not (equal (scalpel-commit--status workdir)
              scalpel-commit--tree-state)))

(defun scalpel-commit--diff (workdir)
  "Return the diff the message must describe for WORKDIR.
Staged and unstaged changes to tracked files, together, are what
the commit will carry: `git add -u' first stages them, so one diff
covers both.  Untracked files are left alone by -u and so stay out.
Function context and a wide -U are on the diff so a hunk always
names the function it touches; the message must never guess."
  (scalpel-commit--git (list "add" "-u") workdir)
  (scalpel-commit--git (list "diff" "--staged" "--cached"
                             "-U10" "--function-context"
                             "--no-color" "--no-ext-diff")
                       workdir))

(defun scalpel-commit--diff-batches (diff)
  "Split DIFF into pieces no larger than `scalpel-commit-diff-max-bytes'.
Each piece starts at a file header and ends on a file boundary, so
every file's hunks travel together; a single file larger than the
limit travels whole as its own piece."
  (let* ((limit scalpel-commit-diff-max-bytes)
         (len (length diff))
         (starts nil)
         (pos 0))
    ;; Pass 1: collect file-header start offsets.  The first chunk
    ;; starts at 0; each subsequent match position is a boundary.
    ;; POS advances by (match-end 0) each iteration, so the loop
    ;; always terminates.
    (push 0 starts)
    (while (string-match-p "^diff --git " diff pos)
      (let ((m (string-match-p "^diff --git " diff pos)))
        (when (> m 0)
          (push m starts))
        (setq pos (+ m (length "diff --git ")))))
    (setq starts (nreverse starts))
    ;; Append LEN as the final boundary.
    (setq starts (append starts (list len)))
    ;; Pass 2: build whole-file chunks from consecutive boundaries,
    ;; then greedily concat them into pieces under the limit.
    (let ((pieces nil)
          (current nil)
          (size 0))
      (let ((chunks nil))
        (let ((i 0))
          (while (< i (1- (length starts)))
            (let ((start (nth i starts))
                  (end (nth (1+ i) starts)))
              (push (substring diff start end) chunks))
            (setq i (1+ i))))
        (setq chunks (nreverse chunks))
        (dolist (chunk chunks)
          (let ((csize (length chunk)))
            (cond
             ((null current)
              (setq current chunk size csize))
             ((<= (+ size csize) limit)
              (setq current (concat current chunk)
                    size (+ size csize)))
             (t
              (push current pieces)
              (setq current chunk size csize)))))
        (when current
          (push current pieces))
        (nreverse pieces)))))

(defun scalpel-commit--style-prompt (style)
  "Return the style rules for STYLE, one of `scalpel-commit--styles'."
  (scalpel-prompt-commit-style-prompt style))

(defun scalpel-commit--build-prompt (diff style language extra)
  "Build the commit message request from DIFF, STYLE, LANGUAGE and EXTRA.
EXTRA carries the regeneration instruction, such as a request for
more detail or a style or language switch, so the model changes its
answer instead of repeating it."
  (scalpel-prompt-commit-build-prompt
   diff style language extra
   scalpel-commit-subject-max
   scalpel-commit-body-width))

(defun scalpel-commit--ask-style ()
  "Ask which commit style to use, and remember the answer."
  (let* ((descriptions
          '((angular . "Conventional Commits: type(scope): subject, body explains changes")
            (linux . "plain subject, blank line, body; no type prefix")))
         (candidates
          (mapcar
           (lambda (style)
             (cons (format "%s  %s"
                           (capitalize (symbol-name style))
                           (or (cdr (assq style descriptions))
                               "no description"))
                   style))
           scalpel-commit--styles))
         (choice
          (completing-read "Commit style: "
                           (mapcar #'car candidates)
                           nil t))
         (style
          (cdr (assoc choice candidates))))
    (setq scalpel-commit--style style)
    style))

(defun scalpel-commit--ask-language ()
  "Ask which language the message is written in, and remember it."
  (let ((language
         (completing-read "Commit message language (e.g. English): "
                          nil nil nil nil nil "English")))
    (setq scalpel-commit--language (and (stringp language)
                                        (not (string-empty-p language))
                                        language))
    scalpel-commit--language))

(defun scalpel-commit--session-install (console style language)
  "Store STYLE and LANGUAGE as CONSOLE's commit defaults.
Called with the answers from the first commit; later commits read
them without asking."
  (when (buffer-live-p console)
    (with-current-buffer console
      (setq scalpel-commit--style style
            scalpel-commit--language language))))

(defun scalpel-commit--insert-message (message)
  "Replace the buffer's message body with MESSAGE.
The body is delimited by the \"--- BEGIN COMMIT MESSAGE ---\" and
\"--- END COMMIT MESSAGE ---\" marker lines.  Everything between
them is replaced, so regenerations never pile up; the header lines
above the BEGIN marker stay untouched.  If the BEGIN marker is
missing, it is inserted at point-max so subsequent regenerations
can find it."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (if (re-search-forward "^--- BEGIN COMMIT MESSAGE ---\n" nil t)
        (let ((beg (point)))
          (if (re-search-forward "^--- END COMMIT MESSAGE ---\n" nil t)
              (delete-region beg (match-beginning 0))
            (goto-char (point-max))
            (delete-region beg (point-max)))
          (insert message "\n")
          (insert (propertize "--- END COMMIT MESSAGE ---\n" 'face 'shadow))
          (goto-char (point-max)))
      (goto-char (point-max))
      (insert (propertize "--- BEGIN COMMIT MESSAGE ---\n" 'face 'shadow))
      (insert message "\n")
      (insert (propertize "--- END COMMIT MESSAGE ---\n" 'face 'shadow))
      (goto-char (point-max)))))

(defun scalpel-commit--render (message)
  "Render MESSAGE in the commit buffer, creating it when needed.
Erases the buffer and lays it out as: a shadow-propertized header
containing the title line and the key hints, one blank line, then the
message body delimited by the shadow-faced \"--- BEGIN COMMIT MESSAGE
---\" and \"--- END COMMIT MESSAGE ---\" markers (both laid down by
`scalpel-commit--insert-message').  Only when
`scalpel-commit--untracked-files' is non-nil, a blank line followed by a
shadow-propertized untracked-files section is appended after the
message.  Everything outside the message body is shadow-faced."
  (let ((buffer (get-buffer-create (scalpel-commit--buffer-name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'scalpel-commit-mode)
        (scalpel-commit-mode))
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (erase-buffer)
        (insert (propertize "Commit message\n" 'face 'shadow))
        (insert (propertize "C-c C-c commit   C-c C-w more detail   "
                            'face 'shadow))
        (insert (propertize "C-c C-s shorter   C-c C-n regenerate\n"
                            'face 'shadow))
        (insert (propertize "C-c C-e switch style   C-c C-t switch language   "
                            'face 'shadow))
        (insert (propertize "C-c C-k abort\n" 'face 'shadow))
        (insert "\n"))
      (scalpel-commit--insert-message message)
      (when scalpel-commit--untracked-files
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (save-excursion
            (goto-char (point-max))
            (insert (propertize "\nUntracked (not in commit):\n"
                                'face 'shadow))
            (dolist (file scalpel-commit--untracked-files)
              (insert (propertize (format "%s\n" file)
                                  'face 'shadow)))))))
    (pop-to-buffer buffer)
    buffer))

(defun scalpel-commit--generate (console workdir extra)
  "Ask the LLM for a message and show it in the commit buffer.
CONSOLE is the console that owns the choices; WORKDIR is the
working directory to diff; EXTRA is the regeneration instruction
or nil."
  (let* ((style (if (buffer-live-p console)
                    (buffer-local-value 'scalpel-commit--style console)
                  nil))
         (language (if (buffer-live-p console)
                       (buffer-local-value 'scalpel-commit--language console)
                     nil))
         (diff (when workdir (scalpel-commit--diff workdir))))
    (unless workdir
      (user-error "Scalpel: console has no root directory"))
    (unless (and diff (not (string-empty-p diff)))
      (user-error "Scalpel: nothing staged or unstaged to commit"))
    (when (buffer-live-p console)
      (with-current-buffer console
        (setq scalpel-commit--style style)
        (setq scalpel-commit--language language)
        (setq scalpel-commit--tree-state (scalpel-commit--status workdir))))
    (scalpel-llm-request-async
     (scalpel-commit--build-prompt diff style language extra)
     (lambda (message)
       (with-current-buffer (get-buffer-create (scalpel-commit--buffer-name workdir))
         (unless (derived-mode-p 'scalpel-commit-mode)
           (scalpel-commit-mode))
         (setq scalpel-commit--console console)
         (setq scalpel-commit--style style)
         (setq scalpel-commit--language language)
         (setq scalpel-commit--request-extra extra)
         (setq scalpel-commit--workdir-cache workdir)
         (setq scalpel-commit--untracked-files (scalpel-commit--untracked workdir))
         (setq scalpel-commit--tree-state
               (when (buffer-live-p console)
                 (with-current-buffer console scalpel-commit--tree-state)))
         (rename-buffer (scalpel-commit--buffer-name) t)
         (scalpel-commit--render message))
       (message "Scalpel: commit message generation finished.")
       (when (buffer-live-p console)
         (with-current-buffer console
           (setq scalpel-commit--busy nil))))
     (lambda (payload)
       (when (buffer-live-p console)
         (with-current-buffer console
           (setq scalpel-commit--busy nil)))
       (message "Scalpel: commit message generation failed: %s"
                (plist-get payload :message)))
     (concat "You write git commit messages and nothing else. "
             "Reply with the message text only."))))

(defun scalpel-commit--workdir ()
  "Return the repository root the commit buffer serves.
The root is captured at generation time in `scalpel-commit--workdir-cache',
so confirming or regenerating a commit keeps working even if the console
buffer has since been killed.  Fall back to the console buffer's
`scalpel-console--root' only when the cache is empty."
  (or scalpel-commit--workdir-cache
      (and (buffer-live-p scalpel-commit--console)
           (buffer-local-value 'scalpel-console--root scalpel-commit--console))
      (user-error "Scalpel: the console that started this commit is gone")))

(defun scalpel-commit--style ()
  "Return the style this commit should use, asking once if needed."
  (or scalpel-commit--style
      (scalpel-commit--ask-style)))

(defun scalpel-commit--language ()
  "Return the language this commit should use, asking once if needed."
  (or scalpel-commit--language
      (scalpel-commit--ask-language)))

(defun scalpel-commit--regenerate (extra)
  "Regenerate the message with EXTRA as the new instruction.
Use EXTRA as the new instruction."
  (when (or scalpel-commit--busy
            (and (buffer-live-p scalpel-commit--console)
                 (buffer-local-value 'scalpel-commit--busy
                                     scalpel-commit--console)))
    (message "Scalpel: commit message generation rejected: another generation is already in progress")
    (user-error "A commit message generation is already in progress"))
  (setq scalpel-commit--request-extra extra)
  (let ((workdir scalpel-commit--workdir-cache))
    (unless workdir
      (user-error "No commit message has been generated yet"))
    (message "Scalpel: starting commit message generation...")
    (scalpel-commit--generate scalpel-commit--console workdir extra)))

(defun scalpel-commit--abort ()
  "Abandon this commit and close its buffer."
  (interactive)
  (when-let ((buffer (get-buffer (scalpel-commit--buffer-name))))
    (kill-buffer buffer)))

(defun scalpel-commit--commit ()
  "Run `git commit' with the message shown in the buffer.
The tree is re-checked first: a file changed since the message was
generated means the message may be stale, and the user is told to
regenerate rather than commit something undescribed.  The commit
carries staged and unstaged tracked changes (the diff ran
`git add -u'); untracked files stay out, as the feature promises.
After committing, the status is re-read: only a non-nil status that
still reports a tracked change (a line not starting with `??') means
the commit failed; a nil status (git error or a stubbed read in
tests) counts as success and the user is never asked to commit
manually on its account."
  (interactive)
  (let ((workdir (scalpel-commit--workdir)))
    (when (scalpel-commit--tree-changed-p workdir)
      (user-error
       (concat "Scalpel: the working tree changed since this message "
               "was generated; regenerate with C-c C-n first")))
    (let ((message (scalpel-commit--message-text)))
      (unless (and message (not (string-empty-p message)))
        (user-error "Scalpel: no commit message to commit"))
      (unless (scalpel-commit--git
               (list "commit" "-m" message)
               workdir)
        (let ((err (with-temp-buffer
                     (apply #'call-process "git" nil t nil
                            (append (list "-C" workdir
                                          "commit" "-m" message)))
                     (buffer-string))))
          (user-error "Scalpel: git commit failed:\n%s" err)))
      (message "Scalpel: committed")
      (scalpel-commit--abort))))

(defun scalpel-commit--message-text ()
  "Return the message body between the begin/end commit message markers, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^--- BEGIN COMMIT MESSAGE ---\n" nil t)
      (let ((beg (point)))
        (when (re-search-forward "^--- END COMMIT MESSAGE ---\n?" nil t)
          (string-trim (buffer-substring beg (match-beginning 0))))))))

(defun scalpel-commit--more-detail ()
  "Ask for a more detailed version of the message."
  (interactive)
  (scalpel-commit--regenerate "more detail is wanted"))

(defun scalpel-commit--shorter ()
  "Ask for a more concise version of the message."
  (interactive)
  (scalpel-commit--regenerate "a shorter, tighter message is wanted"))

(defun scalpel-commit--regen ()
  "Regenerate the message with no extra instruction."
  (interactive)
  (scalpel-commit--regenerate nil))

(defun scalpel-commit--switch-style ()
  "Switch to the other style and regenerate; it becomes the default."
  (interactive)
  (let ((new (pcase scalpel-commit--style
               ('angular 'linux)
               ('linux 'angular)
               (_ (car scalpel-commit--styles)))))
    (setq scalpel-commit--style new)
    (when (buffer-live-p scalpel-commit--console)
      (with-current-buffer scalpel-commit--console
        (setq scalpel-commit--style new)))
    (scalpel-commit--regenerate
     (format "the style must be %s, not the previous one"
             (symbol-name new)))))

(defun scalpel-commit--switch-language ()
  "Ask for a new language and regenerate; it becomes the default."
  (interactive)
  (let ((new (scalpel-commit--ask-language)))
    (scalpel-commit--session-install scalpel-commit--console
                                     scalpel-commit--style
                                     new)
    (scalpel-commit--regenerate
     (format "the message must be written in %s" new))))

(defvar scalpel-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'scalpel-commit--commit)
    (define-key map (kbd "C-c C-w") #'scalpel-commit--more-detail)
    (define-key map (kbd "C-c C-s") #'scalpel-commit--shorter)
    (define-key map (kbd "C-c C-n") #'scalpel-commit--regen)
    (define-key map (kbd "C-c C-e") #'scalpel-commit--switch-style)
    (define-key map (kbd "C-c C-t") #'scalpel-commit--switch-language)
    (define-key map (kbd "C-c C-k") #'scalpel-commit--abort)
    map)
  "Keys of the commit message buffer.
\\<scalpel-commit-mode-map>
\\[scalpel-commit--commit] commits the message.
\\[scalpel-commit--more-detail] asks for more detail.
\\[scalpel-commit--shorter] asks for a shorter message.
\\[scalpel-commit--regen] regenerates the message.
\\[scalpel-commit--switch-style] switches the commit style.
\\[scalpel-commit--switch-language] switches the language.
\\[scalpel-commit--abort] aborts.
These keys belong to this buffer's own map, so they cannot
collide with the console's keys.")

(define-derived-mode scalpel-commit-mode special-mode "Scalpel Commit"
  "Major mode showing a generated commit message awaiting confirmation.
\\<scalpel-commit-mode-map>\\[scalpel-commit--commit] commits; the other keys tune the message and regenerate it:
\\[scalpel-commit--more-detail] more detail, \\[scalpel-commit--shorter] shorter, \\[scalpel-commit--regen] regenerate,
\\[scalpel-commit--switch-style] switch style, \\[scalpel-commit--switch-language] switch language,
\\[scalpel-commit--abort] abort."
  (setq buffer-read-only t))

(provide 'scalpel-commit)

;;; scalpel-commit.el ends here
