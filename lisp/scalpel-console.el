;;; scalpel-console.el --- Persistent interactive agent console for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Provides the *scalpel* buffer where users type a textual instruction and
;; press RET to send it to the agent.  The buffer is a normal editable text
;; buffer, so users can review prior turns.

;;; Code:

(require 'scalpel-agent)

(defcustom scalpel-console-buffer-name-format "*scalpel: %s*"
  "Format string for the Scalpel console buffer name.
The single `%s' is substituted with the abbreviated console root,
so a console opened at /home/me/proj is named \"*scalpel: ~/proj*\"."
  :type 'string
  :group 'scalpel)

(defvar-local scalpel-console--root nil
  "Absolute directory this console session is anchored to.
Set by `scalpel-console-open'; while non-nil, `default-directory'
in the console buffer is pinned to this value.")

(defvar-local scalpel-console--context-baseline 'none-yet
  "Context entries shown by the previous refresh.
The symbol `none-yet' means no refresh has happened in this
console buffer yet, so nothing is highlighted as changed.")

(defface scalpel-console-context-added-face
  '((t :inherit bold))
  "Face for files newly added to the Scalpel context."
  :group 'scalpel)

(defface scalpel-console-context-removed-face
  '((t :inherit shadow :strike-through t))
  "Face for files removed from the Scalpel context."
  :group 'scalpel)

(defface scalpel-console-context-unchanged-face
  '((t :inherit shadow))
  "Face for files unchanged in the Scalpel context."
  :group 'scalpel)

(defun scalpel-console--buffer-name (root)
  "Return the Scalpel console buffer name for ROOT.
ROOT is expanded, normalized with `file-name-as-directory' and
`directory-file-name', then abbreviated with `abbreviate-file-name'."
  (format scalpel-console-buffer-name-format
          (abbreviate-file-name
           (directory-file-name
            (file-name-as-directory (expand-file-name root))))))

(defun scalpel-console--target-buffer ()
  "Return the console buffer the current command should write to.
Return the current buffer when it is a console buffer; otherwise
return the console buffer whose root contains `default-directory',
or signal a `user-error' when there is none."
  (cond
   ((derived-mode-p 'scalpel-console-mode)
    ;; The mode function is reachable via M-x; a buffer entered that way
    ;; has no root, so fail loud instead of anchoring to the caller's
    ;; default-directory.
    (unless scalpel-console--root
      (user-error "Scalpel: console buffer has no root; use `scalpel-open'"))
    (current-buffer))
   (t
    (let ((dir (file-name-as-directory (expand-file-name default-directory)))
          found)
      (dolist (buf (buffer-list))
        (unless found
          (with-current-buffer buf
            (when (and (bound-and-true-p scalpel-console--root)
                       (string-prefix-p scalpel-console--root dir))
              (setq found buf)))))
      (or found
          (user-error "Scalpel: no console for %s; run `scalpel-open'" dir))))))

(defvar scalpel-console-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'scalpel-console-send-line)
    (define-key map (kbd "C-c C-c") #'scalpel-console-send-line)
    (define-key map (kbd "C-c C-b") #'scalpel-llm-select-backend)
    (define-key map (kbd "C-c C-a") #'scalpel-console-add-file)
    (define-key map (kbd "C-c C-o") #'scalpel-console-add-readonly-file)
    (define-key map (kbd "C-c C-d") #'scalpel-console-remove-file)
    (define-key map (kbd "C-c C-r") #'scalpel-console-reset-context)
    map)
  "Keymap used in Scalpel console buffers.")

(defvar scalpel-console--busy nil
  "Non-nil while an agent request is in flight.")

(defun scalpel-console--status-start ()
  "Insert a one-line status display at point-max.
Return a cons (REFRESH . STOP) of zero-arg functions.  REFRESH
rewrites the line with the current token counters and whole
elapsed seconds; STOP removes the line together with its trailing
newline, so the cursor returns to the line the status occupied."
  (let ((inhibit-read-only t)
        (start (float-time))
        beg)
    (goto-char (point-max))
    (setq beg (point-marker))
    (insert (format "Scalpel: %d up, %d down, 0s\n"
                    scalpel-llm--tokens-uploaded
                    scalpel-llm--tokens-received))
    (let ((refresh
           (lambda ()
             (when (marker-buffer beg)
               (with-current-buffer (marker-buffer beg)
                 (let ((inhibit-read-only t))
                   (goto-char beg)
                   (delete-region (point) (1+ (line-end-position)))
                   (insert (format "Scalpel: %d up, %d down, %ds\n"
                                   scalpel-llm--tokens-uploaded
                                   scalpel-llm--tokens-received
                                   (round (- (float-time) start)))))))))
          (stop
           (lambda ()
             (when (marker-buffer beg)
               (with-current-buffer (marker-buffer beg)
                 (let ((inhibit-read-only t))
                   (goto-char beg)
                   (delete-region (point) (1+ (line-end-position)))))
               (set-marker beg nil)))))
      (cons refresh stop))))

(define-derived-mode scalpel-console-mode text-mode "Scalpel Console"
  "Major mode for Scalpel's interactive console buffer.

Type a natural-language instruction on its own line and press RET.
The agent will process the instruction and append its reply to this buffer.
Not a user entry point: open a console with `scalpel-open', which
also anchors the buffer to a root directory."
  (setq-local electric-indent-mode nil)
  (setq-local comment-start "")
  (setq buffer-read-only nil))

(defun scalpel-console--append (text)
  "Append TEXT to the end of the console buffer."
  (let ((buf (scalpel-console--target-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (format "%s\n\n" text)))))))

(defun scalpel-console--render-diff (lines)
  "Return LINES as text with per-name change highlighting.
LINES is a list of display cells as returned by
`scalpel-agent-context-update'.  Added names are bolded; removed
names are dimmed and struck through.  The tree graphics are never
highlighted."
  (mapconcat
   (lambda (cell)
     (let* ((text (plist-get cell :text))
            (start (plist-get cell :name-start))
            (graphics (substring text 0 start))
            (name (substring text start)))
       (pcase (plist-get cell :status)
         ('added (concat graphics
                         (propertize name
                                     'face 'scalpel-console-context-added-face)))
         ('removed (concat graphics
                           (propertize name
                                       'face 'scalpel-console-context-removed-face)))
         ('same (concat graphics
                        (propertize name
                                    'face 'scalpel-console-context-unchanged-face)))
         (_ text))))
   lines
   "\n"))

(defun scalpel-console--show-context ()
  "Append the context tree, marking the delta since the last refresh.
Files git would ignore are marked in the tree.  Lines dropped since
the previous refresh are dimmed and struck through, newly added
lines are bolded.  The baseline is replaced afterwards, so each
delta is highlighted exactly once."
  (with-current-buffer (scalpel-console--target-buffer)
    (let* ((ignored (scalpel-agent--git-ignored-files
                     (append scalpel-agent--context-files
                             scalpel-agent--context-readonly-files)))
           (result (scalpel-agent-context-update
                    scalpel-console--context-baseline ignored))
           (lines (car result)))
      (setq scalpel-console--context-baseline (cdr result))
      (scalpel-console--append
       (if (null lines)
           "Context: none"
         (concat "Context:\n" (scalpel-console--render-diff lines)))))))

(defun scalpel-console-add-file (&optional ignore-gitignore)
  "Prompt for a file or directory and add it as writable context.
With prefix argument IGNORE-GITIGNORE, do not filter directory
expansion through gitignore rules."
  (interactive "P")
  (let ((path (read-file-name "Add to Scalpel context: ")))
    (scalpel-agent-context-add path ignore-gitignore)
    (scalpel-console--show-context)))

(defun scalpel-console-add-readonly-file (&optional ignore-gitignore)
  "Prompt for a file or directory and add it as read-only reference.
Read-only files are shown to the LLM with their full contents and
cannot be edited.  With prefix argument IGNORE-GITIGNORE, do not
filter directory expansion through gitignore rules."
  (interactive "P")
  (let ((path (read-file-name "Add read-only reference: ")))
    (scalpel-agent-context-add-readonly path ignore-gitignore)
    (scalpel-console--show-context)))

(defun scalpel-console-remove-file ()
  "Prompt for a context file or directory and remove it."
  (interactive)
  (let ((candidates (append scalpel-agent--context-files
                            scalpel-agent--context-readonly-files)))
    (if (null candidates)
        (message "Scalpel: context is empty")
      (let ((path (completing-read "Remove from Scalpel context: "
                                   candidates nil nil)))
        (scalpel-agent-context-remove path)
        (scalpel-console--show-context)))))

(defun scalpel-console-reset-context ()
  "Reset the agent context to currently open located files."
  (interactive)
  (scalpel-agent-context-reset)
  (scalpel-console--show-context))

(defun scalpel-console-open ()
  "Open (or switch to) the Scalpel console buffer and clear its contents.
Internal setup routine of `scalpel-open'; not an interactive command.
The console is anchored to `default-directory' at call time: the
buffer name embeds the path and the buffer's `default-directory' is
pinned to it, so any file-system command run inside the console uses
that path."
  (let* ((root (file-name-as-directory (expand-file-name default-directory)))
         (buf (get-buffer-create (scalpel-console--buffer-name root))))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (unless (eq major-mode 'scalpel-console-mode)
      (scalpel-console-mode))
    (setq-local scalpel-console--root root)
    (setq-local default-directory root)
    (setq-local scalpel-console--context-baseline 'none-yet)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (insert "Scalpel console.\n")
      (insert "Type an instruction on its own line and press RET.\n")
      (insert "\n")
      (goto-char (point-max)))
    (scalpel-agent-context-reset)
    (scalpel-console--show-context)
    (goto-char (point-max))))

(defun scalpel-console-send-line ()
  "Send the current line to the Scalpel agent and append the reply."
  (interactive)
  (if scalpel-console--busy
      (progn
        (message "Scalpel: still working on the previous instruction...")
        (ding))
    (let ((buf (scalpel-console--target-buffer)))
      (unless (eq (current-buffer) buf)
        (switch-to-buffer buf))
      (let* ((instr (string-trim
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))))
        (if (string-empty-p instr)
            (message "Scalpel: nothing to send on this line.")
          (progn
            ;; Rewrite the typed input line into the logged user message, so
            ;; the instruction is not shown twice (once raw, once prefixed).
            (let ((inhibit-read-only t)
                  (beg (line-beginning-position))
                  (end (line-end-position)))
              (delete-region beg end)
              (goto-char beg)
              (insert (format "User: %s\n" instr)))
            (let ((scalpel-console--busy t)
                  (status (scalpel-console--status-start)))
              (let ((refresh (car status))
                    (stop (cdr status)))
                (unwind-protect
                    (condition-case err
                        (let* ((scalpel-llm--progress-callback refresh)
                               (report (scalpel-agent-run instr)))
                          (funcall stop)
                          (let ((inhibit-read-only t))
                            (insert (format "Scalpel: %s\n\n" report))))
                      (error
                       (funcall stop)
                       (let ((inhibit-read-only t))
                         (insert (format "Scalpel error: %s\n\n"
                                         (error-message-string err))))))
                  (setq scalpel-console--busy nil))))
            (goto-char (point-max))
            (message "Scalpel: instruction sent.")))))))

(provide 'scalpel-console)

;;; scalpel-console.el ends here
