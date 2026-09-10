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
  (if (derived-mode-p 'scalpel-console-mode)
      (current-buffer)
    (let ((dir (file-name-as-directory (expand-file-name default-directory)))
          found)
      (dolist (buf (buffer-list))
        (unless found
          (with-current-buffer buf
            (when (and (bound-and-true-p scalpel-console--root)
                       (string-prefix-p scalpel-console--root dir))
              (setq found buf)))))
      (or found
          (user-error "Scalpel: no console for %s; run `scalpel-open'" dir)))))

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

(defconst scalpel-console--spinner-frames ["|" "/" "-" "\\"]
  "Frames for the busy spinner shown while waiting for the LLM.")

(defun scalpel-console--spinner-start ()
  "Insert a spinner line at point-max.
Return a cons (ADVANCE . STOP) of zero-arg functions: ADVANCE rotates
the frame char, STOP removes the whole spinner line."
  (let ((inhibit-read-only t)
        (i 0))
    (goto-char (point-max))
    (insert "Scalpel: thinking |")
    (let* ((beg (copy-marker (- (point) 1) nil))
           (label (copy-marker (- (marker-position beg)
                                  (length "Scalpel: thinking ")) nil))
           (advance
            (lambda ()
              (when (marker-buffer beg)
                (let ((inhibit-read-only t))
                  (save-excursion
                    (goto-char beg)
                    (delete-char 1)
                    (insert (aref scalpel-console--spinner-frames
                                  (setq i (% (1+ i)
                                             (length
                                              scalpel-console--spinner-frames))))))))))
           (stop
            (lambda ()
              (when (marker-buffer beg)
                (with-current-buffer (marker-buffer beg)
                  (let ((inhibit-read-only t))
                    (delete-region label (1+ (marker-position beg)))))
                (set-marker beg nil)
                (set-marker label nil)))))
      (cons advance stop))))

(define-derived-mode scalpel-console-mode text-mode "Scalpel Console"
  "Major mode for Scalpel's interactive console buffer.

Type a natural-language instruction on its own line and press RET.
The agent will process the instruction and append its reply to this buffer."
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

(defun scalpel-console--show-context ()
  "Append a context summary to the console buffer.
Paths are shown relative to the console root when available.
Files git would ignore are marked in the tree."
  (let* ((root (and (bound-and-true-p scalpel-console--root)
                    scalpel-console--root))
         (ignored (scalpel-agent--git-ignored-files
                   (append scalpel-agent--context-files
                           scalpel-agent--context-readonly-files)))
         (summary (scalpel-agent-context-summary root ignored)))
    (scalpel-console--append
     (if (string= summary "none")
         "Context: none"
       (concat "Context:\n" summary)))))

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
The console is anchored to `default-directory' at call time: the
buffer name embeds the path and the buffer's `default-directory' is
pinned to it, so any file-system command run inside the console uses
that path."
  (interactive)
  (let* ((root (file-name-as-directory (expand-file-name default-directory)))
         (buf (get-buffer-create (scalpel-console--buffer-name root))))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (unless (eq major-mode 'scalpel-console-mode)
      (scalpel-console-mode))
    (setq-local scalpel-console--root root)
    (setq-local default-directory root)
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
            ;; Close the input line before echoing the user message
            (goto-char (line-end-position))
            (newline)
            (let ((inhibit-read-only t))
              (insert (format "User: %s\n" instr)))
            (let ((scalpel-console--busy t)
                  (spinner (scalpel-console--spinner-start)))
              (let ((advance (car spinner))
                    (stop (cdr spinner)))
                (unwind-protect
                    (condition-case err
                        (let* ((scalpel-llm--progress-callback advance)
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
