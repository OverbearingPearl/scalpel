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

(defvar scalpel-console-buffer-name "*scalpel*"
  "Buffer name for the Scalpel console.")

(defvar scalpel-console-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'scalpel-console-send-line)
    (define-key map (kbd "C-c C-b") #'scalpel-console-set-backend)
    map)
  "Keymap used in Scalpel console buffers.")

(define-derived-mode scalpel-console-mode text-mode "Scalpel Console"
  "Major mode for Scalpel's interactive console buffer.

Type a natural-language instruction on its own line and press RET.
The agent will process the instruction and append its reply to this buffer."
  (setq-local electric-indent-mode nil)
  (setq-local comment-start "")
  (setq buffer-read-only nil))

(defun scalpel-console-open ()
  "Open (or switch to) the Scalpel console buffer and clear its contents."
  (interactive)
  (let ((buf (get-buffer-create scalpel-console-buffer-name)))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (unless (eq major-mode 'scalpel-console-mode)
      (scalpel-console-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (insert "Scalpel console.\n")
      (insert "Type an instruction on its own line and press RET.\n")
      (insert "\n")
      (goto-char (point-max)))))

(defun scalpel-console--add-text (text)
  "Insert TEXT at the end of the Scalpel console buffer."
  (when (get-buffer scalpel-console-buffer-name)
    (with-current-buffer scalpel-console-buffer-name
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)
        (goto-char (point-max))))))

(defun scalpel-console-set-backend ()
  "Interactively switch the gptel backend used for future Scalpel requests."
  (interactive)
  (require 'gptel)
  (call-interactively #'gptel-menu))

(defun scalpel-console-send-line ()
  "Send the current line to the Scalpel agent and append the reply."
  (interactive)
  (let ((buf (get-buffer-create scalpel-console-buffer-name)))
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
          (condition-case err
              (let ((report (scalpel-agent-run instr)))
                (let ((inhibit-read-only t))
                  (insert (format "Scalpel: %s\n\n" report))))
            (error
             (let ((inhibit-read-only t))
               (insert (format "Scalpel error: %s\n\n"
                               (error-message-string err))))))
          (goto-char (point-max)))))))

(defun scalpel-console-ask ()
  "Query the agent using the line under point in the console buffer.

If called while point is on an empty line, fall back to reading an
instruction from the minibuffer using `read-string'.

This command is provided as a convenience for users who prefer to invoke
the agent from any buffer; the canonical interaction is to type directly
in `*scalpel*' and press RET."
  (interactive)
  (let ((buf (get-buffer-create scalpel-console-buffer-name)))
    (if (eq (current-buffer) buf)
        (scalpel-console-send-line)
      (let ((instr (read-string "Scalpel instruction: ")))
        (scalpel-console--add-text (format "\nUser: %s\n" instr))
        (let ((report (scalpel-agent-run instr)))
          (scalpel-console--add-text (format "Scalpel: %s\n\n" report)))))))

(provide 'scalpel-console)

;;; scalpel-console.el ends here
