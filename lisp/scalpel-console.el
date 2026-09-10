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
    (define-key map (kbd "C-c C-b") 'scalpel-set-backend)
    map)
  "Keymap used in Scalpel console buffers.")

(defvar scalpel-console--busy nil
  "Non-nil while an agent request is in flight.")

(defvar scalpel-console--pending-logs nil
  "Log lines deferred while `scalpel-console--busy' is non-nil, newest first.")

(defconst scalpel-console--spinner-frames ["|" "/" "-" "\\"]
  "Frames for the busy spinner shown while waiting for the LLM.")

(defun scalpel-console--spinner-start ()
  "Insert a spinner line at point-max and return a stop function.
Calling the stop function removes the spinner line and cancels the timer."
  (let ((inhibit-read-only t)
        (i 0))
    (goto-char (point-max))
    (insert "Scalpel: thinking |")
    (let ((beg (copy-marker (- (point) 1) nil))
          (end (point-marker)))
      (set-marker beg (- end 1))
      (let ((timer (run-with-timer 0.15 0.15
                     (lambda ()
                       (when (marker-buffer beg)
                         (with-current-buffer (marker-buffer beg)
                           (let ((inhibit-read-only t))
                             (save-excursion
                               (goto-char beg)
                               (delete-char 1)
                               (insert (aref scalpel-console--spinner-frames
                                             (setq i (% (1+ i)
                                                        (length scalpel-console--spinner-frames)))))))))))))
        (lambda ()
          (cancel-timer timer)
          (when (marker-buffer beg)
            (with-current-buffer (marker-buffer beg)
              (let ((inhibit-read-only t))
                (delete-region (- beg (length "Scalpel: thinking ")) end)))
            (set-marker beg nil)
            (set-marker end nil)))))))

(define-derived-mode scalpel-console-mode text-mode "Scalpel Console"
  "Major mode for Scalpel's interactive console buffer.

Type a natural-language instruction on its own line and press RET.
The agent will process the instruction and append its reply to this buffer."
  (setq-local electric-indent-mode nil)
  (setq-local comment-start "")
  (setq buffer-read-only nil))

(defun scalpel-console--append (text)
  "Append TEXT to the end of the console buffer."
  (let ((buf (get-buffer-create scalpel-console-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (format "%s\n\n" text)))))))

(defun scalpel-console--log (text)
  "Append TEXT to the console buffer, or queue it while busy."
  (if scalpel-console--busy
      (push text scalpel-console--pending-logs)
    (scalpel-console--append text)))

(defun scalpel-console-open ()
  "Open (or switch to) the Scalpel console buffer and clear its contents."
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

(defun scalpel-console-send-line ()
  "Send the current line to the Scalpel agent and append the reply."
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
          (let ((scalpel-console--busy t)
                (spinner (scalpel-console--spinner-start)))
            (unwind-protect
                (condition-case err
                    (let ((report (scalpel-agent-run instr)))
                      (funcall spinner)
                      (let ((inhibit-read-only t))
                        (insert (format "Scalpel: %s\n\n" report)))
                      (dolist (line (nreverse scalpel-console--pending-logs))
                        (scalpel-console--append line))
                      (setq scalpel-console--pending-logs nil))
                  (error
                   (funcall spinner)
                   (let ((inhibit-read-only t))
                     (insert (format "Scalpel error: %s\n\n"
                                     (error-message-string err))))
                   (dolist (line (nreverse scalpel-console--pending-logs))
                     (scalpel-console--append line))
                   (setq scalpel-console--pending-logs nil)))
              (setq scalpel-console--busy nil))))
          (goto-char (point-max))
          (message "Scalpel: instruction sent.")))))

(provide 'scalpel-console)

;;; scalpel-console.el ends here
