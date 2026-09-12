;;; scalpel-token.el --- Token cost accounting buffer for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Per-round token accounting in a dedicated buffer.  Estimates use the
;; same 4-characters-per-token heuristic as `scalpel-llm--count-tokens';
;; they are cost estimates, not API usage reports.  Each round appends
;; one line naming the console, the round's up/down estimate, that
;; console's cumulative totals, the grand total across all consoles,
;; and a breakdown (system prompt, context files, history, instruction)
;; so the dominant cost of a session is visible at a glance.

;;; Code:

(require 'cl-lib)
(require 'scalpel-llm)

(defcustom scalpel-token-buffer-name "*scalpel-tokens*"
  "Buffer name holding the per-round token accounting."
  :type 'string
  :group 'scalpel)

(defvar scalpel-token--console-totals (make-hash-table :test 'equal)
  "Console buffer name -> (UP DOWN) cumulative estimated tokens.")

(defvar scalpel-token--grand-up 0
  "Cumulative estimated upload tokens across all consoles.")

(defvar scalpel-token--grand-down 0
  "Cumulative estimated download tokens across all consoles.")

(defun scalpel-token--console-totals (console)
  "Return (UP DOWN) cumulative estimates for CONSOLE, or (0 0)."
  (or (gethash console scalpel-token--console-totals) (list 0 0)))

(defconst scalpel-token--header
  "Scalpel token estimates (4 chars/token, not API usage).
One line per round: time | console | round up/down | sys/ctx/hist/instr prompt makeup | console up/down | ALL up/down.")

(defun scalpel-token--append-line (console up down breakdown)
  "Append one accounting line for CONSOLE to the token buffer.
UP and DOWN are this round's estimates; BREAKDOWN is a plist with
:system :context :history :instruction token counts.  The console
name is never truncated: it is the account key.  The breakdown
columns are dimmed and the ALL columns bolded, so visual weight
matches the account hierarchy."
  (with-current-buffer (get-buffer-create scalpel-token-buffer-name)
    (let ((inhibit-read-only t)
          (ctot (scalpel-token--console-totals console)))
      (save-excursion
        (goto-char (point-max))
        (insert
         (format
          (concat "%s | %s | round %d/%d | %d/%d/%d/%d"
                  " | console %d/%d | ALL %d/%d\n")
          (format-time-string "%H:%M:%S")
          console up down
          (plist-get breakdown :system)
          (plist-get breakdown :context)
          (plist-get breakdown :history)
          (plist-get breakdown :instruction)
          (car ctot) (cadr ctot)
          scalpel-token--grand-up scalpel-token--grand-down))))))

(defun scalpel-token-record (console up down breakdown)
  "Account one round for CONSOLE and print it.
UP and DOWN are the round's estimated upload/download tokens;
BREAKDOWN is a plist as described in `scalpel-token--append-line'."
  (let ((prev (scalpel-token--console-totals console)))
    (puthash console
             (list (+ up (car prev)) (+ down (cadr prev)))
             scalpel-token--console-totals))
  (setq scalpel-token--grand-up (+ scalpel-token--grand-up up)
        scalpel-token--grand-down (+ scalpel-token--grand-down down))
  (scalpel-token--append-line console up down breakdown))

(defun scalpel-token-open ()
  "Show the token accounting buffer."
  (interactive)
  (let ((buf (get-buffer-create scalpel-token-buffer-name)))
    (with-current-buffer buf
      (when (= (buffer-size) 0)
        (let ((inhibit-read-only t))
          (insert scalpel-token--header "\n\n")))
      (special-mode))
    (switch-to-buffer buf)))

(defun scalpel-token-reset ()
  "Clear all token accounting and the buffer."
  (interactive)
  (setq scalpel-token--grand-up 0
        scalpel-token--grand-down 0)
  (clrhash scalpel-token--console-totals)
  (when (get-buffer scalpel-token-buffer-name)
    (with-current-buffer scalpel-token-buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(provide 'scalpel-token)

;;; scalpel-token.el ends here
