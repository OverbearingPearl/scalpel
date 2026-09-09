;;; scalpel-execute.el --- Boundary-locked replacement for blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Applies an edit only within the verified bounds, never beyond.

;;; Code:

(defun scalpel-execute--brackets-balanced-p (text)
  "Return non-nil when TEXT has balanced parentheses."
  (with-temp-buffer
    (delay-mode-hooks (funcall major-mode))
    (insert text)
    (goto-char (point-min))
    (condition-case nil
        (progn (scan-sexps (point) (point-max)) t)
      (scan-error nil))))

(defun scalpel-execute-replace (beg end new-text)
  "Replace region BEG..END in current buffer with NEW-TEXT.
Signal user-error when NEW-TEXT is structurally unbalanced."
  (unless (scalpel-execute--brackets-balanced-p new-text)
    (user-error "Scalpel: replacement has unbalanced brackets; edit refused"))
  (let ((inhibit-read-only t))
    (delete-region beg end)
    (goto-char beg)
    (insert new-text)))

(provide 'scalpel-execute)

;;; scalpel-execute.el ends here
