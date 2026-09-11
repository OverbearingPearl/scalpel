;;; scalpel-execute.el --- Boundary-locked replacement for blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Applies an edit only within the verified bounds, never beyond.
;;
;; This module is what makes unconfirmed application defensible: the replacement
;; lands on a range the locator verified and nowhere else, so no per-hunk
;; approval is asked for.  What is not asked for must be visible instead, so the
;; caller reports every replacement it makes as it lands, and a session's
;; changes stay readable for review and for rollback.  A structurally unbalanced
;; replacement is refused outright rather than applied for the user to notice
;; afterwards.
;;
;; An applied change is written to its file as it lands, because the change is
;; not only for the user to read afterwards: the shell commands the agent runs
;; next read the file from disk, and a change still sitting in a buffer would be
;; invisible to them.  A deletion is more than the removal of a range: the line
;; the block occupied goes with it, and the blank lines the removal brings
;; together are collapsed, so deleting a definition does not leave the blank
;; line before it and the blank line after it stacked where the code was.

;;; Code:

(require 'subr-x)

(defun scalpel-execute--brackets-balanced-p (text)
  "Return non-nil when TEXT has balanced parentheses."
  (with-temp-buffer
    (delay-mode-hooks (funcall major-mode))
    (insert text)
    (goto-char (point-min))
    (condition-case nil
        (progn (scan-sexps (point) (point-max)) t)
      (scan-error nil))))

(defun scalpel-execute--save ()
  "Write the current buffer to its file when it visits one.
The single point where an applied change reaches disk: the
replacement and the deletion primitives both end here, so no write
path can forget it.  A buffer that visits no file -- a temp buffer
in a test, say -- has nothing to write and is left alone."
  (when buffer-file-name
    (save-buffer)))

(defun scalpel-execute-replace (beg end new-text)
  "Replace region BEG..END in current buffer with NEW-TEXT.
The buffer is saved when it visits a file, so the change is on disk
before this returns.  Signal user-error when NEW-TEXT is
structurally unbalanced."
  (unless (scalpel-execute--brackets-balanced-p new-text)
    (user-error "Scalpel: replacement has unbalanced brackets; edit refused"))
  (let ((inhibit-read-only t))
    (delete-region beg end)
    (goto-char beg)
    (insert new-text))
  (scalpel-execute--save))

(defun scalpel-execute--block-deletion-end (beg end)
  "Return the end position for deleting the block BEG..END.
END is extended past the line's terminating newline when the block
starts at the beginning of its line and nothing but whitespace
follows END on that line -- when the block owns the whole line.
Without the extension the line's own newline survives the deletion
as a blank line at the join.  END comes back unchanged otherwise, so
a block sharing its line with other text leaves that text in place."
  (if (and (= beg (save-excursion (goto-char beg) (line-beginning-position)))
           (save-excursion
             (goto-char end)
             (let ((line-end (line-end-position)))
               (and (< line-end (point-max))
                    (eq (char-after line-end) ?\n)
                    (string-blank-p
                     (buffer-substring-no-properties end line-end))))))
      (save-excursion (goto-char end) (1+ (line-end-position)))
    end))

(defun scalpel-execute--collapse-blank-lines (pos)
  "Collapse the run of blank lines touching POS to at most one.
POS is where two regions of text were just joined by a deletion.
A blank line is a line holding only whitespace.  A run reaching the
start or the end of the buffer is removed entirely: at a buffer
boundary there is no second definition for a blank line to separate
the survivor from.  Return nil."
  (let ((after-end (save-excursion
                     (goto-char pos)
                     (while (and (< (point) (point-max))
                                 (looking-at "[ \t]*\n"))
                       (forward-line 1))
                     (point)))
        (before-start (save-excursion
                        (goto-char pos)
                        (while (and (> (point) (point-min))
                                    (save-excursion
                                      (forward-line -1)
                                      (looking-at "[ \t]*$")))
                          (forward-line -1))
                        (point))))
    (let* ((boundary (or (= before-start (point-min))
                         (= after-end (point-max))))
           (blanks (how-many "\n" before-start after-end))
           (keep (if boundary 0 (min blanks 1))))
      (unless (= keep blanks)
        (let ((inhibit-read-only t))
          (delete-region before-start after-end)
          (goto-char before-start)
          (when (= keep 1)
            (insert "\n")))))))

(defun scalpel-execute-delete (beg end)
  "Delete the block BEG..END and reconcile the blank lines around it.
The line the block owns is deleted whole, and the blank lines the
deletion brings together are collapsed to a single one when there
was more than one, so a block with a blank line on each side leaves
one blank line behind and not two.  A block with no blank line
around it leaves none: deleting it puts its neighbours on adjacent
lines.  The buffer is saved when it visits a file."
  (let ((inhibit-read-only t))
    (delete-region beg (scalpel-execute--block-deletion-end beg end)))
  (goto-char beg)
  (scalpel-execute--collapse-blank-lines (point))
  (scalpel-execute--save))

(provide 'scalpel-execute)

;;; scalpel-execute.el ends here
