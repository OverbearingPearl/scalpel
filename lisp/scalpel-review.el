;;; scalpel-review.el --- Session-end change review for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The consuming half of session review: one buffer at the end of a
;; session, listing every file the session changed as a grouped list
;; of per-record blocks.  The user can reject single blocks (k, which
;; restores through `scalpel-lineage-restore') or accept everything
;; and quit (q).  Nothing here records; it only reads lineage.

;;; Code:

(require 'cl-lib)

(defvar scalpel-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'scalpel-review-next-block)
    (define-key map (kbd "k") #'scalpel-review-reject-block)
    (define-key map (kbd "q") #'scalpel-review-accept-all)
    map)
  "Keymap for session review buffers.")

(require 'scalpel-lineage)

(defun scalpel-review--block-button-p (pos)
  "Return non-nil when the text property at POS identifies a review block."
  (get-text-property pos 'scalpel-review-record))

(defun scalpel-review-next-block ()
  "Move point to the beginning of the next review block."
  (interactive)
  (let ((next (next-single-property-change
               (point) 'scalpel-review-record nil (point-max))))
    (if (and next (< next (point-max)))
        (goto-char next)
      (message "No further block"))))

(defun scalpel-review--record-at-point ()
  "Return the lineage record under point, or nil."
  (get-text-property (point) 'scalpel-review-record))

(defun scalpel-review-reject-block ()
  "Restore the change under point through `scalpel-lineage-restore'."
  (interactive)
  (let ((record (scalpel-review--record-at-point)))
    (if (not record)
        (message "No block under point")
      (pcase (scalpel-lineage-restore record)
        (`t
         (scalpel-review--refresh)
         (message "Restored one change"))
        (`conflict
         (message "Refused: the file changed since this change landed"))
        (other (message "Restore returned %S" other))))))

(defun scalpel-review--group-by-file (records)
  "Group RECORDS by their :file, preserving first-seen order."
  (let ((groups nil))
    (dolist (record records)
      (let* ((file (plist-get record :file))
             (entry (assoc file groups)))
        (if entry
            (setcdr entry (append (cdr entry) (list record)))
          (setq groups (append groups (list (cons file (list record))))))))
    groups))

(defun scalpel-review--render ()
  "Fill the current buffer with the grouped review listing."
  (let ((inhibit-read-only t)
        (records (scalpel-lineage-changes)))
    (erase-buffer)
    (insert "Session review: changes made this session.\n\n")
    (if (null records)
        (insert "No changes were recorded.\n")
      (pcase-dolist (`(,file . ,recs) (scalpel-review--group-by-file records))
        (insert (format "%s (%d change%s)%s\n"
                        file
                        (length recs)
                        (if (= (length recs) 1) "" "s")
                        (if (cl-some #'scalpel-lineage-conflict-p recs)
                            "  [CONFLICT: file changed after the session]"
                          "")))
        (dolist (record recs)
          (let ((begin (point)))
            (insert (format "  round %d  %s\n"
                            (plist-get record :round)
                            (plist-get record :tool)))
            (put-text-property begin (point)
                               'scalpel-review-record record))))
      (insert "\nTAB next block, k reject block, q accept all and quit\n")))
  (goto-char (point-min)))

(defun scalpel-review--refresh ()
  "Re-render the review buffer after a block was restored."
  (scalpel-review--render))

(defun scalpel-review-accept-all ()
  "Accept every remaining change and close the review buffer."
  (interactive)
  (kill-buffer))

(defun scalpel-review-open ()
  "Open the session review buffer when the session changed anything.
Do nothing at all -- no buffer, no message -- when
`scalpel-lineage-clean-p' reports no recorded change."
  (unless (scalpel-lineage-clean-p)
    (let ((buf (get-buffer-create "*scalpel session review*")))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (unless (eq major-mode 'scalpel-review-mode)
          (progn
            (kill-all-local-variables)
            (setq major-mode 'scalpel-review-mode
                  mode-name "Scalpel-Review")
            (use-local-map scalpel-review-mode-map)))
        (scalpel-review--render)
        (setq buffer-read-only t))
      (pop-to-buffer buf))))

(defun scalpel-review-session-ended ()
  "Session-end hook: open the review when the session changed files."
  (when (require 'scalpel-lineage nil t)
    (scalpel-review-open)))

(provide 'scalpel-review)

;;; scalpel-review.el ends here
