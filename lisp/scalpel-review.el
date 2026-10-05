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
    (define-key map (kbd "<backtab>") #'scalpel-review-previous-block)
    (define-key map (kbd "k") #'scalpel-review-reject-block)
    (define-key map (kbd "K") #'scalpel-review-reject-hunk)
    (define-key map (kbd "q") #'scalpel-review-accept-all)
    map)
  "Keymap for session review buffers.
K rejects a single hunk; k rejects a whole block.")

(defface scalpel-review-rejected
  '((t (:strike-through t)))
  "Face used to mark blocks the user has rejected with `k'."
  :group 'scalpel)

(require 'diff)

(require 'scalpel-lineage)

(defun scalpel-review--block-button-p (pos)
  "Return non-nil when the text property at POS identifies a review block."
  (get-text-property pos 'scalpel-review-record))

(defun scalpel-review-next-block ()
  "Move point to the beginning of the next review block."
  (interactive)
  (let ((pos (point))
        (found nil))
    (while (and pos (/= pos (point-max)) (not found))
      (setq pos (next-single-property-change pos 'scalpel-review-record
                                             nil (point-max)))
      (when (and pos (< pos (point-max))
                 (get-text-property pos 'scalpel-review-record))
        (setq found pos)))
    (if found
        (goto-char found)
      (message "No further block"))))

(defun scalpel-review-previous-block ()
  "Move point backward to the beginning of the previous block.
Signal with a message when there is no earlier block."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'scalpel-review-record)))
    (if (and pos (/= pos (point-min)))
        (goto-char pos)
      (message "No earlier block"))))

(defun scalpel-review--record-at-point ()
  "Return the lineage record under point, or nil."
  (get-text-property (point) 'scalpel-review-record))

(defun scalpel-review-reject-block ()
  "Restore the change under point through `scalpel-lineage-restore'."
  (interactive)
  (let ((record (scalpel-review--record-at-point)))
    (if (not record)
        (message "No block under point")
      (let ((file (plist-get record :file))
            (pos (point))
            (blocked nil))
        ;; A later edit to the same file can rewrite or remove the
        ;; region this record's new text occupies.  Undo every newer
        ;; record for the same file (newest first, i.e. buffer order
        ;; below point) so the earlier record's new text is present
        ;; again and restores cleanly.
        (save-excursion
          (goto-char (next-single-property-change
                      pos 'scalpel-review-record nil (point-max)))
          (catch 'blocked
            (while (< (point) (point-max))
              (let ((newer (get-text-property (point) 'scalpel-review-record)))
                (when (and newer (equal (plist-get newer :file) file))
                  (pcase (scalpel-lineage-restore newer)
                    (`conflict
                     (setq blocked t)
                     (throw 'blocked nil)))))
              (goto-char (next-single-property-change
                          (point) 'scalpel-review-record nil (point-max))))))
        (if blocked
            (message "Refused: the file changed since this change landed")
          (pcase (scalpel-lineage-restore record)
            (`t
             (save-excursion
               (if (get-text-property (point) 'scalpel-review-record)
                   (let* ((end (next-single-property-change
                                (point) 'scalpel-review-record nil (point-max)))
                          (beg (previous-single-property-change
                                end 'scalpel-review-record nil (point-min))))
                     (let ((inhibit-read-only t))
                       (put-text-property beg end 'face 'scalpel-review-rejected)))
                 (let ((inhibit-read-only t))
                   (put-text-property (point) (point) 'face 'scalpel-review-rejected))))
             (message "Restored one change"))
            (`conflict
             (message "Refused: the file changed since this change landed"))
            (other (message "Restore returned %S" other))))))))

(defun scalpel-review--hunk-region-at-point ()
  "Return (BEG . END) covering the diff hunk containing point.
The hunk is one @@-delimited section of the record block under
point, including its @@ header line.  Nil when point is not
inside a record's diff hunks."
  (let* ((record (scalpel-review--record-at-point))
         (block-beg nil)
         (block-end nil)
         (hunk-beg nil)
         (hunk-end nil))
    (when record
      (save-excursion
        (setq block-end (next-single-property-change
                         (point) 'scalpel-review-record
                         nil (point-max)))
        (setq block-beg (previous-single-property-change
                         block-end 'scalpel-review-record
                         nil (point-min)))
        (beginning-of-line)
        ;; Walk back to the nearest @@ line, staying in the block.
        (catch 'found
          (while t
            (when (looking-at "@@")
              (setq hunk-beg (point))
              (throw 'found nil))
            (if (or (bobp) (<= (point) block-beg))
                (throw 'found nil)
              (forward-line -1))))
        (when hunk-beg
          (goto-char hunk-beg)
          (forward-line 1)
          (while (and (< (point) block-end)
                      (not (looking-at "@@")))
            (forward-line 1))
          (setq hunk-end (point))))
      (when hunk-beg (cons hunk-beg hunk-end)))))

(defun scalpel-review--hunk-operations (beg end)
  "Return the hunk's line operations between BEG and END.
Each element is (KIND TEXT), where KIND is `context', `removed'
or `added' and TEXT is the line's content including its newline.
The @@ header line is skipped."
  (save-excursion
    (let (ops)
      (goto-char beg)
      (forward-line 1)
      (while (< (point) end)
        (let* ((raw (buffer-substring (point) (line-end-position)))
               (nl (if (< (line-end-position) end) "\n" ""))
               (kind (cond
                      ((string-prefix-p " " raw) 'context)
                      ((string-prefix-p "-" raw) 'removed)
                      ((string-prefix-p "+" raw) 'added))))
          (when kind
            (push (list kind (concat (substring raw 1) nl)) ops)))
        (forward-line 1))
      (nreverse ops))))

(defun scalpel-review-reject-hunk ()
  "Reject the diff hunk under point in the review buffer.
Reverse-apply one @@ hunk of the record's diff instead of
restoring the whole block: the diff shows changes already applied,
so the file is in the after state.  The hunk's after-side lines
\(context and added) must occur exactly once in the file, then
they are replaced by the before-side lines (context and removed).
The record's :new-text is rewritten accordingly so later
block-level restores stay consistent.  Unlike
`scalpel-review-reject-block' no later records for the same file
are undone first; a stale file is reported and nothing is
changed."
  (interactive)
  (let ((region (scalpel-review--hunk-region-at-point)))
    (if (not region)
        (message "No hunk under point")
      (let* ((record (scalpel-review--record-at-point))
             (file (plist-get record :file))
             (ops (scalpel-review--hunk-operations
                   (car region) (cdr region)))
             (after (mapconcat
                     (lambda (op)
                       (if (memq (car op) '(context added))
                           (nth 1 op) ""))
                     ops ""))
             (before (mapconcat
                      (lambda (op)
                        (if (memq (car op) '(context removed))
                            (nth 1 op) ""))
                      ops "")))
        (if (string-empty-p after)
            (message "Nothing to locate in this hunk")
          (let ((current
                 (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))
            (let ((first (string-search after current)))
              (cond
               ((not first)
                (message "Refused: hunk context not found in file"))
               ((string-search after current (1+ first))
                (message "Refused: hunk context is ambiguous"))
               (t
                (with-temp-buffer
                  (insert (string-replace after before current))
                  (write-region (point-min) (point-max)
                                file nil 'silent))
                (plist-put record :new-text
                           (string-replace after before
                                           (plist-get record :new-text)))
                (message "Rejected one hunk"))))))))))

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

(defun scalpel-review--insert-record (record)
  "Insert one RECORD into the review buffer.
Insert a visible separator line of box-drawing dashes before the
block, styled with `magit-section-heading', then render a header
line (bold) followed by a unified diff of the record's old-text
versus new-text, produced synchronously by the diff program;
rename records (detected by :tool being `file-rename') show a
rename line instead.  The record's :session value, when present,
is shown in a bracketed suffix on the header line; records
without a :session render as before.  The diff program's own
header lines (\"--- before/<name>\" and \"+++ after/<name>\") are
removed, so the diff output shows only the @@ hunk line and the
+/- content lines; the file header line (\"File: <path>\") is the
only label for the block.
Header lines get a bold face and diff output lines are colored
with magit's diff faces (`magit-diff-added',
`magit-diff-removed', `magit-diff-hunk-heading') when those
faces are defined, falling back to the built-in
`diff-added'/`diff-removed'/`diff-hunk-header' faces otherwise.
The whole block (including the separator line) is tagged with the
`scalpel-review-record' text property."
  (let* ((inhibit-read-only t)
         (beg (point))
         (old-text (plist-get record :old-text))
         (new-text (plist-get record :new-text))
         (file (plist-get record :file))
         (tool (plist-get record :tool))
         (session (plist-get record :session))
         (session-suffix (if session
                             (format "  [session: %s]" session)
                           ""))
         (program (if (boundp 'diff-command) diff-command "diff"))
         ;; Resolve diff faces at render time: prefer magit's diff
         ;; faces when magit has been loaded (its faces are defined),
         ;; otherwise fall back to the built-in diff-mode faces.
         (added-face (if (facep 'magit-diff-added)
                         'magit-diff-added
                       'diff-added))
         (removed-face (if (facep 'magit-diff-removed)
                           'magit-diff-removed
                         'diff-removed))
         (hunk-face (if (facep 'magit-diff-hunk-heading)
                        'magit-diff-hunk-heading
                      'diff-hunk-header))
         old-file new-file)
    (unwind-protect
        (progn
          ;; Visible separator line before the block, magit style.
          (insert (concat (make-string (max (- (window-width) 1) 8)
                                       (aref "─" 0))
                          "\n"))
          (put-text-property beg (point) 'face 'magit-section-heading)
          (setq beg (point))
          (if (eq tool 'file-rename)
              (progn
                (insert (format "Rename: %s -> %s%s\n"
                                old-text file session-suffix))
                ;; Highlight the rename header line in bold.
                (put-text-property beg (point) 'face
                                   '(bold diff-header)))
            (progn
              (insert (format "File: %s%s\n" file session-suffix))
              (when (scalpel-lineage-conflict-p record)
                (save-excursion
                  (forward-line -1)
                  (end-of-line)
                  (insert "  [CONFLICT: file changed after the session]")))
              ;; Highlight the file header line in bold.
              (put-text-property beg (point) 'face
                                 '(bold diff-header))
              ;; Write old and new text to temporary files so the
              ;; external diff program can compare them synchronously.
              (setq old-file (make-temp-file "scalpel-review-old-"))
              (setq new-file (make-temp-file "scalpel-review-new-"))
              (with-temp-file old-file
                (insert (or old-text "")))
              (with-temp-file new-file
                (insert (or new-text "")))
              ;; Run the diff program synchronously, capturing its
              ;; stdout into a temp buffer, then insert the full text
              ;; at point under the File header.  Pass the labels
              ;; through two -L options before the two temp file
              ;; operands, so the diff headers name the real file
              ;; (before/after) rather than the temp file paths.
              (let ((diff-start (point))
                    (diff-output (with-temp-buffer
                                   (call-process program nil t nil
                                                 "-u"
                                                 "-L" (concat "before/" (file-name-nondirectory file))
                                                 "-L" (concat "after/" (file-name-nondirectory file))
                                                 old-file new-file)
                                   (buffer-substring (point-min) (point-max)))))
                (insert diff-output)
                ;; Remove the diff program's own header lines
                ;; ("--- before/<name>" and "+++ after/<name>") so
                ;; only the @@ hunk line and +/- content lines remain.
                (save-excursion
                  (goto-char diff-start)
                  (while (not (eobp))
                    (if (looking-at "\\(?:---\\|\\+\\+\\+\\) ")
                        (delete-region (point) (progn (forward-line 1) (point)))
                      (forward-line 1))))
                ;; Apply per-line faces to the diff output:
                ;; "+" lines as added, "-" lines as removed, and
                ;; "@@" lines as hunk headers, using the resolved
                ;; magit or built-in faces.
                (save-excursion
                  (goto-char diff-start)
                  (while (not (eobp))
                    (let ((line-start (point))
                          (line-end (line-end-position)))
                      (cond
                       ((looking-at "\\+\\(?:\\+\\+\\)?")
                        (put-text-property line-start line-end
                                           'face added-face))
                       ((looking-at "-\\(?:--\\)?")
                        (put-text-property line-start line-end
                                           'face removed-face))
                       ((looking-at "@@")
                        (put-text-property line-start line-end
                                           'face hunk-face)))
                      (forward-line 1))))
                (when (string-empty-p
                       (buffer-substring diff-start (point)))
                  (delete-region diff-start (point))
                  (insert "No differences\n")))))
          ;; Tag the entire block (including the separator line)
          ;; with the record property last so face properties
          ;; applied above are preserved.
          (let ((record-end (point)))
            (put-text-property beg record-end 'scalpel-review-record record)))
      ;; Clean up the temporary files regardless of success or error.
      (when old-file
        (condition-case nil
            (delete-file old-file)
          (error nil)))
      (when new-file
        (condition-case nil
            (delete-file new-file)
          (error nil))))))

(defun scalpel-review--render ()
  "Fill the current buffer with a unified-diff review listing.

If the buffer-local variable `scalpel-agent--context-files' is bound
and non-nil, only lineage records whose :file is a member of that list
are shown; otherwise all recorded changes are rendered."
  (require 'diff)
  (let ((inhibit-read-only t)
        (records (scalpel-lineage-changes)))
    ;; When session context files are known (the session-end hook runs in
    ;; the console buffer), restrict the review to files in that context
    ;; so test-run temp files never appear.  Compare through
    ;; `file-truename' on both sides so resolved paths such as
    ;; /private/var/folders/... match user-spelled /var/folders/... ones.
    (when (and (boundp 'scalpel-agent--context-files)
               scalpel-agent--context-files)
      (let ((context-truenames
             (mapcar #'file-truename scalpel-agent--context-files)))
        (setq records
              (cl-remove-if-not
               (lambda (record)
                 (member (file-truename (plist-get record :file))
                         context-truenames))
               records))))
    (setq header-line-format " TAB next | Shift+TAB prev | k reject | K reject hunk | q accept all & quit ")
    (erase-buffer)
    (insert "Session review: changes made this session.\n\n")
    (if (null records)
        (insert "No changes were recorded.\n")
      (dolist (record records)
        (scalpel-review--insert-record record)))
    (goto-char (point-min))))

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
`scalpel-lineage-clean-p' reports no recorded change.

Capture `scalpel-agent--context-files' from the calling (console)
buffer before switching, so the context filter reflects this
session even though the variable is not buffer-local in the
review buffer.

Rendered records live buffer-locally on the edited file buffers,
so a console buffer holds none of them and a purely local lookup
would render nothing.  Softly require `scalpel-review-session'
before collecting, then collect records by scanning every live
buffer's buffer-local `scalpel-lineage--records' with no session
filter, so consoles that never registered are not dropped; fall
back to the calling buffer's local records (or all records when
unbound) only when the collector is unavailable.

After popping to the review buffer, move its point and every
displaying window's start back to the beginning on all frames, so
a reused window never leaves the cursor at the end."
  (interactive)
  (unless (scalpel-lineage-clean-p)
    (let ((buf (get-buffer-create "*scalpel session review*"))
          (context-files (if (boundp 'scalpel-agent--context-files)
                             scalpel-agent--context-files
                           nil))
          (records
           (if (require 'scalpel-review-session nil t)
               (let (collected)
                 (dolist (b (buffer-list) collected)
                   (with-current-buffer b
                     (when (boundp 'scalpel-lineage--records)
                       (setq collected
                             (append collected
                                     (copy-sequence
                                      scalpel-lineage--records)))))))
             (if (boundp 'scalpel-lineage--records)
                 scalpel-lineage--records
               (scalpel-lineage-changes)))))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (unless (eq major-mode 'scalpel-review-mode)
          (progn
            (kill-all-local-variables)
            (setq major-mode 'scalpel-review-mode
                  mode-name "Scalpel-Review")
            (use-local-map scalpel-review-mode-map)))
        (let ((scalpel-agent--context-files context-files)
              (scalpel-lineage--records records))
          (scalpel-review--render))
        (setq buffer-read-only t)
        (goto-char (point-min)))
      (pop-to-buffer buf)
      (dolist (win (get-buffer-window-list buf nil t))
        (set-window-start win (point-min) t)
        (set-window-point win (point-min))))))

(defun scalpel-review-session-ended ()
  "Session-end hook: open the review when the session changed files."
  (when (require 'scalpel-lineage nil t)
    (scalpel-review-open)))

(provide 'scalpel-review)

;;; scalpel-review.el ends here
