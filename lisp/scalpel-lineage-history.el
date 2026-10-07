;;; scalpel-lineage-history.el --- Lineage roadmap viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Read-only roadmap view over the lineage dialogues.  Each recorded
;; dialogue of every console session appears as one node on a vertical
;; unicode line, annotated with the originating console, the user's
;; question (truncated), and the change records it covers.  Point
;; movement walks node to node; RET rolls the worktree back to just
;; before the selected dialogue by restoring its records newest first.

;;; Code:

(require 'cl-lib)

(defvar scalpel-lineage-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'scalpel-lineage-history-next)
    (define-key map "p" #'scalpel-lineage-history-previous)
    (define-key map "f" #'scalpel-lineage-history-forward)
    (define-key map "b" #'scalpel-lineage-history-backward)
    (define-key map "j" #'scalpel-lineage-history-next)
    (define-key map "k" #'scalpel-lineage-history-previous)
    (define-key map [down] #'scalpel-lineage-history-next)
    (define-key map [up] #'scalpel-lineage-history-previous)
    (define-key map "l" #'scalpel-lineage-history-forward)
    (define-key map "h" #'scalpel-lineage-history-backward)
    (define-key map "g" #'scalpel-lineage-history--refresh)
    (define-key map (kbd "RET") #'scalpel-lineage-history-rollback)
    (define-key map "q" #'quit-window)
    map))

(defvar scalpel-lineage-history--buffer-name "*scalpel-roadmap*"
  "Name of the roadmap buffer used by --refresh and --open.")

(defun scalpel-lineage-history--all-records ()
  "Collect lineage records from every live buffer.
Lineage records are buffer-local on the edited file's buffer, so the
roadmap scans all buffers and concatenates their records."
  (let ((records nil))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (boundp 'scalpel-lineage--records)
          (setq records (append records scalpel-lineage--records)))))
    (cl-sort records #'< :key (lambda (r) (or (plist-get r :index) 0)))))

(defun scalpel-lineage-history--records-in (records start end)
  "Return the RECORDS whose :index falls in [START, END).
An open dialogue (END nil) takes every record at or past START."
  (cl-remove-if-not
   (lambda (r)
     (let ((idx (plist-get r :index)))
       (and (numberp idx) (>= idx start)
            (or (null end) (< idx end)))))
   records))

(defun scalpel-lineage-history--truncate-question (question)
  "Trim QUESTION to one line of at most 60 visible characters."
  (when (stringp question)
    (let ((text (car (split-string question "\n" t))))
      (setq text (string-trim text))
      (when (> (length text) 60)
        (setq text (concat (substring text 0 60) "...")))
      text)))

(defun scalpel-lineage-history--nodes ()
  "Build the node list from the lineage dialogues.
Each node is a plist: :dialogue :session :question :records :open.
Dialogues with no records and no question are still shown so the
roadmap reflects every dialogue the consoles opened."
  (require 'scalpel-lineage)
  (let ((records (scalpel-lineage-history--all-records)))
    (mapcar
     (lambda (dialogue)
       (let ((start (plist-get dialogue :start))
             (end (plist-get dialogue :end))
             (session (plist-get dialogue :session)))
         (list :dialogue dialogue
               :session session
               :question (plist-get dialogue :question)
               :open (null end)
               :records (scalpel-lineage-history--records-in
                         records start end))))
     (scalpel-lineage-dialogues))))

(defun scalpel-lineage-history--render ()
  "Fill the current buffer with the unicode roadmap."
  (let ((inhibit-read-only t)
        (nodes (scalpel-lineage-history--nodes)))
    (erase-buffer)
    (setq header-line-format
          " npjk node | fbhl session | RET rollback | g refresh | q quit ")
    (if (null nodes)
        (insert "No lineage dialogues recorded.\n")
      (cl-loop for node in nodes
               for i from 1 do
               (scalpel-lineage-history--insert-node node i)))
    (goto-char (point-min))
    (scalpel-lineage-history--snap)))

(defun scalpel-lineage-history--refresh ()
  "Refresh the lineage history roadmap buffer in place.
Re-render the roadmap from the current lineage dialogues so that
newly appeared dialogues or records are reflected in the display."
  (interactive)
  (when (get-buffer scalpel-lineage-history--buffer-name)
    (with-current-buffer scalpel-lineage-history--buffer-name
      (scalpel-lineage-history--render))))

(defun scalpel-lineage-history--insert-node (node i)
  "Insert one roadmap NODE numbered I at point."
  (let* ((session (or (plist-get node :session) "anonymous"))
         (open (plist-get node :open))
         (records (plist-get node :records))
         (question (scalpel-lineage-history--truncate-question
                    (plist-get node :question)))
         beg)
    ;; Trunk separator only between nodes, inserted before the node text
    ;; and outside the text-property span.
    (unless (= i 0)
      (insert "│\n"))
    (setq beg (point))
    ;; Node bullet on the vertical trunk line.
    (insert (if open "○ " "● "))
    (insert (format "dialogue %d  session: %s  %d change(s)%s\n"
                    i session (length records)
                    (if open "  (still open)" "")))
    (when question
      (insert "│   Q: " question "\n"))
    (dolist (record records)
      (insert (format "│   - %s %s%s\n"
                      (plist-get record :tool)
                      (plist-get record :file)
                      (if (plist-get record :restored)
                          "  [restored]" ""))))
    ;; Property covers only this node's own text, not the separator.
    (put-text-property beg (point)
                       'scalpel-lineage-history-node node)))

(defun scalpel-lineage-history--snap ()
  "Leave point on the first node line if it is off the roadmap."
  (goto-char (point-min))
  (if (get-text-property (point) 'scalpel-lineage-history-node)
      nil
    (let ((pos (next-single-property-change
                (point-min) 'scalpel-lineage-history-node
                nil (point-max))))
      (goto-char pos))))

(defun scalpel-lineage-history--current-node ()
  "Return the node at point, or the nearest one at or before it.
If no node exists at or before point, fall back to the nearest
node after point, so callers starting at the beginning of the
buffer still resolve to the first node."
  (or (get-text-property (point) 'scalpel-lineage-history-node)
      (let ((pos (point)))
        (or (progn
              (goto-char (previous-single-property-change
                          (1+ (point)) 'scalpel-lineage-history-node
                          nil (point-min)))
              (get-text-property (point) 'scalpel-lineage-history-node))
            ;; Fallback: nearest node strictly after the original position.
            (progn
              (goto-char (next-single-property-change
                          pos 'scalpel-lineage-history-node
                          nil (point-max)))
              (get-text-property (point) 'scalpel-lineage-history-node))))))

(defun scalpel-lineage-history--node-positions ()
  "Start by returning buffer positions where each node's property begins."
  (let ((positions nil) (pos (point-min)))
    (while (< pos (point-max))
      (when (get-text-property pos 'scalpel-lineage-history-node)
        (push pos positions)
        (setq pos (next-single-property-change
                   pos 'scalpel-lineage-history-node nil (point-max))))
      (setq pos (next-single-property-change
                 pos 'scalpel-lineage-history-node nil (point-max))))
    (nreverse positions)))

(defun scalpel-lineage-history--goto-node (delta)
  "Move point by DELTA nodes along the roadmap."
  (interactive)
  (let* ((positions (scalpel-lineage-history--node-positions))
         (current (point))
         (idx (or (cl-position current positions :test #'<=) 0)))
    (when (get-text-property current 'scalpel-lineage-history-node)
      (setq idx (cl-position current positions)))
    (setq idx (max 0 (min (1- (length positions)) (+ (or idx 0) delta))))
    (goto-char (nth idx positions))))

(defun scalpel-lineage-history-next ()
  "Move to the next dialogue node on the roadmap."
  (interactive)
  (let ((positions (scalpel-lineage-history--node-positions))
        (here (point)))
    (let ((target (cl-find-if
                   (lambda (pos) (> pos here))
                   positions)))
      (if target
          (goto-char target)
        (ding)))))

(defun scalpel-lineage-history-previous ()
  "Move to the previous dialogue node on the roadmap."
  (interactive)
  (let* ((positions (scalpel-lineage-history--node-positions))
         (here (point))
         (on-node (get-text-property here 'scalpel-lineage-history-node))
         (target nil))
    (dolist (pos positions)
      (when (or (and on-node (< pos here))
                (and (not on-node) (<= pos here)))
        (setq target pos)))
    (if target
        (goto-char target)
      (ding))))

(defun scalpel-lineage-history-forward ()
  "Jump to the first node of the next distinct session."
  (interactive)
  (let* ((node (scalpel-lineage-history--current-node))
         (positions (scalpel-lineage-history--node-positions))
         (pos (cl-position (point) positions)))
    (when pos
      (catch 'done
        (dolist (target (nthcdr (1+ pos) positions))
          (let ((n (get-text-property target 'scalpel-lineage-history-node)))
            (unless (equal (plist-get n :session)
                           (plist-get node :session))
              (goto-char target)
              (throw 'done nil))))))))

(defun scalpel-lineage-history-backward ()
  "Jump to the first node of the previous distinct session."
  (interactive)
  (let* ((node (scalpel-lineage-history--current-node))
         (positions (scalpel-lineage-history--node-positions))
         (pos (cl-position (point) positions)))
    (when pos
      (catch 'done
        (dolist (target (reverse (cl-subseq positions 0 pos)))
          (let ((n (get-text-property target 'scalpel-lineage-history-node)))
            (unless (equal (plist-get n :session)
                           (plist-get node :session))
              (goto-char target)
              (throw 'done nil))))))))

(defun scalpel-lineage-history-rollback ()
  "Restore the roadmap state as of the dialogue at point.
Collect the change records of this node and of every later node
in the roadmap in chronological order, then restore them newest
first via `scalpel-lineage-restore'.  Conflicts are counted and
reported, not forced.  The roadmap is re-rendered afterwards to
show the restored marks."
  (interactive)
  (let* ((node (scalpel-lineage-history--current-node))
         (dialogue (plist-get node :dialogue))
         (all-nodes (scalpel-lineage-history--nodes))
         (nodes (or (cl-member dialogue all-nodes
                               :test (lambda (a b)
                                       (equal a (plist-get b :dialogue))))
                    (error "Scalpel: current node not found in roadmap nodes")))
         (records (nreverse
                   (apply #'append
                          (mapcar (lambda (n)
                                    (plist-get n :records))
                                  nodes))))
         (restored 0) (conflicted 0))
    (if (null records)
        (message "Scalpel: this dialogue has no change records to roll back.")
      (dolist (record records)
        (if (eq (scalpel-lineage-restore record) t)
            (setq restored (1+ restored))
          (setq conflicted (1+ conflicted))))
      (message "Scalpel: rolled back %d change(s); %d refused as conflict."
               restored conflicted)
      (scalpel-lineage-history--render))))

(defun scalpel-lineage-history-open ()
  "Open the lineage history roadmap buffer.
The roadmap lists every lineage dialogue across all console
sessions in global record order, so the user can inspect and roll
back to any point of the recorded history."
  (interactive)
  (let ((buf (get-buffer-create scalpel-lineage-history--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'scalpel-lineage-history-mode)
        (kill-all-local-variables)
        (setq major-mode 'scalpel-lineage-history-mode
              mode-name "Scalpel-Lineage-History")
        (setq buffer-read-only t))
      (use-local-map scalpel-lineage-history-mode-map)
      (scalpel-lineage-history--render))
    (pop-to-buffer buf)))

(provide 'scalpel-lineage-history)

;;; scalpel-lineage-history.el ends here
