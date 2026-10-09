;;; scalpel-review-session.el --- Multi-console review coordination -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Coordination half of the multi-console session review.  Consoles
;; that finished an operation with recorded changes register here;
;; every registration offers the user a review update.  The review
;; renders the records of each registered console over its last dialogue, with each record tagged by its owning session.

;;; Code:

(defvar scalpel-review-session--consoles nil
  "Console identities that ended an operation with recorded changes.
Global: shared by every console so the single review buffer shows
all of their work.")

(defvar scalpel-review-session--buffer-name "*scalpel session review*"
  "Name of the single global review buffer.")

(require 'scalpel-review)

(defun scalpel-review-session-offer ()
  "Update or create the session review buffer with the latest diff.
For each registered console, only that console's latest dialogue is
considered: records are collected via
`scalpel-review-session--collect-records', which scopes to each
console's latest dialogue (records whose :index is strictly greater
than that dialogue's :start and at most its :end), snapshotting the
current record counter at dialogue end.  When the result is empty,
do nothing and return nil.  Otherwise build or reuse the buffer named
`scalpel-review-session--buffer-name': set up the review major mode
manually, bind `scalpel-lineage--records' to the
collected records and `scalpel-agent--context-files' to nil while
calling `scalpel-review--render', make the buffer read-only and move
point to `point-min'.  When the buffer did not previously exist, pop
to it and return t.  When it did exist, ask \"Review buffer exists;
update it with the latest dialogue diff? \": on a no answer kill the
review buffer and return nil, on a yes pop to it and return t."
  (let ((records (scalpel-review-session--collect-records)))
    (if (not records)
        nil
      (let* ((existed (and (get-buffer scalpel-review-session--buffer-name)
                           (buffer-live-p
                            (get-buffer scalpel-review-session--buffer-name))))
             (buffer (get-buffer-create scalpel-review-session--buffer-name)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (kill-all-local-variables)
            (setq major-mode 'scalpel-review-mode
                  mode-name "Scalpel-Review")
            (use-local-map scalpel-review-mode-map)
            (let ((scalpel-lineage--records records)
                  (scalpel-agent--context-files nil))
              (ignore scalpel-lineage--records scalpel-agent--context-files)
              (scalpel-review--render))
            (setq buffer-read-only t)
            (goto-char (point-min))))
        (if (not existed)
            (progn (pop-to-buffer buffer) t)
          (if (y-or-n-p "Review buffer exists; update it with the latest dialogue diff? ")
              (progn (pop-to-buffer buffer) t)
            (kill-buffer buffer)
            nil))))))

(defun scalpel-review-session-refresh ()
  "Refresh the session review buffer.  A thin alias of\n`scalpel-review-session-offer`."
  (scalpel-review-session-offer))

(defun scalpel-review-session--collect-records ()
  "Collect lineage records of the registered consoles' last closed dialogues.
For each record, look up the most recent dialogue in
`scalpel-lineage--dialogues' whose :session equals the record's
:session.  Only when the record's :session is registered in
`scalpel-review-session--consoles', that dialogue exists and is
closed (:end non-nil), and the record carries an :index, is the
record filtered by start < index <= end: an index outside that
range belongs to an earlier dialogue and is dropped.  In every
other case -- no :session, unregistered session, no :index, no
matching dialogue, or an open dialogue -- the record is kept.
This keeps the collected set aligned with the changes counted by
clean-p instead of silently discarding unattributable records.
Records live buffer-locally on the edited file's buffer, so scan
every live buffer's `scalpel-lineage--records'.  The review buffer
itself (named `scalpel-review-session--buffer-name') is skipped: it
holds a buffer-local copy of the records from the last render, and
including it would list every record twice."
  (let ((result nil))
    (dolist (buffer (buffer-list))
      (when (and (not (string= (buffer-name buffer)
                               scalpel-review-session--buffer-name))
                 (local-variable-p 'scalpel-lineage--records buffer))
        (dolist (record (buffer-local-value
                         'scalpel-lineage--records buffer))
          (let ((session (plist-get record :session))
                (index (plist-get record :index))
                dialogue)
            ;; `scalpel-lineage--dialogues' is appended over time, so
            ;; the same session may appear multiple times; the most
            ;; recent dialogue is the last matching entry.  Scan
            ;; backwards instead of taking the first (earliest) match.
            (setq dialogue
                  (cl-find-if
                   (lambda (d) (eq (plist-get d :session) session))
                   (reverse scalpel-lineage--dialogues)))
            (if (and session
                     (member session
                             scalpel-review-session--consoles)
                     index
                     dialogue
                     (plist-get dialogue :end))
                (when (and (> index (plist-get dialogue :start))
                           (<= index (plist-get dialogue :end)))
                  (push record result))
              (push record result))))))
    (setq result (nreverse result))
    result))

(provide 'scalpel-review-session)

;;; scalpel-review-session.el ends here
