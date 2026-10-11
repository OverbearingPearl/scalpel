;;; scalpel-review-session.el --- Multi-console review coordination -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Coordination half of the session review.  The lineage dialogue
;; chain is global: every console appends to the same list, and each
;; review offer or update renders the diff between the two most
;; recent adjacent nodes, mixing records from all sessions.

;;; Code:

(defvar scalpel-review-session--buffer-name "*scalpel session review*"
  "Name of the single global review buffer.")

(require 'scalpel-review)

(defun scalpel-review-session-offer ()
  "Update or create the session review buffer with the latest diff.
Collect records spanning the global dialogue chain via
`scalpel-review-session--collect-records', which renders the diff
between the two most recent adjacent nodes over their [start, end)
intervals.  When the result is empty, do nothing and return nil.
When a buffer named `scalpel-review-session--buffer-name' already
exists, first ask \"Review buffer exists; update it with the latest
dialogue diff? \": on a no answer leave the old buffer completely
untouched (no re-render, no kill) and return nil; on a yes answer
proceed through the shared render path below.  The render path is
shared by both the fresh and the update case: build or reuse the
buffer, set up the review major mode manually, bind
`scalpel-lineage--records' to the collected records and
`scalpel-agent--context-files' to nil while calling
`scalpel-review--render', make the buffer read-only, move point to
`point-min', then pop to the buffer and return t."
  (let ((records (scalpel-review-session--collect-records)))
    (if (not records)
        nil
      (let ((existed (and (get-buffer scalpel-review-session--buffer-name)
                          (buffer-live-p
                           (get-buffer scalpel-review-session--buffer-name)))))
        (if (and existed
                 (not (y-or-n-p
                       "Review buffer exists; update it with the latest dialogue diff? ")))
            nil
          (let ((buffer (get-buffer-create scalpel-review-session--buffer-name)))
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
            (pop-to-buffer buffer)
            t))))))

(defun scalpel-review-session-refresh ()
  "Refresh the session review buffer.  A thin alias of\n`scalpel-review-session-offer`."
  (scalpel-review-session-offer))

(defun scalpel-review-session--collect-records ()
  "Collect lineage records belonging to the last global dialogue diff.
Read the global `scalpel-lineage--dialogues' and take its last node;
keep only records whose :index falls in that node's half-open
[start, end) range -- the diff between the two most recent adjacent
nodes, mixed across all sessions.  With a single node the same rule
applies (it diffs against an empty start).  :end is an exclusive
upper bound: the dialogue records the global counter value at
session end, and the next record receives exactly that value as its
:index, so such a record belongs to the next dialogue and is dropped
here.  When the last node is still open (:end nil), keep all records
with :index >= its :start.

Records live buffer-locally on the edited file's buffer, so scan
every live buffer's `scalpel-lineage--records'.  The review buffer
itself (named `scalpel-review-session--buffer-name') is skipped: it
holds a buffer-local copy of the records from the last render, and
including it would list every record twice.  The result is sorted
by :index."
  (let ((result nil)
        (dialogue (car (last scalpel-lineage--dialogues))))
    (when dialogue
      (let ((start (plist-get dialogue :start))
            (end (plist-get dialogue :end)))
        (dolist (buffer (buffer-list))
          (when (and (not (string= (buffer-name buffer)
                                   scalpel-review-session--buffer-name))
                     (local-variable-p 'scalpel-lineage--records buffer))
            (dolist (record (buffer-local-value
                             'scalpel-lineage--records buffer))
              (let ((index (plist-get record :index)))
                (when (and index (>= index start)
                           (or (null end) (< index end)))
                  (push record result))))))))
    (setq result (sort (nreverse result)
                       (lambda (a b)
                         (< (plist-get a :index)
                            (plist-get b :index)))))
    result))

(provide 'scalpel-review-session)

;;; scalpel-review-session.el ends here
