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
;; renders the widest index range covered by the registered consoles'
;; last dialogues, with each record tagged by its owning session.

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
The review buffer always shows the diff between two adjacent lineage
indices that have file changes, snapshotting the current record
counter at dialogue end.  Collect records via
`scalpel-review-session--collect-records'; when the result is empty,
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
  "Collect lineage records made by registered consoles.
Records live buffer-locally on the edited file's buffer, so scan
every live buffer's `scalpel-lineage--records' and keep records
whose :session is registered and whose :index falls in the widest
dialogue range.  Records without :index (pre-dating the counter)
are kept when their :session matches.  The review buffer itself
\(named `scalpel-review-session--buffer-name') is skipped: it
holds a buffer-local copy of the records from the last render,
and including it would list every record twice."
  (let ((range (scalpel-review-session--collect-indices))
        (result nil))
    (when range
      (dolist (buffer (buffer-list))
        (when (and (not (string= (buffer-name buffer)
                                 scalpel-review-session--buffer-name))
                   (local-variable-p 'scalpel-lineage--records buffer))
          (dolist (record (buffer-local-value
                           'scalpel-lineage--records buffer))
            (let ((session (plist-get record :session))
                  (index (plist-get record :index)))
              (when (and (member session
                                 scalpel-review-session--consoles)
                         (or (null index)
                             (and (>= index (car range))
                                  (<= index (cdr range)))))
                (push record result)))))))
    (nreverse result)))

(defun scalpel-review-session--collect-indices ()
  "Collect :index values of records tied to registered review sessions.
Scan every live buffer for a buffer-local `scalpel-lineage--records',
gather the :index of each record whose :session is registered in
`scalpel-review-session--consoles', and return an inclusive (lo . hi)
cons covering all such indices.
Return nil when no qualifying index exists."
  (let ((indices nil))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (local-variable-p 'scalpel-lineage--records buffer))
        (dolist (record (buffer-local-value 'scalpel-lineage--records buffer))
          (when (and (listp record)
                     (plist-member record :session)
                     (plist-member record :index)
                     (member (plist-get record :session)
                             scalpel-review-session--consoles))
            (push (plist-get record :index) indices)))))
    (when indices
      (cons (apply #'min indices) (apply #'max indices)))))

(provide 'scalpel-review-session)

;;; scalpel-review-session.el ends here
