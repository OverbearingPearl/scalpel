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

(defun scalpel-review-session-register (console)
  "Add CONSOLE to the review participant list and offer an update.
Return non-nil when the review was refreshed.  A declined update
only defers: CONSOLE stays in the list and is picked up by the
next refresh."
  (unless (member console scalpel-review-session--consoles)
    (setq scalpel-review-session--consoles
          (append scalpel-review-session--consoles (list console))))
  (when (y-or-n-p
         (format "Console %s finished with changes; update the review buffer? "
                 console))
    (scalpel-review-session-refresh)
    t))

(defun scalpel-review-session--index-range ()
  "Return \\(START . END) covering all registered consoles' dialogues.
START is the smallest dialogue :start and END the largest :end, or
the current record counter for still-open dialogues.  Nil when no
registered console has a dialogue."
  (let ((start nil) (end nil))
    (dolist (entry (scalpel-lineage-dialogues))
      (when (member (plist-get entry :session)
                    scalpel-review-session--consoles)
        (let ((s (plist-get entry :start))
              (e (or (plist-get entry :end)
                     (scalpel-lineage-record-counter))))
          (setq start (if (or (null start) (< s start)) s start)
                end (if (or (null end) (> e end)) e end)))))
    (when start (cons start end))))

(defun scalpel-review-session--collect-records ()
  "Collect lineage records made by registered consoles.
Records live buffer-locally on the edited file's buffer, so scan
every live buffer's `scalpel-lineage--records' and keep records
whose :session is registered and whose :index falls in the widest
dialogue range.  Records without :index (pre-dating the counter)
are kept when their :session matches."
  (let ((range (scalpel-review-session--index-range))
        (result nil))
    (when range
      (dolist (buffer (buffer-list))
        (when (local-variable-p 'scalpel-lineage--records buffer)
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

(defun scalpel-review-session-refresh ()
  "Re-render the single review buffer over the widest index range.
Bind the collected records as the lineage records the renderer
sees and clear the context filter, so every registered console's
changes show.  Do nothing when no record matches."
  (let ((records (scalpel-review-session--collect-records)))
    (when records
      (let ((buf (get-buffer-create scalpel-review-session--buffer-name)))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (unless (eq major-mode 'scalpel-review-mode)
            (kill-all-local-variables)
            (setq major-mode 'scalpel-review-mode
                  mode-name "Scalpel-Review")
            (use-local-map scalpel-review-mode-map))
          (let ((scalpel-lineage--records records)
                (scalpel-agent--context-files nil))
            (scalpel-review--render))
          (setq buffer-read-only t)
          (goto-char (point-min)))
        (pop-to-buffer buf)
        (dolist (win (get-buffer-window-list buf nil t))
          (set-window-start win (point-min) t)
          (set-window-point win (point-min)))))))

(defun scalpel-review-session--on-record-appended (_record)
  "Silently re-render the existing review buffer after a change.
Intended for `scalpel-lineage--record-appended-hook'; the record
argument is ignored.  Do nothing unless the review buffer already
exists, so no prompt, no `pop-to-buffer' and no creation ever
happens from here."
  (let ((buffer (get-buffer scalpel-review-session--buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (records (scalpel-review-session--collect-records)))
          (setq scalpel-lineage--records records)
          (scalpel-review--render)
          (setq buffer-read-only t))))))

(add-hook 'scalpel-lineage--record-appended-hook
          #'scalpel-review-session--on-record-appended)

(provide 'scalpel-review-session)

;;; scalpel-review-session.el ends here
