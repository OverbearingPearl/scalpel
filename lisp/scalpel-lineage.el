;;; scalpel-lineage.el --- Session change lineage for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The recording half of session review: one record per file-writing
;; action, holding the old and new text needed to restore that single
;; change later.  Consumers (the session review buffer) read the
;; records through `scalpel-lineage-changes' and undo one through
;; `scalpel-lineage-restore'; nothing here renders or decides.

;;; Code:

(defvar scalpel-lineage--records nil
  "Change records of the current session, oldest last.
Each entry is a plist: :tool :file :old-text :new-text :round
:old-hash :new-hash.  For file-create, :old-text is nil; for
file-delete, :new-text is nil; for file-rename, :file names the new
path and :old-text the original one.  Text tools record the replaced
segment in :old-text and what replaced it in :new-text, so a restore
can locate its target without a whole-file snapshot per record.

This is deliberately global, not buffer-local: the recorder runs
from agent/execute callbacks whose current buffer is the edited
file's buffer, so buffer-local state would silently drop records.
A multi-console future should add a session tag to scope records
per session.")

(defvar scalpel-lineage--baseline nil
  "Dangling git commit captured at session start, or nil.
Produced by `git stash create', which touches neither the worktree
nor any ref; the session-level diff anchors on it.
Global, not buffer-local: the anchor is captured once per session
and must survive being set from a non-session buffer.")

(defvar scalpel-lineage-round 0
  "Round counter the recorder stamps into every record.
The console increments it per round; lineage itself never does.")

(defun scalpel-lineage--git-ok (args)
  "Run git with ARGS in the session directory; return output on exit 0."
  (condition-case nil
      (with-temp-buffer
        (when (eq 0 (apply #'call-process "git" nil t nil args))
          (buffer-string)))
    (error nil)))

(defun scalpel-lineage-start ()
  "Anchor the session baseline: a dangling commit of the current worktree.
Nil on any git failure; the session then runs without a baseline and
the session-level diff degrades to per-record restore only."
  (let ((sha (string-trim
              (or (scalpel-lineage--git-ok
                   '("stash" "create" "scalpel session baseline"))
                  ""))))
    (setq scalpel-lineage--baseline
          (and (string-match-p "\\`[0-9a-f]\\{40\\}\\'" sha) sha))))

(defun scalpel-lineage-baseline ()
  "Return this session's baseline commit SHA, or nil."
  scalpel-lineage--baseline)

(defun scalpel-lineage-note (tool file old-text new-text)
  "Record one change: TOOL touched FILE turning OLD-TEXT into NEW-TEXT.
For file-rename, FILE is the new path and OLD-TEXT holds the old
path with NEW-TEXT nil; such renames are always recorded.
Writes that are a no-op are not recorded at all: when both
OLD-TEXT and NEW-TEXT are strings and equal, or when both are
nil, return nil without creating a record, so sessions whose
every write left content identical stay clean-p true and the
session-end review stays silent.  Hashes let a later restore
detect that the file moved on since the change, so it refuses
instead of clobbering user edits."
  (if (or (and (stringp old-text) (stringp new-text)
               (string= old-text new-text))
          (and (null old-text) (null new-text)))
      nil
    (let ((record
           (list :tool tool
                 :file file
                 :old-text old-text
                 :new-text new-text
                 :round scalpel-lineage-round
                 :old-hash (when old-text (secure-hash 'sha1 old-text))
                 :new-hash (when new-text (secure-hash 'sha1 new-text)))))
      (setq scalpel-lineage--records
            (append scalpel-lineage--records (list record)))
      record)))

(defun scalpel-lineage-changes ()
  "Return this session's change records, oldest first."
  scalpel-lineage--records)

(defun scalpel-lineage-clean-p ()
  "Return t when this session recorded no change at all."
  (null scalpel-lineage--records))

(defun scalpel-lineage-reset ()
  "Hold no lineage state of this session."
  (setq scalpel-lineage--records nil
        scalpel-lineage--baseline nil
        scalpel-lineage-round 0))

(defun scalpel-lineage--file-hash (file)
  "Return the SHA-1 of the content of FILE, or nil when unreadable."
  (condition-case nil
      (secure-hash
       'sha1
       (with-temp-buffer
         (insert-file-contents-literally file)
         (buffer-string)))
    (error nil)))

(defun scalpel-lineage-conflict-p (record)
  "Return non-nil when the file no longer matches what RECORD expects.
A restore in this state would either fail or clobber edits the user
made after the change landed, so it is refused rather than guessed."
  (let ((file (plist-get record :file)))
    (cond
     ((eq (plist-get record :tool) 'file-create)
      (and (file-exists-p file)
           (not (equal (scalpel-lineage--file-hash file)
                       (plist-get record :new-hash)))))
     ((eq (plist-get record :tool) 'file-rename)
      (not (and (file-exists-p file)
                (not (file-exists-p (plist-get record :old-text))))))
     (t
      (not (equal (scalpel-lineage--file-hash file)
                  (plist-get record :new-hash)))))))

(defun scalpel-lineage-restore (record)
  "Undo the single change RECORD describes.
Return t on success, or the symbol `conflict' when the file has
changed since the record was made; a conflicted restore is refused.
For a text change the whole-file hash must match first, then the
recorded new text must occur exactly once in the file, and only then
is it replaced by the recorded old text."
  (catch 'result
    (let ((tool (plist-get record :tool))
          (file (plist-get record :file)))
      (when (scalpel-lineage-conflict-p record)
        (throw 'result 'conflict))
      (cond
       ((eq tool 'file-create)
        (condition-case nil
            (progn (delete-file file) (throw 'result t))
          (error (throw 'result 'conflict))))
       ((eq tool 'file-delete)
        (condition-case nil
            (progn
              (with-temp-buffer
                (insert (plist-get record :old-text))
                (write-region (point-min) (point-max) file nil 'silent))
              (throw 'result t))
          (error (throw 'result 'conflict))))
       ((eq tool 'file-rename)
        (condition-case nil
            (progn
              (rename-file file (plist-get record :old-text))
              (throw 'result t))
          (error (throw 'result 'conflict))))
       (t
        (let* ((current (with-temp-buffer
                          (insert-file-contents-literally file)
                          (buffer-string)))
               (new-text (plist-get record :new-text))
               (old-text (plist-get record :old-text)))
          ;; Restore only when the recorded new text occurs exactly once.
          (if (and new-text
                   (let ((first (string-search new-text current)))
                     (and first
                          (not (string-search new-text current (1+ first))))))
              (let ((replaced (string-replace new-text old-text current)))
                (with-temp-buffer
                  (insert replaced)
                  (write-region (point-min) (point-max) file nil 'silent))
                t)
            'conflict)))))))

(provide 'scalpel-lineage)

;;; scalpel-lineage.el ends here
