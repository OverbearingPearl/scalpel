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

(defvar-local scalpel-lineage--records nil
  "Change records of the current buffer's session, oldest last.
Each entry is a plist: :tool :file :old-text :new-text :round
:old-hash :new-hash.  For file-create, :old-text is nil; for
file-delete, :new-text is nil; for file-rename, :file names the new
path and :old-text the original one.  Text tools record the replaced
segment in :old-text and what replaced it in :new-text, so a restore
can locate its target without a whole-file snapshot per record.

Buffer-local so each session tracks only the changes its own
conversation produced instead of sharing one global list across
sessions.  The recorder runs from agent/execute callbacks whose
current buffer is the edited file's buffer, so records land on the
buffer belonging to the file being edited.  A multi-console future
should add a session tag to further scope records per session.")

(defvar-local scalpel-lineage--baseline nil
  "Dangling git commit captured at session start, or nil.
Produced by `git stash create', which touches neither the worktree
nor any ref; the session-level diff anchors on it.
Buffer-local: each console captures its own git baseline, so one
console's anchor never leaks to another.")

(defvar-local scalpel-lineage-round 0
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
session-end review stays silent.  When the change has already
landed on disk and the current file content contains the
recorded new text exactly once, store whole-file snapshots: the
pre-change content derived by replacing the new text back with
the old text, and the post-change file content, each with a
whole-content sha1 hash.  Otherwise keep the passed segment
texts.  Whole-file snapshots let a later restore of an earlier
record succeed for a file edited several times in one session;
hashes let a later restore detect that the file moved on since
the change, so it refuses instead of clobbering user edits.
Each record is also stamped with :index, a value drawn from the
global monotonic sequence counter `scalpel-lineage--record-counter'
incremented once per record, so record order in the flat list and
index order agree; and :session, the identity of the console
session currently registered as producing changes, taken from
`scalpel-lineage--current-session'.  When no session is currently
registered, :session is stamped nil so old callers and tests keep
working."
  (if (or (and (stringp old-text) (stringp new-text)
               (string= old-text new-text))
          (and (null old-text) (null new-text)))
      nil
    (let* ((current (and (stringp old-text) (stringp new-text)
                         (file-exists-p file)
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
           (snapshot-p (and current
                            (stringp new-text)
                            (not (string= new-text ""))
                            (equal
                             (let ((first (string-search new-text current)))
                               (and first
                                    (not (string-search
                                          new-text current
                                          (1+ first)))))
                             0)))
           (record
            (if snapshot-p
                (let* ((after current)
                       (before (concat
                                (substring current 0
                                           (string-search new-text current))
                                old-text
                                (substring current
                                           (+ (string-search new-text current)
                                              (length new-text))))))
                  (list :tool tool
                        :file file
                        :old-text before
                        :new-text after
                        :round scalpel-lineage-round
                        :old-hash (secure-hash 'sha1 before)
                        :new-hash (secure-hash 'sha1 after)))
              (list :tool tool
                    :file file
                    :old-text old-text
                    :new-text new-text
                    :round scalpel-lineage-round
                    :old-hash (when old-text (secure-hash 'sha1 old-text))
                    :new-hash (when new-text (secure-hash 'sha1 new-text))))))
      (setq record
            (plist-put record :index scalpel-lineage--record-counter))
      (setq scalpel-lineage--record-counter
            (1+ scalpel-lineage--record-counter))
      (setq record
            (plist-put record :session scalpel-lineage--current-session))
      (setq scalpel-lineage--records
            (append scalpel-lineage--records (list record)))
      record)))

(defvar scalpel-lineage--record-counter 0
  "Global counter of lineage records produced so far.
Incremented by the note path for each record emitted.  Not
buffer-local: it coordinates across multiple consoles.")

(defun scalpel-lineage-record-counter ()
  "Return the current value of the global record counter."
  scalpel-lineage--record-counter)

(defvar scalpel-lineage--current-session nil
  "Identity tag of the console currently producing changes.
This is the console buffer name or a similar identity, or nil
when none is registered.")

(defvar scalpel-lineage--dialogues nil
  "List of dialogue entries, most operations append to the end.
Each entry is a plist with :session, :start and :end, where
:start and :end are sequence-index values bracketing one
user-question-to-final-answer dialogue.  :end is nil while the
dialogue is still open.")

(defun scalpel-lineage-session-begin (session &optional question)
  "Register SESSION as the console currently producing edits.
Open a new dialogue entry whose :start is the current record
counter value and whose :end is nil.  QUESTION is the user's
instruction text for this dialogue; it is stored as :question and
defaults to nil when not supplied.  The history roadmap renders
this question text truncated."
  (setq scalpel-lineage--current-session session)
  (setq scalpel-lineage--dialogues
        (append scalpel-lineage--dialogues
                (list (list :session session
                            :question question
                            :start scalpel-lineage--record-counter
                            :end nil)))))

(defun scalpel-lineage-session-end (session)
  "Close the open dialogue entry for SESSION.
Stamp the current record counter value as its :end.  Clear the
current-session tag only when it still matches SESSION, so an
aborted session cannot clear another console's registration."
  (let ((entry (seq-find
                (lambda (e)
                  (and (equal (plist-get e :session) session)
                       (null (plist-get e :end))))
                scalpel-lineage--dialogues)))
    (when entry
      (plist-put entry :end scalpel-lineage--record-counter)))
  (when (equal scalpel-lineage--current-session session)
    (setq scalpel-lineage--current-session nil)))

(defun scalpel-lineage-dialogues ()
  "Return the list of dialogue entries."
  scalpel-lineage--dialogues)

(defun scalpel-lineage-changes ()
  "Return this session's change records, oldest first."
  scalpel-lineage--records)

(defun scalpel-lineage-clean-p ()
  "Return t when this session recorded no change at all.

Records live buffer-locally on the edited file's buffers, so a
plain read of the caller's own binding usually misses them.  When
`scalpel-review-session' is loaded, scan every known dialogue
session rather than only the registered consoles: registration
happens only after a first dirty report, so filtering by the
registration list would hide unregistered consoles that already
carry recorded changes.  Otherwise fall back to the local
variable."
  (if (featurep 'scalpel-review-session)
      (progn
        (require 'scalpel-review-session)
        (let* ((known (cond
                       ((fboundp 'scalpel-review-session--all-sessions)
                        (scalpel-review-session--all-sessions))
                       ((boundp 'scalpel-review-session--sessions)
                        (symbol-value 'scalpel-review-session--sessions))
                       (t nil)))
               (records
                (if known
                    (cl-loop for session in known
                             when (fboundp 'scalpel-review-session--session-records)
                             append (scalpel-review-session--session-records session))
                  (scalpel-review-session--collect-records))))
          (null records)))
    (null scalpel-lineage--records)))

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
         (insert-file-contents file)
         (buffer-string)))
    (error nil)))

(defun scalpel-lineage-conflict-p (record)
  "Return non-nil when the file no longer matches what RECORD expects.
For text edits the check is segment-based: the recorded :new-text must
occur in the file exactly once, mirroring the test
`scalpel-lineage-restore' performs with `string-search'.  A restore in
this state would either fail or clobber edits the user made after the
change landed, so it is refused rather than guessed."
  (let ((file (plist-get record :file)))
    (cond
     ((eq (plist-get record :tool) 'file-create)
      (and (file-exists-p file)
           (not (equal (scalpel-lineage--file-hash file)
                       (plist-get record :new-hash)))))
     ((eq (plist-get record :tool) 'file-delete)
      (and (file-exists-p file)
           (not (equal (scalpel-lineage--file-hash file)
                       (plist-get record :old-hash)))))
     ((eq (plist-get record :tool) 'file-rename)
      (not (and (file-exists-p file)
                (not (file-exists-p (plist-get record :old-text))))))
     (t
      (if (not (file-readable-p file))
          t
        (let* ((content (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string)))
               (new-text (plist-get record :new-text))
               (count 0)
               (pos 0))
          (while (string-search new-text content pos)
            (setq count (1+ count)
                  pos (1+ (string-search new-text content pos))))
          (not (= count 1))))))))

(defun scalpel-lineage-restore (record)
  "Undo the single change RECORD describes.
Return t on success, or the symbol `conflict' when the file has
changed since the record was made; a conflicted restore is refused.
On success the record is marked as processed by setting its
:restored property to t, so the review buffer can later show
restored blocks as already-handled instead of pending.
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
            (progn (delete-file file)
                   (plist-put record :restored t)
                   (throw 'result t))
          (error (throw 'result 'conflict))))
       ((eq tool 'file-delete)
        (condition-case nil
            (progn
              (with-temp-buffer
                (insert (plist-get record :old-text))
                (write-region (point-min) (point-max) file nil 'silent))
              (plist-put record :restored t)
              (throw 'result t))
          (error (throw 'result 'conflict))))
       ((eq tool 'file-rename)
        (condition-case nil
            (progn
              (rename-file file (plist-get record :old-text))
              (plist-put record :restored t)
              (throw 'result t))
          (error (throw 'result 'conflict))))
       (t
        (let* ((current (with-temp-buffer
                          (insert-file-contents file)
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
                (plist-put record :restored t)
                t)
            'conflict)))))))

(provide 'scalpel-lineage)

;;; scalpel-lineage.el ends here
