;;; scalpel-story-lineage-test.el --- Lineage and review stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the session lineage and the
;; session-end review: recording a change, a no-op write staying
;; unrecorded, restoring a recorded change, refusing a conflicted
;; restore, and the review buffer staying silent when nothing was
;; recorded.  Every test cleans up its own temporary files and
;; buffers, and no global state survives a run.

;;; Code:

(require 'ert)
(require 'scalpel-lineage)
(require 'scalpel-review)

;; Story A: a recorded edit is restored exactly once.
(ert-deftest scalpel-story-lineage-test-restore-succeeds ()
  (let ((root (make-temp-file "scalpel-lineage-" t))
        (file nil)
        (record nil))
    (unwind-protect
        (progn
          (setq file (expand-file-name "a.el" root))
          (scalpel-lineage-reset)
          (setq record (scalpel-lineage-note 'block-edit file "old\n" "new\n"))
          (with-temp-file file (insert "new\n"))
          ;; Then the recorded change restores the old content.
          (should (eq (scalpel-lineage-restore record) t))
          (should (string=
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))
                   "old\n")))
      (condition-case nil (delete-directory root t) (error nil))
      (scalpel-lineage-reset))))

;; Story B: a no-op write leaves no record, so the session stays clean.
(ert-deftest scalpel-story-lineage-test-noop-unrecorded ()
  (let ((root (make-temp-file "scalpel-lineage-" t)))
    (unwind-protect
        (progn
          (scalpel-lineage-reset)
          ;; Given a write whose old and new content are identical.
          (let ((record (scalpel-lineage-note 'block-edit
                                              (expand-file-name "b.el" root)
                                              "same\n"
                                              "same\n")))
            ;; Then nothing is recorded and the session counts as clean.
            (should-not record)
            (should (scalpel-lineage-clean-p))))
      (condition-case nil (delete-directory root t) (error nil))
      (scalpel-lineage-reset))))

;; Story C: restoring after the file moved on refuses without clobbering.
(ert-deftest scalpel-story-lineage-test-conflict-refused ()
  (let ((root (make-temp-file "scalpel-lineage-" t))
        (file nil)
        (record nil))
    (unwind-protect
        (progn
          (setq file (expand-file-name "c.el" root))
          (with-temp-file file (insert "original\n"))
          (scalpel-lineage-reset)
          (setq record (scalpel-lineage-note 'block-edit file
                                             "original\n"
                                             "recorded\n"))
          (with-temp-file file (insert "recorded\n"))
          ;; The user edits the file after the change landed, so the
          ;; whole-file hash no longer matches the record.
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-max))
            (insert "user edit\n")
            (write-region (point-min) (point-max) file nil 'silent))
          ;; Then the restore refuses and the user edit survives.
          (should (eq (scalpel-lineage-restore record) 'conflict))
          (should (string-search "user edit"
                                 (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string)))))
      (condition-case nil (delete-directory root t) (error nil))
      (scalpel-lineage-reset))))

;; Story D: the review stays silent when the session recorded nothing.
(ert-deftest scalpel-story-lineage-test-silent-when-clean ()
  (let ((buffers-before (buffer-list)))
    (unwind-protect
        (progn
          (scalpel-lineage-reset)
          (let ((inhibit-message t))
            (scalpel-review-open))
          ;; Then no review buffer exists and no new buffer appeared.
          (should-not (get-buffer "*scalpel session review*"))
          (should (equal (buffer-list) buffers-before)))
      (scalpel-lineage-reset))))

;; Story E: the review renders a recorded edit as a diff block.
(ert-deftest scalpel-story-lineage-test-renders-recorded-edit ()
  (let ((review-buf nil))
    (unwind-protect
        (progn
          (scalpel-lineage-reset)
          (scalpel-lineage-note 'block-edit
                                "/tmp/scalpel-review-example.el"
                                "old\n"
                                "new\n")
          (with-current-buffer (get-buffer-create " *review render*")
            (erase-buffer)
            (let ((inhibit-read-only t))
              (scalpel-review--render))
            (setq review-buf (current-buffer))
            ;; Then the buffer lists the file and tags each block with
            ;; its record so navigation can find it.  The header lines
            ;; precede the first block, so locate the block boundary.
            (should (string-search "/tmp/scalpel-review-example.el"
                                   (buffer-string)))
            (let ((block-pos (next-single-property-change
                              (point-min) 'scalpel-review-record)))
              (should block-pos)
              (should (get-text-property block-pos 'scalpel-review-record)))))
      (when (buffer-live-p review-buf)
        (kill-buffer review-buf))
      (scalpel-lineage-reset))))

(provide 'scalpel-story-lineage-test)

;;; scalpel-story-lineage-test.el ends here
