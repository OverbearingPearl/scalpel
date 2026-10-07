;;; scalpel-story-lineage-test.el --- Lineage and review stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the session lineage and the
;; session-end review: recording a change, a no-op write staying
;; unrecorded, restoring a recorded change, refusing a conflicted
;; restore, and the review buffer staying silent when nothing was
;; recorded.  Every test cleans up its own temporary files and
;; buffers, and no global state survives a run.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-lineage)
(require 'scalpel-review)

;; Story A: a recorded edit is restored exactly once.
(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-lineage-" t))
           (file (expand-file-name "a.el" root))
           (result nil))
          (scalpel-lineage-reset)
          (with-temp-file file (insert "new\n")))
  (:given ((record (scalpel-lineage-note 'block-edit file "old\n" "new\n"))))
  (:when (setq result (scalpel-lineage-restore record)))
  (:then (should (eq result t)))
  (:then (should (string=
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))
                  "old\n")))
  (:cleanup (condition-case nil (delete-directory root t) (error nil))
            (scalpel-lineage-reset)))

;; Story B: a no-op write leaves no record, so the session stays clean.
(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-lineage-" t)))
          (scalpel-lineage-reset))
  (:when (scalpel-lineage-note 'block-edit
                               (expand-file-name "b.el" root)
                               "same\n"
                               "same\n"))
  (:then (should (scalpel-lineage-clean-p)))
  (:cleanup (condition-case nil (delete-directory root t) (error nil))
            (scalpel-lineage-reset)))

;; Story C: restoring after the file moved on refuses without clobbering.
(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-lineage-" t))
           (file (expand-file-name "c.el" root))
           (result nil))
          (with-temp-file file (insert "original\n"))
          (scalpel-lineage-reset))
  (:given ((record (scalpel-lineage-note 'block-edit file
                                         "original\n"
                                         "recorded\n")))
          (with-temp-file file (insert "recorded\n"))
          ;; Conflict is now segment-based: the recorded new-text must
          ;; still occur in the file. Overwrite the file entirely so
          ;; the recorded "recorded\n" segment is gone.
          (with-temp-file file (insert "user content\n")))
  (:when (setq result (scalpel-lineage-restore record)))
  (:then (should (eq result 'conflict)))
  (:then (should (string-search "user content"
                                (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string)))))
  (:then (should (not (string-search "recorded"
                                     (with-temp-buffer
                                       (insert-file-contents file)
                                       (buffer-string))))))
  (:cleanup (condition-case nil (delete-directory root t) (error nil))
            (scalpel-lineage-reset)))

;; Story D: the review stays silent when the session recorded nothing.
(ert-gwt-deftest
  (:given ((old-records scalpel-lineage--records))
          (scalpel-lineage-reset)
          ;; Kill any pre-existing review buffer left over from earlier
          ;; tests so this test only observes its own behavior.
          (when (get-buffer "*scalpel session review*")
            (kill-buffer "*scalpel session review*")))
  (:when (let ((inhibit-message t))
           (scalpel-review-open)))
  (:then (should (not (get-buffer "*scalpel session review*"))))
  (:cleanup (setq scalpel-lineage--records old-records)))

;; Story E: the review renders a recorded edit as a diff block.
(ert-gwt-deftest
  (:given ((buffer (get-buffer-create " *review render*"))
           (scalpel-agent--context-files nil))
          (with-current-buffer buffer
            (scalpel-lineage-reset)
            (scalpel-lineage-note 'block-edit
                                  "/tmp/scalpel-review-example.el"
                                  "old\n"
                                  "new\n")))
  (:when (with-current-buffer buffer
           (erase-buffer)
           (let ((inhibit-read-only t))
             (scalpel-review--render))))
  (:then (with-current-buffer buffer
           (should (string-search "/tmp/scalpel-review-example.el"
                                  (buffer-string)))))
  (:then (with-current-buffer buffer
           ;; The header lines precede the first block, so locate the
           ;; block boundary by the record text property.
           (let ((block-pos (next-single-property-change
                             (point-min) 'scalpel-review-record)))
             (should block-pos)
             (should (get-text-property block-pos 'scalpel-review-record)))))
  (:cleanup (when (buffer-live-p buffer)
              (kill-buffer buffer))
            (scalpel-lineage-reset)))

(provide 'scalpel-story-lineage-test)

;;; scalpel-story-lineage-test.el ends here
