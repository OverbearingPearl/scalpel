;;; scalpel-story-lineage-history-test.el --- Roadmap stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the lineage history roadmap: the
;; roadmap shows every recorded dialogue with its session, question
;; and change list; rolling back from the latest dialogue restores
;; that dialogue's change; an empty history renders a placeholder.

;;; Code:

(require 'ert)
(require 'scalpel-lineage)
(require 'scalpel-lineage-history)

;; Story A: the roadmap renders each dialogue with its session,
;; question and recorded changes.
(ert-deftest scalpel-story-lineage-history-test-renders-dialogues ()
  (let ((rec-buf (get-buffer-create " *story-lh-rec*")))
    (unwind-protect
        (progn
          (with-current-buffer rec-buf
            (scalpel-lineage-reset)
            (scalpel-lineage-session-begin "console-a" "fix the parser")
            (scalpel-lineage-note 'block-edit "/tmp/a.el" "old\n" "new\n")
            (scalpel-lineage-session-end "console-a"))
          (with-temp-buffer
            (scalpel-lineage-history--render)
            (let ((text (buffer-string)))
              (should (string-search "session: console-a" text))
              (should (string-search "Q: fix the parser" text))
              (should (string-search "/tmp/a.el" text))
              (should (string-search "1 change(s)" text)))))
      (kill-buffer rec-buf)
      (scalpel-lineage-reset))))

;; Story B: rolling back from the latest dialogue restores that
;; dialogue's change, turning the file back to the prior content.
(ert-deftest scalpel-story-lineage-history-test-rollback-restores ()
  (let ((root (make-temp-file "scalpel-story-lh-" t))
        (file nil)
        (rec-buf (get-buffer-create " *story-lh-rec*")))
    (unwind-protect
        (progn
          (setq file (expand-file-name "f.el" root))
          (with-current-buffer rec-buf
            (scalpel-lineage-reset)
            (with-temp-file file (insert "old\n"))
            (scalpel-lineage-session-begin "s" "first")
            (scalpel-lineage-note 'block-edit file "old\n" "mid\n")
            (scalpel-lineage-session-end "s")
            (with-temp-file file (insert "mid\n"))
            (scalpel-lineage-session-begin "s" "second")
            (scalpel-lineage-note 'block-edit file "mid\n" "new\n")
            (scalpel-lineage-session-end "s")
            (with-temp-file file (insert "new\n")))
          ;; Given a roadmap with two dialogues over one file, when the
          ;; user rolls back from the first (earliest) dialogue node,
          ;; the rollback restores records newest first, undoing every
          ;; later change until the original content is back.
          (with-temp-buffer
            (scalpel-lineage-history--render)
            (goto-char (point-min))
            (let ((inhibit-message t))
              (scalpel-lineage-history-rollback))
            ;; Then the file holds the original content, i.e. both
            ;; changes were undone newest-first.
            (should (string=
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     "old\n"))))
      (condition-case nil (delete-directory root t) (error nil))
      (kill-buffer rec-buf)
      (scalpel-lineage-reset))))

;; Story C: an empty history renders a placeholder, not an error.
(ert-deftest scalpel-story-lineage-history-test-empty-placeholder ()
  (let ((rec-buf (get-buffer-create " *story-lh-rec*")))
    (unwind-protect
        (progn
          (with-current-buffer rec-buf
            (scalpel-lineage-reset))
          (with-temp-buffer
            (scalpel-lineage-history--render)
            (should (string-search "No lineage dialogues recorded."
                                   (buffer-string)))))
      (kill-buffer rec-buf)
      (scalpel-lineage-reset))))

(provide 'scalpel-story-lineage-history-test)

;;; scalpel-story-lineage-history-test.el ends here
