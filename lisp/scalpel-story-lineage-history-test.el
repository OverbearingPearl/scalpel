;;; scalpel-story-lineage-history-test.el --- Roadmap stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the lineage history roadmap: the
;; roadmap shows every recorded dialogue with its session, question
;; and change list; rolling back from the earliest dialogue restores
;; the original content; an empty history renders a placeholder.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-lineage)
(require 'scalpel-lineage-history)

;; Story A: the roadmap renders each dialogue with its session,
;; question and recorded changes.
(ert-gwt-deftest
  (:given ((rec-buf (get-buffer-create " *story-lh-rec*"))
           (text nil))
          (with-current-buffer rec-buf
            (scalpel-lineage-reset)
            (scalpel-lineage-session-begin "console-a" "fix the parser")
            (scalpel-lineage-note 'block-edit "/tmp/a.el" "old\n" "new\n")
            (scalpel-lineage-session-end "console-a")))
  (:when (with-temp-buffer
           (scalpel-lineage-history--render)
           (setq text (buffer-string))))
  (:then (should (string-search "session: console-a" text)))
  (:then (should (string-search "Q: fix the parser" text)))
  (:then (should (string-search "/tmp/a.el" text)))
  (:then (should (string-search "1 change(s)" text)))
  (:cleanup (kill-buffer rec-buf)
            (scalpel-lineage-reset)))

;; Story B: rolling back from the earliest dialogue node restores
;; records newest first, so the file returns to its original content.
(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-lh-" t))
           (file nil)
           (rec-buf (get-buffer-create " *story-lh-rec*")))
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
            (with-temp-file file (insert "new\n"))))
  (:when (with-temp-buffer
           (scalpel-lineage-history--render)
           (goto-char (point-min))
           (let ((inhibit-message t))
             (scalpel-lineage-history-rollback))))
  (:then (should (string=
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))
                  "old\n")))
  (:cleanup (condition-case nil (delete-directory root t) (error nil))
            (kill-buffer rec-buf)
            (scalpel-lineage-reset)))

;; Story C: an empty history renders a placeholder, not an error.
(ert-gwt-deftest
  (:given ((rec-buf (get-buffer-create " *story-lh-rec*"))
           (text nil))
          (with-current-buffer rec-buf
            (scalpel-lineage-reset)))
  (:when (setq text (with-temp-buffer
                      (scalpel-lineage-history--render)
                      (buffer-string))))
  (:then (should (string-search "No lineage dialogues recorded." text)))
  (:cleanup (kill-buffer rec-buf)
            (scalpel-lineage-reset)))

(provide 'scalpel-story-lineage-history-test)

;;; scalpel-story-lineage-history-test.el ends here
