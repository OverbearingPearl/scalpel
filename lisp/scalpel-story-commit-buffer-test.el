;;; scalpel-story-commit-buffer-test.el --- Commit buffer stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the commit-message buffer, from
;; the moment the message is shown to the moment it is committed.
;; The LLM itself is never called: a story renders a message as if
;; the model had answered, then exercises what the user can do with
;; it -- read the body, regenerate without the old answer piling up,
;; and be refused when the buffer no longer describes a committable
;; state.  Every clause is a single form and every THEN is one
;; should form; buffers created here are killed in cleanup.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-commit)

(defvar scalpel-story-commit-buffer-test--buffer nil
  "Buffer a story is exercising.")

(defvar scalpel-story-commit-buffer-test--report nil
  "Value a story WHEN clause captured for its THEN clauses.")

(ert-gwt-deftest
  (:given ())
  (:when (progn
           (setq scalpel-story-commit-buffer-test--buffer
                 (get-buffer-create " *story commit render*"))
           (with-current-buffer scalpel-story-commit-buffer-test--buffer
             (scalpel-commit--insert-message
              "feat: add a thing\n\nWhy and what.")
             (setq scalpel-story-commit-buffer-test--report
                   (scalpel-commit--message-text)))))
  (:then (should (string= scalpel-story-commit-buffer-test--report
                          "feat: add a thing\n\nWhy and what.")))
  (:cleanup (when (buffer-live-p scalpel-story-commit-buffer-test--buffer)
              (kill-buffer
               scalpel-story-commit-buffer-test--buffer))))

(ert-gwt-deftest
  (:given ())
  (:when (progn
           (setq scalpel-story-commit-buffer-test--buffer
                 (get-buffer-create " *story commit regen*"))
           (with-current-buffer scalpel-story-commit-buffer-test--buffer
             (scalpel-commit--insert-message "first subject")
             (scalpel-commit--insert-message "second subject")
             (setq scalpel-story-commit-buffer-test--report
                   (list (scalpel-commit--message-text)
                         (buffer-string))))))
  (:then (should (equal (car scalpel-story-commit-buffer-test--report)
                        "second subject")))
  (:then (should (equal (cadr scalpel-story-commit-buffer-test--report)
                        "--- BEGIN COMMIT MESSAGE ---\nsecond subject\n--- END COMMIT MESSAGE ---\n")))
  (:cleanup (when (buffer-live-p scalpel-story-commit-buffer-test--buffer)
              (kill-buffer
               scalpel-story-commit-buffer-test--buffer))))

(ert-gwt-deftest
  (:given ())
  (:when (progn
           (setq scalpel-story-commit-buffer-test--buffer
                 (get-buffer-create " *story commit name*"))
           (with-current-buffer scalpel-story-commit-buffer-test--buffer
             (setq-local scalpel-commit--workdir-cache
                         "/tmp/scalpel-commit-story/myrepo/")
             (setq scalpel-story-commit-buffer-test--report
                   (scalpel-commit--buffer-name)))))
  (:then (should (string= scalpel-story-commit-buffer-test--report
                          "*scalpel commit: myrepo*")))
  (:cleanup (when (buffer-live-p scalpel-story-commit-buffer-test--buffer)
              (kill-buffer
               scalpel-story-commit-buffer-test--buffer))))

(ert-gwt-deftest
  (:given ())
  (:when (progn
           (setq scalpel-story-commit-buffer-test--buffer
                 (get-buffer-create " *story commit torn*"))
           (with-current-buffer scalpel-story-commit-buffer-test--buffer
             (insert "--- BEGIN COMMIT MESSAGE ---\nbody only")
             (setq scalpel-story-commit-buffer-test--report
                   (scalpel-commit--message-text)))))
  (:then (should (null scalpel-story-commit-buffer-test--report)))
  (:cleanup (when (buffer-live-p scalpel-story-commit-buffer-test--buffer)
              (kill-buffer
               scalpel-story-commit-buffer-test--buffer))))

(ert-gwt-deftest
  (:given ())
  (:when (progn
           (setq scalpel-story-commit-buffer-test--buffer
                 (get-buffer-create " *story commit refuse*"))
           (with-current-buffer scalpel-story-commit-buffer-test--buffer
             (scalpel-commit-mode)
             (setq-local scalpel-commit--workdir-cache
                         "/tmp/scalpel-commit-story")
             (insert "just prose, no markers"))))
  (:then (should-error
          (with-current-buffer
              scalpel-story-commit-buffer-test--buffer
            (scalpel-commit--commit))))
  (:cleanup (when (buffer-live-p scalpel-story-commit-buffer-test--buffer)
              (kill-buffer
               scalpel-story-commit-buffer-test--buffer))))

(ert-gwt-deftest
  (:given ((diff (concat "diff --git a/aaa b/aaa\n@@ -1 +1 @@\n"
                         "-old\n+new\n\ndiff --git a/bbb b/bbb\n"
                         "@@ -1 +1 @@\n-old\n+new\n"))
           (scalpel-commit-diff-max-bytes 40)))
  (:when (setq scalpel-story-commit-buffer-test--report
               (scalpel-commit--diff-batches diff)))
  (:then (should (= (length
                     scalpel-story-commit-buffer-test--report)
                    2)))
  (:then (should (string-prefix-p
                  "diff --git "
                  (car scalpel-story-commit-buffer-test--report))))
  (:then (should (string-prefix-p
                  "diff --git "
                  (cadr scalpel-story-commit-buffer-test--report))))
  (:cleanup))

(provide 'scalpel-story-commit-buffer-test)

;;; scalpel-story-commit-buffer-test.el ends here
