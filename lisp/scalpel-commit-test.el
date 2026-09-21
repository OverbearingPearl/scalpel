;;; scalpel-commit-test.el --- Tests for scalpel-commit -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the LLM-assisted commit message feature: the git
;; plumbing, diff batching, prompt construction and the commit
;; buffer's rendering and confirmation logic.

;;; Code:

(require 'ert)
(require 'scalpel-commit)

(ert-deftest scalpel-commit-test-diff-stages-and-diffs-tracked-files ()
  "The diff runs `git add -u' first and reads one combined diff."
  (let* ((calls nil))
    (cl-letf (((symbol-function 'scalpel-commit--git)
               (lambda (args &optional _workdir)
                 (push args calls)
                 "diff output")))
      (should (equal "diff output" (scalpel-commit--diff "/tmp")))
      (should (equal (list (list "diff" "--staged" "--cached" "-U10"
                                 "--function-context" "--no-color"
                                 "--no-ext-diff")
                           (list "add" "-u"))
                     calls)))))

(ert-deftest scalpel-commit-test-git-returns-nil-on-failure ()
  "A failed git command yields nil, not an error."
  (should (null (scalpel-commit--git (list "status" "--porcelain")
                                     "/"))))

(ert-deftest scalpel-commit-test-diff-batches-split-on-file-boundary ()
  "An oversized diff is split where files start, never mid-file."
  (let* ((file (concat "diff --git a/f b/f\n"
                       (make-string 150 ?x)
                       "\n"))
         (scalpel-commit-diff-max-bytes 100)
         (pieces (scalpel-commit--diff-batches
                  (concat file file file))))
    (should (= 3 (length pieces)))
    (dolist (piece pieces)
      (should (string-suffix-p "\n" piece))
      (should (string-prefix-p "diff --git" piece)))))

(ert-deftest scalpel-commit-test-diff-batches-one-piece-when-small ()
  "A diff under the limit stays in one piece."
  (should (equal (list "small")
                 (let ((scalpel-commit-diff-max-bytes 1000))
                   (scalpel-commit--diff-batches "small")))))

(ert-deftest scalpel-commit-test-build-prompt-names-the-rules ()
  "The prompt carries the house rules, style, language and extra."
  (let ((prompt (scalpel-commit--build-prompt
                 "THE DIFF" 'angular "Chinese"
                 "more detail is wanted")))
    (should (string-match-p "imperative" prompt))
    (should (string-match-p (number-to-string
                             scalpel-commit-subject-max)
                            prompt))
    (should (string-match-p (number-to-string
                             scalpel-commit-body-width)
                            prompt))
    (should (string-match-p "Chinese" prompt))
    (should (string-match-p "THE DIFF" prompt))
    (should (string-match-p "more detail" prompt))
    (should (string-match-p "angular" prompt))))

(ert-deftest scalpel-commit-test-style-prompt-covers-each-style ()
  "Both styles get wording; an unknown style gets nothing."
  (should (string-match-p "type(scope)"
                          (scalpel-commit--style-prompt 'angular)))
  (should (string-match-p "kernel"
                          (scalpel-commit--style-prompt 'linux)))
  (should (equal "" (scalpel-commit--style-prompt 'other))))

(ert-deftest scalpel-commit-test-tree-changed-p-compares-snapshot ()
  "The confirm check fires only when the porcelain status moved."
  (with-temp-buffer
    (setq scalpel-commit--tree-state "M  a.el")
    (cl-letf (((symbol-function 'scalpel-commit--status)
               (lambda (_workdir) "M  a.el")))
      (should-not (scalpel-commit--tree-changed-p "/tmp")))
    (cl-letf (((symbol-function 'scalpel-commit--status)
               (lambda (_workdir) "M  a.el\nM  b.el")))
      (should (scalpel-commit--tree-changed-p "/tmp")))))

(ert-deftest scalpel-commit-test-render-creates-header-and-body ()
  "Rendering builds the buffer with the header and the message."
  (let ((buffer (get-buffer-create (scalpel-commit--buffer-name))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    (unwind-protect
        (with-temp-buffer
          (scalpel-commit--render "subject\n\nbody")
          (with-current-buffer (get-buffer (scalpel-commit--buffer-name))
            (should (derived-mode-p 'scalpel-commit-mode))
            (should (string-match-p "C-c C-c commit"
                                    (buffer-string)))
            (should (string-match-p "^--- BEGIN COMMIT MESSAGE ---"
                                    (buffer-string)))
            (should (string-match-p "subject\n\nbody"
                                    (buffer-string)))))
      (kill-buffer buffer))))

(ert-deftest scalpel-commit-test-insert-message-replaces-not-appends ()
  "A second render replaces the old message instead of piling up."
  (let ((buffer (get-buffer-create (scalpel-commit--buffer-name))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "--- BEGIN COMMIT MESSAGE ---\nfirst\n--- END COMMIT MESSAGE ---\n"))
    (unwind-protect
        (with-current-buffer buffer
          (scalpel-commit--insert-message "second")
          (should-not (string-match-p "first" (buffer-string)))
          (should (string-match-p "second" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest scalpel-commit-test-message-text-reads-below-marker ()
  "The committed message is exactly what sits below the marker."
  (with-temp-buffer
    (insert "header\n--- BEGIN COMMIT MESSAGE ---\n  msg \n--- END COMMIT MESSAGE ---\n")
    (should (equal "msg" (scalpel-commit--message-text)))))

(ert-deftest scalpel-commit-test-commit-refuses-stale-tree ()
  "Committing a tree that changed since generation is refused."
  (with-temp-buffer
    (setq scalpel-commit--console (current-buffer))
    (setq scalpel-commit--tree-state "M  a.el")
    (insert "--- BEGIN COMMIT MESSAGE ---\nmsg\n--- END COMMIT MESSAGE ---\n")
    (cl-letf (((symbol-function 'scalpel-commit--status)
               (lambda (_workdir) "M  b.el")))
      (should-error (scalpel-commit--commit)))))

(ert-deftest scalpel-commit-test-commit-runs-git-and-closes-buffer ()
  "A confirmed commit runs `git commit' with the shown message."
  (let* ((args nil)
         (buffer (get-buffer-create (scalpel-commit--buffer-name)))
         (console (get-buffer-create " *scalpel-commit-test console*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "--- BEGIN COMMIT MESSAGE ---\nmsg\n--- END COMMIT MESSAGE ---\n")
      (setq scalpel-commit--console console)
      (with-current-buffer console
        (setq scalpel-console--root "/tmp/scalpel-commit-test-root"))
      (setq scalpel-commit--workdir-cache default-directory)
      (setq scalpel-commit--tree-state nil))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'scalpel-commit--git)
                     (lambda (call &optional _dir) (setq args call) ""))
                    ((symbol-function 'scalpel-commit--status)
                     (lambda (_dir) nil)))
            (scalpel-commit--commit)
            (should (equal (list "commit" "-m" "msg")
                           args))
            (should-not (get-buffer (scalpel-commit--buffer-name)))))
      (condition-case nil (kill-buffer buffer) (error nil))
      (condition-case nil (kill-buffer console) (error nil)))))

(ert-deftest scalpel-commit-test-mode-map-binds-the-promised-keys ()
  "Every key the header advertises is really bound."
  (dolist (key '("C-c C-c" "C-c C-w" "C-c C-s" "C-c C-n"
                 "C-c C-e" "C-c C-t" "C-c C-k"))
    (should (lookup-key scalpel-commit-mode-map (kbd key)))))

(ert-deftest scalpel-commit-test-session-install-writes-the-console ()
  "The style and language answers land in the console's locals."
  (let ((console (get-buffer-create " *scalpel-commit-test console*")))
    (unwind-protect
        (progn
          (scalpel-commit--session-install console 'linux "Chinese")
          (with-current-buffer console
            (should (eq scalpel-commit--style 'linux))
            (should (equal "Chinese" scalpel-commit--language))))
      (kill-buffer console))))

(provide 'scalpel-commit-test)

;;; scalpel-commit-test.el ends here
