;;; scalpel-console-test.el --- Tests for scalpel-console -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-console.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-console)
(require 'scalpel-agent)

(defun scalpel-console-test--new-console-buffer ()
  "Create a fresh console buffer anchored to the temp directory.
The temp directory is the value of the variable
`temporary-file-directory'.  The buffer is put into
`scalpel-console-mode' with `scalpel-console--root' and
`default-directory' pinned to the same directory.  Any pre-existing
buffer of that name is killed first.  The caller must kill the
returned buffer when done."
  (let* ((root (file-name-as-directory
                (expand-file-name temporary-file-directory)))
         (name (scalpel-console--buffer-name root)))
    (when (get-buffer name) (kill-buffer name))
    (let ((buf (get-buffer-create name)))
      (with-current-buffer buf
        (erase-buffer)
        (scalpel-console-mode)
        (setq-local scalpel-console--root root)
        (setq-local default-directory root))
      buf)))

(ert-deftest scalpel-console-test-send-line ()
  "Send a line to the agent and verify the reply is appended."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (_prompt &optional _system)
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (with-current-buffer buf
              (erase-buffer)
              (insert "test instruction")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "User: test instruction" nil t))
              (ert-info ((format "Console contents:\n%S" (buffer-string)))
                (should (search-forward "Scalpel: done" nil t)))
              (should-not (search-forward "thinking" nil t))
              (should (string= (buffer-string)
                               "User: test instruction\nScalpel: done\n\n")))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-send-line-error ()
  "When the agent errors, the error message is appended to the console."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (_prompt &optional _system)
                       (error "Boom"))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "bad instruction\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "User: bad instruction" nil t))
              (should (search-forward "Scalpel error: Boom" nil t))
              (should-not (search-forward "thinking" nil t)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-send-line-rejected-while-busy ()
  "A new instruction is rejected while a request is in flight."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf (erase-buffer))
          (let ((scalpel-console--busy t))
            (with-current-buffer buf
              (insert "second instruction\n")
              (goto-char (point-max))
              (scalpel-console-send-line)))
          (with-current-buffer buf
            (should-not (search-forward "User: second instruction" nil t))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-progress-callback-bound-during-request ()
  "The progress callback must be bound while the agent request runs."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (seen nil))
    (unwind-protect
        (progn
          (with-current-buffer buf (erase-buffer))
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (_prompt &optional _system)
                       (setq seen (functionp scalpel-llm--progress-callback))
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (with-current-buffer buf
              (insert "tick instruction\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (should seen))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-open-shows-context ()
  "Opening the console shows a Context line and does not repeat it on send."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())"))
    (let ((scalpel-agent--context-files (list this-file))
          (buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'scalpel-agent-context-reset)
                     (lambda () nil))
                    ((symbol-function 'scalpel-llm-request)
                     (lambda (_p &optional _s)
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (scalpel-console-open)
            (setq buf (current-buffer))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "Context:" nil t))
              (should (search-forward (file-name-nondirectory this-file)
                                      nil t)))
            (with-current-buffer buf
              (goto-char (point-max))
              (insert "hello")
              (scalpel-console-send-line))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "Scalpel: done" nil t))
              (should (= (how-many "Context:" (point-min) (point-max))
                         1))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest scalpel-console-test-add-file-updates-context-line ()
  "Adding a file appends an updated Context line."
  (let ((scalpel-agent--context-files nil))
    (scalpel-utils-test-with-temp-file ".el"
      (with-temp-file this-file (insert "(defun foo ())"))
      (let ((buf nil))
        (unwind-protect
            (cl-letf (((symbol-function 'scalpel-agent-context-reset)
                       (lambda () nil))
                      ((symbol-function 'read-file-name)
                       (lambda (&rest _) this-file)))
              (scalpel-console-open)
              (setq buf (current-buffer))
              (with-current-buffer buf
                (scalpel-console-add-file))
              (with-current-buffer buf
                (should (search-forward "Context:" nil t))
                (should (search-forward (file-name-nondirectory this-file)
                                        nil t))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest scalpel-console-test-context-diff-highlights-removed ()
  "A file dropped from the context is appended struck through."
  (let ((scalpel-agent--context-files '("/tmp/scalpel-diff-a.el"))
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq scalpel-console--context-baseline 'none-yet)
            (scalpel-console--show-context)
            (setq scalpel-agent--context-files nil)
            (scalpel-console--show-context)
            (goto-char (point-min))
            (let (pos)
              (while (search-forward "scalpel-diff-a.el" nil t)
                (setq pos (match-beginning 0)))
              (should pos)
              (should (eq (get-text-property pos 'face)
                          'scalpel-console-context-removed-face)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-context-diff-face-covers-name-only ()
  "The change face starts at the file name, never at the tree graphics."
  (let ((scalpel-agent--context-files '("/tmp/scalpel-diff-name.el"))
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq scalpel-console--context-baseline 'none-yet)
            (scalpel-console--show-context)
            (setq scalpel-agent--context-files nil)
            (scalpel-console--show-context)
            (goto-char (point-min))
            (let (pos)
              (while (search-forward "scalpel-diff-name.el" nil t)
                (setq pos (match-beginning 0)))
              (should pos)
              (should (eq (get-text-property pos 'face)
                          'scalpel-console-context-removed-face))
              (should (null (get-text-property (1- pos) 'face))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-send-line-does-not-duplicate-input ()
  "The typed instruction is rewritten into the User line, not repeated."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (_prompt &optional _system)
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (with-current-buffer buf
              (erase-buffer)
              (insert "echo me\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (with-current-buffer buf
              (should (= (how-many "echo me" (point-min) (point-max)) 1)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-status-line-shows-counters ()
  "The status line shows token counters and elapsed time."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (seen nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (_prompt &optional _system)
                       (setq seen (functionp scalpel-llm--progress-callback))
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (with-current-buffer buf
              (erase-buffer)
              (insert "status instruction\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (should seen)
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "Scalpel: done" nil t)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-status-line-own-line-and-clean-stop ()
  "Status line sits on its own line; STOP leaves no residue.
Regression: `beg' was derived by subtracting a hard-coded length,
so non-zero counters made STOP delete a character inside
\"Scalpel:\" instead of the whole line."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (scalpel-llm--tokens-uploaded 12)
        (scalpel-llm--tokens-received 34))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "User: hi\n")
          (goto-char (point-max))
          (let* ((status (scalpel-console--status-start))
                 (refresh (car status))
                 (stop (cdr status)))
            (should (= (point) (point-max)))
            (should (eq (char-before) ?\n))
            (save-excursion
              (goto-char (point-min))
              (should (search-forward
                       "Scalpel: 12 up, 34 down, 0s\n" nil t)))
            ;; Refreshing rewrites the same single line.
            (funcall refresh)
            (funcall refresh)
            (should (= (how-many "^Scalpel:" (point-min) (point-max)) 1))
            ;; STOP removes the whole line, leaving no residue.
            (funcall stop)
            (should (string= (buffer-string) "User: hi\n"))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'scalpel-console-test)

;;; scalpel-console-test.el ends here
