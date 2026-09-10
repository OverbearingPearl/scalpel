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
              (insert "test instruction\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "User: test instruction" nil t))
              (should (search-forward "Scalpel: done" nil t))
              (should-not (search-forward "thinking" nil t)))))
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
              (should (search-forward
                       (concat "Context: " (expand-file-name this-file))
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
                (should (search-forward
                         (concat "Context: " (expand-file-name this-file))
                         nil t))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(provide 'scalpel-console-test)

;;; scalpel-console-test.el ends here
