;;; scalpel-console-test.el --- Tests for scalpel-console -*- lexical-binding: t; -*-
;;; Commentary:

;; Tests for scalpel-console.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-console)
(require 'scalpel-agent)

(ert-deftest scalpel-console-test-send-line ()
  "Send a line to the agent and verify the reply is appended."
  (let ((buf (get-buffer-create scalpel-console-buffer-name)))
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
      (when (get-buffer scalpel-console-buffer-name)
        (with-current-buffer scalpel-console-buffer-name
          (set-buffer-modified-p nil))
        (kill-buffer scalpel-console-buffer-name)))))

(ert-deftest scalpel-console-test-send-line-error ()
  "When the agent errors, the error message is appended to the console."
  (let ((buf (get-buffer-create scalpel-console-buffer-name)))
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
      (when (get-buffer scalpel-console-buffer-name)
        (with-current-buffer scalpel-console-buffer-name
          (set-buffer-modified-p nil))
        (kill-buffer scalpel-console-buffer-name)))))

(ert-deftest scalpel-console-test-send-line-rejected-while-busy ()
  "A new instruction is rejected while a request is in flight."
  (let ((buf (get-buffer-create scalpel-console-buffer-name)))
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
      (when (get-buffer scalpel-console-buffer-name)
        (with-current-buffer scalpel-console-buffer-name
          (set-buffer-modified-p nil))
        (kill-buffer scalpel-console-buffer-name)))))

(ert-deftest scalpel-console-test-progress-callback-bound-during-request ()
  "The progress callback must be bound while the agent request runs."
  (let ((buf (get-buffer-create scalpel-console-buffer-name))
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
      (when (get-buffer scalpel-console-buffer-name)
        (with-current-buffer scalpel-console-buffer-name
          (set-buffer-modified-p nil))
        (kill-buffer scalpel-console-buffer-name)))))

(provide 'scalpel-console-test)

;;; scalpel-console-test.el ends here
