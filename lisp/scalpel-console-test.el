;;; scalpel-console-test.el --- Tests for scalpel-console -*- lexical-binding: t; -*-
;;; Commentary:
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
              (should (search-forward "Scalpel: done" nil t)))))
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
              (should (search-forward "Scalpel error: Boom" nil t)))))
      (when (get-buffer scalpel-console-buffer-name)
        (with-current-buffer scalpel-console-buffer-name
          (set-buffer-modified-p nil))
        (kill-buffer scalpel-console-buffer-name)))))

(provide 'scalpel-console-test)
;;; scalpel-console-test.el ends here
