;;; scalpel-llm.el --- LLM gateway adapter for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Scalpel LLM gateway implementation notes.

;;; Code:

(require 'gptel)
(require 'gptel-transient)

(defvar scalpel-llm-timeout 30
  "Maximum seconds to wait for an LLM response before raising an error.")

(defvar scalpel-llm--progress-callback nil
  "Optional zero-arg function called once per wait-loop iteration.")

(defun scalpel-llm--api-key-error-p (message)
  "Return non-nil when MESSAGE indicates gptel needs an API key."
  (string-match-p "gptel-api-key.*is not valid" message))

(defun scalpel-llm-request (prompt &optional system)
  "Send PROMPT to the configured gptel backend.
SYSTEM overrides the default system message.  Returns the response string.
Waits synchronously but calls `accept-process-output' so user interrupts work."
  (let* ((response nil)
         (done nil)
         (start (float-time)))
    (condition-case err
        (progn
          (gptel-request prompt
            :system system
            :stream nil
            :callback (lambda (resp _info)
                        (setq response resp)
                        (setq done t)))
          (while (not done)
            (let ((inhibit-quit t))
              (when (> (- (float-time) start) scalpel-llm-timeout)
                (user-error "Scalpel: LLM request timed out after %s seconds" scalpel-llm-timeout))
              (accept-process-output nil 0.1)
              (when scalpel-llm--progress-callback
                (funcall scalpel-llm--progress-callback))
              (redisplay))
            (when quit-flag
              (setq quit-flag nil)
              (user-error "Scalpel: LLM request interrupted")))
          (cond
           ((stringp response) response)
           ((null response)
            (user-error "Scalpel: LLM request did not return a result"))
           (t
            (user-error "Scalpel: LLM returned error: %S" response))))
      (error
       (if (scalpel-llm--api-key-error-p (error-message-string err))
           (user-error (concat "Scalpel: gptel has no valid API key.  "
                               "Run `M-x scalpel-set-backend' or press `C-c C-b' "
                               "in the *scalpel* buffer to choose a backend and enter credentials"))
         (signal (car err) (cdr err)))))))

;;;###autoload
(defun scalpel-llm-select-backend ()
  "Interactively switch the gptel backend used for Scalpel requests.
Delegates to `gptel-menu'."
  (interactive)
  (call-interactively #'gptel-menu))

(provide 'scalpel-llm)

;;; scalpel-llm.el ends here
