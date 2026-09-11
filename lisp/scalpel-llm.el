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
  "Seconds of total callback silence before a request is abandoned.
This is an *idle* budget, not a wall-clock deadline: every gptel
callback -- content chunk, reasoning chunk, or completion -- resets the
clock, so a slow but active stream is never killed.  Time spent queued
inside gptel counts as idle, because no callback arrives during it.
Raise this for backends that think before emitting their first chunk.")

(defvar scalpel-llm--request-id 0
  "Monotonic id of the most recent request.
A callback captures the id it was created under and drops itself once
that id no longer matches, so a request abandoned by a timeout cannot
leak into the next one.")

(defvar scalpel-llm--progress-callback nil
  "Optional zero-arg function called once per wait-loop iteration.")

(defvar scalpel-llm--tokens-uploaded 0
  "Approximate number of tokens sent in the current request.")

(defvar scalpel-llm--tokens-received 0
  "Approximate number of tokens received in the current request.")

(defconst scalpel-llm-reasoning-buffer-name "*scalpel-thinking*"
  "Buffer name for collecting streaming reasoning chunks.")

(defun scalpel-llm--api-key-error-p (message)
  "Return non-nil when MESSAGE indicates gptel needs an API key."
  (string-match-p "gptel-api-key.*is not valid" message))

(defun scalpel-llm--count-tokens (text)
  "Return an approximate token count for TEXT.
Uses a 4-characters-per-token heuristic; accurate enough for
status display."
  (/ (length text) 4))

(defun scalpel-llm--reset-reasoning-buffer ()
  "Clear the reasoning buffer for a new request."
  (with-current-buffer (get-buffer-create scalpel-llm-reasoning-buffer-name)
    (erase-buffer)))

(defun scalpel-llm--append-reasoning (chunk)
  "Append CHUNK to the reasoning buffer."
  (with-current-buffer (get-buffer-create scalpel-llm-reasoning-buffer-name)
    (goto-char (point-max))
    (insert chunk)))

(defun scalpel-llm-request (prompt &optional system)
  "Send PROMPT to the configured gptel backend.
SYSTEM overrides the default system message.  Returns the response string.
Waits synchronously but calls `accept-process-output' so user interrupts work.
Abandons the request after `scalpel-llm-timeout' seconds without any callback;
abandoning invalidates the callback, so a late response cannot corrupt a later
request.  The underlying gptel request keeps running until gptel settles it;
only its callback is dropped."
  (let* ((response nil)
         (done nil)
         (request-id (setq scalpel-llm--request-id
                           (1+ scalpel-llm--request-id)))
         (last-activity (float-time))
         (accumulated "")
         (upload-tokens (scalpel-llm--count-tokens prompt))
         (received-tokens 0))
    (setq scalpel-llm--tokens-uploaded upload-tokens)
    (setq scalpel-llm--tokens-received 0)
    (scalpel-llm--reset-reasoning-buffer)
    (condition-case err
        (progn
          (gptel-request prompt
            :system system
            :stream t
            :callback (lambda (resp info)
                        ;; A callback from an abandoned request must not touch
                        ;; shared state: drop it unless it belongs to the
                        ;; newest request.
                        (when (eql request-id scalpel-llm--request-id)
                          (setq last-activity (float-time))
                          (cond
                           ;; End of streamed response: gptel signals success
                           ;; by calling back with RESPONSE = t.
                           ((eq resp t)
                            (setq response accumulated)
                            (setq done t))
                           ;; Failure: gptel calls back with a nil RESPONSE;
                           ;; the human-readable cause is in INFO's :status.
                           ((null resp)
                            (setq response
                                  (cons 'error
                                        (or (plist-get info :status) "unknown")))
                            (setq done t))
                           ;; Content chunk.
                           ((stringp resp)
                            (setq accumulated (concat accumulated resp))
                            (setq received-tokens
                                  (+ received-tokens
                                     (scalpel-llm--count-tokens resp)))
                            (setq scalpel-llm--tokens-received received-tokens)
                            (when scalpel-llm--progress-callback
                              (funcall scalpel-llm--progress-callback)))
                           ;; Reasoning chunk: delivered as the RESPONSE
                           ;; argument (a (reasoning . TEXT) cons), never as
                           ;; an INFO key.
                           ((and (consp resp) (eq (car resp) 'reasoning))
                            (when (stringp (cdr resp))
                              (scalpel-llm--append-reasoning (cdr resp))
                              (when scalpel-llm--progress-callback
                                (funcall scalpel-llm--progress-callback))))))))
          (while (not done)
            (let ((inhibit-quit t))
              (when (> (- (float-time) last-activity) scalpel-llm-timeout)
                ;; Invalidate the request before bailing out: any callback
                ;; still in flight is now dropped instead of mutating the
                ;; token counters or the reasoning buffer.
                (setq scalpel-llm--request-id (1+ scalpel-llm--request-id))
                (user-error "Scalpel: LLM request idle for more than %s seconds"
                            scalpel-llm-timeout))
              (accept-process-output nil 0.1)
              (when scalpel-llm--progress-callback
                (funcall scalpel-llm--progress-callback))
              (redisplay))
            (when quit-flag
              (setq quit-flag nil)
              (setq scalpel-llm--request-id (1+ scalpel-llm--request-id))
              (user-error "Scalpel: LLM request interrupted")))
          (cond
           ((stringp response) response)
           ((null response)
            (user-error "Scalpel: LLM request did not return a result"))
           ((and (consp response) (eq (car response) 'error))
            (user-error "Scalpel: LLM returned error: %S" (cdr response)))
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
