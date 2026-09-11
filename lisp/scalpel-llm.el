;;; scalpel-llm.el --- LLM gateway adapter for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Asynchronous bridge to gptel.  A request streams through a callback that
;; accumulates content, collects reasoning chunks, and feeds token counters to
;; the console status line; it settles exactly once, through ON-SUCCESS or
;; ON-ERROR.  A request with no callback activity for `scalpel-llm-timeout'
;; seconds is abandoned by its idle timer, and its callbacks are dropped, so a
;; late response cannot corrupt a later request.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-transient)

(defvar scalpel-llm-timeout 30
  "Seconds of total callback silence before a request is abandoned.
This is an *idle* budget, not a wall-clock deadline: every gptel
callback -- content chunk, reasoning chunk, or completion -- resets the
clock, so a slow but active stream is never killed.  Time spent queued
inside gptel counts as idle, because no callback arrives during it.
Raise this for backends that think before emitting their first chunk.")

(defvar scalpel-llm--progress-callback nil
  "Optional zero-arg function called on every streaming callback.
The console binds it to a status-line refresh for the duration of a
round; it is nil when no request is in flight.")

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

(defun scalpel-llm-request-async (prompt on-success on-error &optional system)
  "Send PROMPT to the configured gptel backend without blocking.
Return immediately.  ON-SUCCESS is called with the accumulated
response string once the stream ends.  ON-ERROR is called with a
plist (:type SYMBOL :message STRING) when the request fails, stays
idle for `scalpel-llm-timeout' seconds without any callback, or
cannot be set up.  SYSTEM overrides the default system message.

The request holds no global cancellation state: it is abandoned
either by its own idle timer or when the stream ends, and either
path sets the request's `cancelled' flag.  A callback that arrives
after the flag is set is dropped, so a late response cannot corrupt
a later request.

:type is `idle' for the idle timer, `api' for a backend error
surfaced by gptel, `api-key' for a missing or invalid API key, and
`setup' for a failure raised synchronously by `gptel-request'."
  (let* ((cancelled nil)
         (accumulated "")
         (timer nil))
    (setq scalpel-llm--tokens-uploaded (scalpel-llm--count-tokens prompt))
    (setq scalpel-llm--tokens-received 0)
    (scalpel-llm--reset-reasoning-buffer)
    (cl-labels
        ((abandon ()
           (setq cancelled t)
           (when timer (cancel-timer timer) (setq timer nil)))
         (arm-timeout ()
           (when timer (cancel-timer timer))
           (setq timer
                 (run-with-timer
                  scalpel-llm-timeout nil
                  (lambda ()
                    (unless cancelled
                      (abandon)
                      (funcall on-error
                               (list :type 'idle
                                     :message
                                     (format "Scalpel: LLM request idle for more than %s seconds"
                                             scalpel-llm-timeout))))))))
         (finish (kind payload)
           (unless cancelled
             (abandon)
             (funcall (if (eq kind 'success) on-success on-error) payload))))
      (condition-case err
          (progn
            (gptel-request prompt
              :system system
              :stream t
              :callback (lambda (resp info)
                          (unless cancelled
                            (arm-timeout)
                            (cond
                             ;; End of streamed response: gptel signals
                             ;; success by calling back with RESPONSE = t.
                             ((eq resp t)
                              (finish 'success accumulated))
                             ;; Failure: gptel calls back with a nil
                             ;; RESPONSE; the human-readable cause is in
                             ;; INFO's :status.
                             ((null resp)
                              (finish 'error
                                      (list :type 'api
                                            :message
                                            (format "Scalpel: LLM returned error: %S"
                                                    (or (plist-get info :status)
                                                        "unknown")))))
                             ;; Content chunk.
                             ((stringp resp)
                              (setq accumulated (concat accumulated resp))
                              (setq scalpel-llm--tokens-received
                                    (+ scalpel-llm--tokens-received
                                       (scalpel-llm--count-tokens resp)))
                              (when scalpel-llm--progress-callback
                                (funcall scalpel-llm--progress-callback)))
                             ;; Reasoning chunk: delivered as the RESPONSE
                             ;; argument (a (reasoning . TEXT) cons).
                             ((and (consp resp) (eq (car resp) 'reasoning))
                              (when (stringp (cdr resp))
                                (scalpel-llm--append-reasoning (cdr resp))
                                (setq scalpel-llm--tokens-received
                                      (+ scalpel-llm--tokens-received
                                         (scalpel-llm--count-tokens (cdr resp))))
                                (when scalpel-llm--progress-callback
                                  (funcall scalpel-llm--progress-callback))))))))
            (arm-timeout))
        (error
         (abandon)
         (if (scalpel-llm--api-key-error-p (error-message-string err))
             (funcall on-error
                      (list :type 'api-key
                            :message
                            (concat "Scalpel: gptel has no valid API key.  "
                                    "Run `M-x scalpel-set-backend' or press `C-c C-b' "
                                    "in the *scalpel* buffer to choose a backend and enter credentials")))
           (funcall on-error
                    (list :type 'setup
                          :message (error-message-string err)))))))))

;;;###autoload
(defun scalpel-llm-select-backend ()
  "Interactively switch the gptel backend used for Scalpel requests.
Delegates to `gptel-menu'."
  (interactive)
  (call-interactively #'gptel-menu))

(provide 'scalpel-llm)

;;; scalpel-llm.el ends here
