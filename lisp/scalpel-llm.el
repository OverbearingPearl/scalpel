;;; scalpel-llm.el --- LLM gateway adapter for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Asynchronous bridge to gptel.
;; A request streams through a callback that accumulates content, collects
;; reasoning chunks, and feeds token counters to the console status line; it
;; settles exactly once, through ON-SUCCESS or ON-ERROR.  A request with no
;; callback activity for `scalpel-llm-timeout' seconds is abandoned by its
;; idle timer, and its callbacks are dropped, so a late response cannot
;; corrupt a later request.

;;; Code:

(require 'scalpel-redact)

(require 'cl-lib)
(require 'gptel)
(require 'gptel-transient)

(defcustom scalpel-llm-timeout 120
  "Seconds of total callback silence before a request is abandoned.
This is an *idle* budget, not a wall-clock deadline: every gptel
callback -- content chunk, reasoning chunk, or completion -- resets the
clock, so a slow but active stream is never killed.  Time spent queued
inside gptel counts as idle, because no callback arrives during it.
Raise this for backends that think before emitting their first chunk.
A continued round re-sends the whole conversation, and a backend may
queue that prompt far longer than a first-round one before the first
token arrives, which is why the default leaves room above the older
30-second value."
  :type 'integer
  :group 'scalpel-llm)

(defcustom scalpel-llm-deadline 1800
  "Seconds from the moment a request is sent until it is abandoned.
It is independent of `scalpel-llm-timeout'.  Unlike the idle
budget, this deadline is never reset by callbacks, so a backend
that keeps streaming heartbeats/reasoning chunks without ever
emitting a terminal callback still ends within this budget.  It
must be larger than `scalpel-llm-timeout' to be useful."
  :type 'integer
  :group 'scalpel-llm)

(defvar scalpel-llm--cancel-current nil
  "Closure that cancels the request currently in flight, or nil when none.
Set by `scalpel-llm-request-async' when a request starts and cleared
at every terminal point, so user-level commands can abort a round
without reaching into a request's private state.")

(defvar scalpel-llm--progress-callback nil
  "Optional zero-arg function called on every streaming callback.
The console binds it to a status-line refresh for the duration of a
round; it is nil when no request is in flight.")

(defvar scalpel-llm--tokens-uploaded 0
  "Approximate number of tokens sent in the current request.")

(defvar scalpel-llm--tokens-received 0
  "Approximate number of tokens received in the current request.")

(defvar scalpel-llm--total-uploaded 0
  "Cumulative estimated upload tokens across all requests.
Never reset per request, so a caller can take a snapshot before a
round and diff it afterwards to price the whole round, however many
LLM requests it contained.")

(defvar scalpel-llm--total-received 0
  "Cumulative estimated download tokens across all requests.
Companion to `scalpel-llm--total-uploaded'.")

(defconst scalpel-llm-reasoning-buffer-name "*scalpel-thinking*"
  "Buffer name for collecting streaming reasoning chunks.")

(defun scalpel-llm--api-key-error-p (message)
  "Return non-nil when MESSAGE indicates gptel needs an API key."
  (string-match-p "gptel-api-key.*is not valid" message))

(defconst scalpel-llm--cjk-ranges
  '((#x2E80 . #x9FFF)   ; CJK radicals, strokes, kana, unified ideographs
    (#xF900 . #xFAFF)   ; CJK compatibility ideographs
    (#x3000 . #x303F)   ; CJK punctuation
    (#xFF00 . #xFF60))  ; fullwidth forms
  "Code-point ranges counted as one token per character.
Modern tokenizers (DeepSeek, GPT, Claude) spend roughly one token
per CJK character, against roughly four ASCII characters per
token, so a single characters-per-token ratio cannot serve both.")

(defconst scalpel-llm--cjk-regexp
  (concat "["
          (mapconcat (lambda (range) (format "%c-%c" (car range) (cdr range)))
                     scalpel-llm--cjk-ranges "")
          "]")
  "Precompiled single-pass scanner form of `scalpel-llm--cjk-ranges'.
Built programmatically with `mapconcat' and (format \"%c-%c\" ...)
rather than pasting literal multibyte characters, so it can never
drift out of sync with `scalpel-llm--cjk-ranges' and no non-ASCII
literal can be lost or altered by editors or encodings in transit.
Encodes the exact same code-point ranges as one character
alternative, so `scalpel-llm--count-tokens' can count CJK
characters per chunk with a single C-level regexp scan on the
streaming hot path instead of an Elisp loop over every character
times every range.

The string contains only a character-alternative expression: no
grouping constructs and no literal parens, so no
parenthesis-escaping concerns arise; it matches single characters
only, so greedy/non-greedy semantics are irrelevant, and the dot
metacharacter and newline handling play no role in its behavior.")

(defun scalpel-llm--cjk-char-p (char)
  "Return non-nil when CHAR falls in a CJK code-point range."
  (cl-some (lambda (range)
             (and (>= char (car range)) (<= char (cdr range))))
           scalpel-llm--cjk-ranges))

(defun scalpel-llm--count-tokens (text)
  "Return an approximate token count for TEXT.
ASCII and other non-CJK text uses the 4-characters-per-token
heuristic; CJK characters are counted at one token per character,
which is what modern tokenizers actually spend on them.  The
previous single ratio priced Chinese text at a quarter of its real
cost, so the status line and the token accounting under-reported
every CJK-heavy prompt.  CJK detection uses `scalpel-llm--cjk-regexp', a
regexp precompiled once, so per-chunk token counting stays cheap on
the streaming hot path; token numbers are unchanged."
  (let ((cjk 0)
        (pos 0))
    (while (and (<= pos (length text))
                (string-match scalpel-llm--cjk-regexp text pos))
      (setq cjk (1+ cjk)
            pos (match-beginning 0))
      (setq pos (1+ pos)))
    (+ cjk (ceiling (/ (- (length text) cjk) 4.0)))))

(defun scalpel-llm--reset-reasoning-buffer (&optional buffer-name)
  "Clear the reasoning buffer for a new request.
BUFFER-NAME names the buffer to clear, defaulting to
`scalpel-llm-reasoning-buffer-name'.  Because the caller names the
buffer, concurrent consoles each clear their own reasoning buffer."
  (with-current-buffer (get-buffer-create (or buffer-name scalpel-llm-reasoning-buffer-name))
    (erase-buffer)))

(defun scalpel-llm--reasoning-buffer-name (buffer)
  "Return the reasoning buffer name to use for BUFFER.
When BUFFER is a console buffer (its buffer-local `scalpel-console--root'
is non-nil), reuse the per-console reasoning buffer derived from the
console's name, so each console has exactly one reasoning buffer.  When
BUFFER is not associated with a console, return the base name
`scalpel-llm-reasoning-buffer-name' unchanged, keeping behavior identical
for tests and other non-console callers."
  (if (buffer-local-value 'scalpel-console--root buffer)
      (format "*scalpel-thinking<%s>*" (buffer-name buffer))
    scalpel-llm-reasoning-buffer-name))

(defun scalpel-llm--append-reasoning (chunk &optional buffer-name)
  "Append CHUNK to the reasoning buffer named BUFFER-NAME.
BUFFER-NAME defaults to `scalpel-llm-reasoning-buffer-name'.
Follow the end of the buffer only when point is already there, so
that scrolling back through earlier output is not interrupted by
new chunks."
  (with-current-buffer (get-buffer-create (or buffer-name
                                              scalpel-llm-reasoning-buffer-name))
    (let ((follow (or (null (get-buffer-window (current-buffer) t))
                      (eobp))))
      (if follow
          (progn
            (goto-char (point-max))
            (insert chunk))
        (save-excursion
          (goto-char (point-max))
          (insert chunk))))))

(cl-defun scalpel-llm-request-async (prompt on-success on-error &optional system)
  "Send PROMPT to the configured gptel backend without blocking.
Return immediately.  ON-SUCCESS is called with the accumulated
response string once the stream ends.  ON-ERROR is called with a
plist (:type SYMBOL :message STRING) when the request fails, stays
idle for `scalpel-llm-timeout' seconds without any callback,
exceeds `scalpel-llm-deadline' seconds total, is cancelled, or
cannot be set up.  SYSTEM overrides the default system message.

The request is abandoned by its idle timer, its total deadline,
an explicit cancellation, or when the stream ends, and each path
sets the request's `cancelled' flag.  A callback that arrives
after the flag is set is dropped, so a late response cannot corrupt
a later request.

:type is `idle' for the idle timer, `timeout' for the total
deadline, `cancelled' for an explicit cancellation, `api' for a
backend error surfaced by gptel, `api-key' for a missing or invalid
API key, `setup' for a failure raised synchronously by
`gptel-request', and `callback' for a success or error callback that
raised.  ON-ERROR is the request's terminal callback; a success
callback which raises is re-delivered through ON-ERROR with :type
`callback', and an error raised by ON-ERROR itself is only reported
in the echo area rather than escaping into gptel's process filter.

The cancel closure is stored buffer-locally in the buffer that
started the request, so a concurrent request from another console
neither overwrites nor cancels it, and the variable no longer holds
only the most recent request globally.

The progress callback is captured once at request start into a
request-local binding, so a later request cannot redirect or
freeze an earlier request's status updates; likewise the
per-request download counter is local to this request and is the
authoritative tally for the stream, with the global
`scalpel-llm--tokens-received' mirrored from it after every chunk
so existing readers keep working, and the cumulative
`scalpel-llm--total-received' incremented by each chunk's token
count as it arrives, so the console's cumulative down count
advances; it is never zeroed or reset at request start.  The
per-request mirror is informational
only and shows the most recent request's tally.  The global
`scalpel-llm--tokens-received' mirrors this request's tally for
readers outside the request; it is no longer reset at request
start, so a concurrent request cannot zero an earlier one's count."
  (message "Scalpel-debug: llm request start")
  (let* ((cancelled nil)
         (accumulated "")
         (timer nil)
         (deadline-timer nil)
         (cancel-fn nil)
         ;; The buffer this request belongs to; the cancel closure is
         ;; installed as a buffer-local value here so two consoles with
         ;; concurrent requests cannot cancel each other's request.
         (owner (current-buffer))
         ;; Per-console naming keeps concurrent requests' reasoning
         ;; streams separate; each console reuses its own buffer, so no
         ;; bookkeeping or cleanup growth.
         (reasoning-buffer (scalpel-llm--reasoning-buffer-name owner))
         ;; Per-request streaming state: a fresh download counter and a
         ;; snapshot of the progress callback taken now, so two
         ;; concurrent requests each keep their own tallies and their
         ;; own status-line updates.
         (received 0)
         (progress scalpel-llm--progress-callback))
    (setq scalpel-llm--tokens-uploaded (scalpel-llm--count-tokens prompt))
    (setq scalpel-llm--total-uploaded
          (+ scalpel-llm--total-uploaded scalpel-llm--tokens-uploaded))
    (scalpel-llm--reset-reasoning-buffer reasoning-buffer)
    (cl-labels
        ((clear-cancel-current ()
           (when (eq (buffer-local-value 'scalpel-llm--cancel-current owner) cancel-fn)
             (with-current-buffer owner
               (setq scalpel-llm--cancel-current nil))))
         (notify-error (payload)
           (message "Scalpel-debug: llm notify-error %S" payload)
           ;; ON-ERROR runs after `abandon' has cancelled the idle timer
           ;; and the total deadline, so an error raised inside it can no
           ;; longer be rescued by either timer and would escape into
           ;; gptel's process filter; this sink swallows it into the echo
           ;; area instead.
           (condition-case err
               (funcall on-error payload)
             (error
              (message "Scalpel: error callback failed: %s (%S)"
                       (error-message-string err)
                       err))))
         (abandon ()
           (setq cancelled t)
           (when timer (cancel-timer timer) (setq timer nil))
           (when deadline-timer (cancel-timer deadline-timer) (setq deadline-timer nil))
           (clear-cancel-current))
         (arm-timeout ()
           (when timer (cancel-timer timer))
           (setq timer
                 (run-with-timer
                  scalpel-llm-timeout nil
                  (lambda ()
                    (unless cancelled
                      (abandon)
                      (notify-error
                       (list :type 'idle
                             :message
                             (format "Scalpel: LLM request idle for more than %s seconds"
                                     scalpel-llm-timeout))))))))
         (arm-deadline ()
           (when deadline-timer (cancel-timer deadline-timer))
           (setq deadline-timer
                 (run-with-timer
                  scalpel-llm-deadline nil
                  (lambda ()
                    (unless cancelled
                      (abandon)
                      (notify-error
                       (list :type 'timeout
                             :message "Scalpel: request exceeded its total time budget")))))))
         (finish (kind payload)
           (unless cancelled
             (abandon)
             (message "Scalpel-debug: llm finish kind=%s" kind)
             (if (eq kind 'success)
                 (condition-case err
                     ;; Restore redaction placeholders (e.g. /Users/madachuan)
                     ;; back to real values on the whole raw reply before
                     ;; dialect parse, so every downstream consumer sees
                     ;; real paths.
                     (funcall on-success (scalpel-redact-restore payload))
                   (error
                    (notify-error
                     (list :type 'callback
                           :message (error-message-string err)))))
               (notify-error payload))))
         (cancel-request ()
           (unless cancelled
             (abandon)
             (notify-error
              (list :type 'cancelled
                    :message "Scalpel: request cancelled")))))
      (setq cancel-fn (lambda () (cancel-request)))
      (with-current-buffer owner
        (set (make-local-variable 'scalpel-llm--cancel-current) cancel-fn))
      ;; Preflight: with `gptel-backend' nil, `gptel-request' dispatches
      ;; into gptel's default path that neither raises synchronously nor
      ;; calls back, and the round dies on the idle timer with a
      ;; misleading "idle" error.  A configured backend whose key is
      ;; missing still raises synchronously inside `gptel-request', and
      ;; that path reports `api-key' already; only the nil case needs
      ;; this guard.
      (unless (and (boundp 'gptel-backend) gptel-backend)
        (abandon)
        (notify-error
         (list :type 'api-key
               :message
               (concat "Scalpel: gptel backend is not configured or has no "
                       "API key.  Run `M-x scalpel-set-backend' or press "
                       "`C-c C-b' in the *scalpel* buffer to choose a "
                       "backend and enter credentials")))
        (cl-return-from scalpel-llm-request-async))
      (condition-case err
          (progn
            (arm-deadline)
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
                              (let ((status (plist-get info :status)))
                                (if (and status (not (string-match-p "200" (format "%s" status))))
                                    (finish 'error
                                            (list :type 'api
                                                  :message
                                                  (format "Scalpel: LLM returned error: %S"
                                                          (or status "unknown"))))
                                  ;; Some gptel versions/backends call back with
                                  ;; nil RESPONSE and a 200 status as a normal
                                  ;; end-of-stream signal. Treat this as success.
                                  (finish 'success accumulated))))
                             ;; Content chunk.
                             ((stringp resp)
                              (setq accumulated (concat accumulated resp))
                              (setq received
                                    (+ received
                                       (scalpel-llm--count-tokens resp)))
                              (setq scalpel-llm--total-received
                                    (+ scalpel-llm--total-received
                                       (scalpel-llm--count-tokens resp)))
                              (setq scalpel-llm--tokens-received received)
                              (when progress
                                (funcall progress)))
                             ;; Reasoning chunk: delivered as the RESPONSE
                             ;; argument (a (reasoning . TEXT) cons).
                             ((and (consp resp) (eq (car resp) 'reasoning))
                              (when (stringp (cdr resp))
                                (scalpel-llm--append-reasoning (cdr resp) reasoning-buffer)
                                (setq received
                                      (+ received
                                         (scalpel-llm--count-tokens (cdr resp))))
                                (setq scalpel-llm--total-received
                                      (+ scalpel-llm--total-received
                                         (scalpel-llm--count-tokens (cdr resp))))
                                (setq scalpel-llm--tokens-received received)
                                (when progress
                                  (funcall progress))))))))
            (unless cancelled
              (arm-timeout)))
        (error
         (abandon)
         (let ((payload
                (if (scalpel-llm--api-key-error-p (error-message-string err))
                    (list :type 'api-key
                          :message
                          (concat "Scalpel: gptel has no valid API key.  "
                                  "Run `M-x scalpel-set-backend' or press `C-c C-b' "
                                  "in the *scalpel* buffer to choose a backend and enter credentials"))
                  (list :type 'setup
                        :message (error-message-string err)))))
           (notify-error payload)))))))

;;;###autoload
(defun scalpel-llm-select-backend ()
  "Interactively switch the gptel backend used for Scalpel requests.
Delegates to `gptel-menu'."
  (interactive)
  (call-interactively #'gptel-menu))

(provide 'scalpel-llm)

;;; scalpel-llm.el ends here
