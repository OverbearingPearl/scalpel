;;; scalpel-llm.el --- LLM gateway adapter for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
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

(defconst scalpel-llm-timeout 60
  "Seconds of total callback silence before a request is abandoned.
This is an *idle* budget, not a wall-clock deadline: every gptel
callback -- content chunk, reasoning chunk, or completion -- resets the
clock, so a slow but active stream is never killed.  Time spent queued
inside gptel counts as idle, because no callback arrives during it.
Raise this for backends that think before emitting their first chunk.
A continued round re-sends the whole conversation, and a backend may
queue that prompt far longer than a first-round one before the first
token arrives, which is why the default leaves room above the older
30-second value.")

(defconst scalpel-llm-deadline 900
  "Seconds from the moment a request is sent until it is abandoned.
It is independent of `scalpel-llm-timeout'.  Unlike the idle
budget, this deadline is never reset by callbacks, so a backend
that keeps streaming heartbeats/reasoning chunks without ever
emitting a terminal callback still ends within this budget.  It
must be larger than `scalpel-llm-timeout' to be useful.")

(defconst scalpel-llm-max-reply-bytes 65536
  "Hard byte cap on the accumulated reply of a single request.
When the streamed reply exceeds this many bytes, the request
finishes immediately with an oversized error instead of waiting
for the stream to end naturally.

This budget truncates by byte count only and never judges the
syntactic completeness of the reply; the truncated reply reaches
on-error with the oversized type, and the diagnose layer's retry
advice then tells the model to shorten output or split the edit.")

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
exceeds `scalpel-llm-deadline' seconds total, is cancelled, exceeds
`scalpel-llm-max-reply-bytes' accumulated reply bytes, or
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
`gptel-request', `oversized' for a reply whose accumulated byte
length exceeded `scalpel-llm-max-reply-bytes', and `callback' for a
success or error callback that
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
start, so a concurrent request cannot zero an earlier one's count.

The content-chunk branch enforces a hard byte budget: once the
accumulated reply exceeds `scalpel-llm-max-reply-bytes', the
request is finished with an `oversized' error naming the
accumulated byte count and the limit, so a runaway stream is cut
off quickly and diagnosably while normal replies are unaffected."
  (scalpel-llm-probe-reasoning-lazy)
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
             (if (eq kind 'success)
                 (condition-case err
                     ;; Restore redaction placeholders (e.g. /Users/madachuan)
                     ;; back to real values on the whole raw reply before
                     ;; dialect parse, so every downstream consumer sees
                     ;; real paths.
                     (with-current-buffer owner
                       (funcall on-success (scalpel-redact-restore payload)))
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
                                (funcall progress))
                              ;; Hard byte budget: a runaway reply is cut
                              ;; off immediately as a diagnosable failure
                              ;; instead of growing without bound; the
                              ;; check runs after the counters so this
                              ;; request's tally still includes the chunk
                              ;; that crossed the limit.
                              (when (and (numberp scalpel-llm-max-reply-bytes)
                                         (> scalpel-llm-max-reply-bytes 0)
                                         (> (length accumulated)
                                            scalpel-llm-max-reply-bytes))
                                (finish 'error
                                        (list :type 'oversized
                                              :message
                                              (format "Scalpel: reply exceeded budget; truncated at %d bytes (limit %d bytes, scalpel-llm-max-reply-bytes)"
                                                      (length accumulated)
                                                      scalpel-llm-max-reply-bytes)))))
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
Delegates to `gptel-menu'.
Reasoning support probing is lazy: the minimal reasoning parameters
are applied just before the next Scalpel request."
  (interactive)
  (call-interactively #'gptel-menu))

(defun scalpel-llm--reasoning-content-length (resp)
  "Three-state measure of reasoning in parsed chat-completions RESP.
nil when the structure is broken; 0 when the message exists but
carries no reasoning field, meaning thinking was fully suppressed;
the reasoning length otherwise."
  (let ((choices (plist-get resp :choices)))
    (cond
     ((not choices)
      (let ((text (format "%S" resp)))
        (message "Scalpel: reasoning probe: response has no choices: %s"
                 (substring text 0 (min 500 (length text))))
        nil))
     ((not (plist-get (car choices) :message))
      (let ((text (format "%S" (car choices))))
        (message "Scalpel: reasoning probe: choice has no message: %s"
                 (substring text 0 (min 500 (length text))))
        nil))
     (t
      (let* ((msg (plist-get (car choices) :message))
             (r (or (plist-get msg :reasoning_content)
                    (plist-get msg :reasoning))))
        (if (stringp r) (length r) 0))))))

(defun scalpel-llm--probe-key (backend)
  "Return the API key for gptel BACKEND as a string, or nil when absent."
  (let ((key (condition-case nil (gptel-backend-key backend) (error nil))))
    (when (functionp key)
      (setq key (condition-case nil (funcall key) (error nil))))
    (and (stringp key) (> (length key) 0) key)))

(defun scalpel-llm--probe-send (params)
  "Send one synchronous chat-completions probe adding PARAMS to the body.
PARAMS is a plist of extra body parameters; nil sends the baseline
body.  The request body is built as one plist -- the shape gptel
documents for :request-params -- so the candidate probed here is
exactly the value later installed on the model symbol.  Return the
parsed JSON response as a plist, or nil on any failure."
  (condition-case err
      (let* ((backend gptel-backend)
             (url (and backend (condition-case nil (gptel-backend-url backend) (error nil))))
             (key (and backend (scalpel-llm--probe-key backend)))
             (model (and (boundp 'gptel-model) gptel-model
                         (condition-case nil (gptel--model-name gptel-model) (error nil))))
             (bname (and backend (condition-case nil (gptel-backend-name backend) (error nil)))))
        (if (not (and url key model))
            (progn
              (message "Scalpel: reasoning probe: backend, url, key or model missing")
              nil)
          (let* ((endpoint
                  (if (string-suffix-p "/chat/completions" url)
                      url
                    (concat (string-trim-right url "/") "/chat/completions")))
                 (url-request-method "POST")
                 (url-request-extra-headers
                  `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " key))))
                 (url-request-data
                  (json-encode
                   (append (list :model model
                                 :messages [((role . "user")
                                             (content . "Solve step by step and show all work: what is 37*43 plus 91*17? Verify each partial product."))]
                                 :stream :json-false)
                           params))))
            (message "Scalpel: reasoning probe: POST %s (%s / %s)" endpoint bname model)
            (let ((buf (url-retrieve-synchronously endpoint t nil 45)))
              (if (not buf)
                  (progn
                    (message "Scalpel: reasoning probe: no response buffer from %s (%s / %s)" endpoint bname model)
                    nil)
                (with-current-buffer buf
                  (goto-char (point-min))
                  (message "Scalpel: reasoning probe: HTTP status: %s (%s / %s)"
                           (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))
                           bname model)
                  (if (not (re-search-forward "\r?\n\r?\n" nil t))
                      (progn
                        (message "Scalpel: reasoning probe: no HTTP body from %s (%s / %s)" endpoint bname model)
                        nil)
                    (condition-case perr
                        (let* ((raw (buffer-substring-no-properties
                                     (point) (point-max)))
                               ;; The url library may already have decoded
                               ;; the response body as multibyte text; encode
                               ;; multibyte strings back to bytes with
                               ;; utf-8-unix (characters not representable
                               ;; in latin-1 still round-trip correctly),
                               ;; use raw single-byte strings as-is, then
                               ;; decode everything as UTF-8.
                               (bytes (if (multibyte-string-p raw)
                                          (encode-coding-string raw 'utf-8-unix)
                                        raw))
                               (text (decode-coding-string bytes 'utf-8))
                               (json-object-type 'plist)
                               (json-array-type 'list))
                          (json-read-from-string text))
                      (error
                       (message "Scalpel: reasoning probe: JSON parse failed: %s"
                                (error-message-string perr))
                       nil)))))))))
    (error
     (message "Scalpel: reasoning probe: request failed: %s"
              (error-message-string err))
     nil)))

(defun scalpel-llm-probe-reasoning ()
  "Probe which parameter groups best suppress the model thinking, install it.
Each candidate is a plist of extra body parameters, matching the plist
shape gptel documents for :request-params, and the winner is installed
with that same shape.  Installation takes over :request-params
entirely: any existing value, including request parameters configured
by the user outside Scalpel, is replaced.  When no candidate wins, or
the model does not think by default, :request-params is set to an
empty plist instead of removed, so the request body uses exactly the
endpoint defaults and nothing else.  Synchronous and uncached;
verdicts for all candidates are collected and printed as one summary
block.

Each candidate combines a reasoning parameter with a full set of
sampling parameters (:temperature 0.0, :presence_penalty 0,
:frequency_penalty 0), so one candidate request validates the whole
group at once; the total request count stays at 1 baseline + N
candidates.  The candidate list starts with reasoning_effort \"none\"
— aiming to switch thinking off entirely — and is ordered toward the
goal of thinking as little as possible.  When a combined candidate is
REJECTED-400, one fallback probe with only the reasoning parameters
of that candidate is sent (at most one fallback per candidate, only
on the failure path).  If the fallback is accepted while the combined
candidate was rejected, the backend does not support the sampling
parameters; both verdicts are recorded separately, and only in that
case may the pure reasoning plist become the installed best."
  (interactive)
  (let ((model gptel-model)
        (baseline (scalpel-llm--probe-send nil)))
    (cond
     ((null baseline)
      (message "Scalpel: reasoning probe: baseline request failed; aborting"))
     (t
      (let ((baseline-len (scalpel-llm--reasoning-content-length baseline)))
        (cond
         ((null baseline-len)
          (message "Scalpel: reasoning probe: baseline response unreadable; aborting"))
         ((zerop baseline-len)
          (put model :request-params nil)
          (message "Scalpel: reasoning probe: model does not think by default; set :request-params to empty plist on %S" model))
         (t
          (message "Scalpel: reasoning probe: baseline reasoning length %d" baseline-len)
          (let ((best nil) (best-len baseline-len) verdicts)
            ;; Each candidate is one whole plist of extra body
            ;; parameters, appended as a unit by
            ;; `scalpel-llm--probe-send'; a flat plist here would be
            ;; iterated atom by atom and corrupt the request body.
            ;; Each candidate appends the full sampling set on top of
            ;; one reasoning parameter, so a single request validates
            ;; the whole group.
            (dolist (c '((:reasoning_effort "none"
                           :temperature 0.0 :presence_penalty 0 :frequency_penalty 0)
                         (:reasoning_effort "low"
                           :temperature 0.0 :presence_penalty 0 :frequency_penalty 0)
                         (:enable_thinking :json-false
                          :temperature 0.0 :presence_penalty 0 :frequency_penalty 0)
                         (:thinking (:type "disabled")
                          :temperature 0.0 :presence_penalty 0 :frequency_penalty 0)))
              (let* ((reasoning-only
                      (let ((k (car c)) (v (cadr c)))
                        (if (and (eq v :json-false))
                            (list k v)
                          (list k v))))
                     (resp (scalpel-llm--probe-send c)))
                (cond
                 ((null resp)
                  (push (format "%S FAILED" c) verdicts))
                 ((plist-get resp :error)
                  (let ((err (plist-get resp :error)))
                    ;; Combined candidate rejected: attribute the
                    ;; failure with one fallback probe of the pure
                    ;; reasoning parameters only.
                    (let ((fb (scalpel-llm--probe-send reasoning-only)))
                      (cond
                       ((null fb)
                        (push (format "%S REJECTED-400 %s; fallback %S FAILED"
                                      c
                                      (decode-coding-string
                                       (or (plist-get err :message)
                                           (prin1-to-string err))
                                       'utf-8)
                                      reasoning-only)
                              verdicts))
                       ((plist-get fb :error)
                        (let ((ferr (plist-get fb :error)))
                          (push (format "%S REJECTED-400 %s; fallback %S REJECTED-400 %s"
                                        c
                                        (decode-coding-string
                                         (or (plist-get err :message)
                                             (prin1-to-string err))
                                         'utf-8)
                                        reasoning-only
                                        (decode-coding-string
                                         (or (plist-get ferr :message)
                                             (prin1-to-string ferr))
                                         'utf-8))
                                verdicts)))
                       (t
                        (let ((flen (scalpel-llm--reasoning-content-length fb)))
                          (cond
                           ((null flen)
                            (push (format "%S REJECTED-400 %s; fallback %S UNREADABLE"
                                          c
                                          (decode-coding-string
                                           (or (plist-get err :message)
                                               (prin1-to-string err))
                                           'utf-8)
                                          reasoning-only)
                                  verdicts))
                           ((< flen best-len)
                            (push (format "%S REJECTED-400 %s; fallback %S ACCEPTED %d -> %d (sampling params unsupported)"
                                          c
                                          (decode-coding-string
                                           (or (plist-get err :message)
                                               (prin1-to-string err))
                                           'utf-8)
                                          reasoning-only
                                          baseline-len flen)
                                  verdicts)
                            (setq best reasoning-only best-len flen))
                           (t
                            (push (format "%S REJECTED-400 %s; fallback %S NO-BETTER (%d >= %d)"
                                          c
                                          (decode-coding-string
                                           (or (plist-get err :message)
                                               (prin1-to-string err))
                                           'utf-8)
                                          reasoning-only
                                          flen baseline-len)
                                  verdicts)))))))))
                 (t
                  (let ((len (scalpel-llm--reasoning-content-length resp)))
                    (cond
                     ((null len)
                      (push (format "%S UNREADABLE" c) verdicts))
                     ((< len best-len)
                      (push (format "%S ACCEPTED %d -> %d" c baseline-len len) verdicts)
                      (setq best c best-len len))
                     (t
                      (push (format "%S NO-BETTER (%d >= %d)" c len baseline-len)
                            verdicts))))))))
            (setq verdicts (nreverse verdicts))
            (message "Scalpel: reasoning probe verdicts:\n%s"
                     (mapconcat #'identity verdicts "\n"))
            (if best
                (progn
                  (put model :request-params best)
                  (message "Scalpel: reasoning probe: installed %S on %S (reasoning %d -> %d)" best model baseline-len best-len))
              (progn
                (put model :request-params nil)
                (message "Scalpel: reasoning probe: no effective candidate; set :request-params to empty plist on %S" model)))))))))))

(defvar scalpel-llm-probe-inhibit nil
  "Non-nil means skip automatic reasoning probes.
Probes are normally triggered by backend switches or console
opening; inhibiting them keeps test runs and batch reloads
side-effect free.")

(defvar-local scalpel-llm-allow-thinking nil
  "Buffer-local flag letting one LLM round keep default thinking behavior.
It is set and reset by the console round layer, which uses it to opt a
single round out of the thinking suppression parameters.

Non-nil means requests issued with this buffer as the current context are
sent without the thinking suppression parameters, leaving the model's
default thinking in effect; nil means the suppression parameters are
installed as usual.  The console round layer sets this to t in the
session buffer when a round is started with the tilde prefix and resets
it to nil when the round ends, success or failure, so two concurrent
consoles each hold an independent flag and never share a global thinking
window.")

(defun scalpel-llm-probe-reasoning-lazy ()
  "Lazy pre-request probe entry point.
Runs a one-shot reasoning probe for the current gptel backend/model
pair unless it has already been probed.  Does nothing when
`scalpel-llm-probe-inhibit' is non-nil, or when the gptel backend or
model is unset.  A probe failure is logged and swallowed so the
request path is never broken; the failed pair is not cached, so the
probe retries on the next request."
  (when (and (not scalpel-llm-probe-inhibit)
             gptel-backend
             gptel-model)
    (let ((key (cons (gptel-backend-name gptel-backend)
                     (gptel--model-name gptel-model))))
      (unless (gethash key scalpel-llm-probe-reasoning-cache)
        ;; Degradation point: probe errors must not break the request path,
        ;; so they are logged loudly and swallowed; failed pairs stay
        ;; uncached and are retried on the next request.  The probed
        ;; model object itself is cached so the strip loop can find the
        ;; real model symbol to remove :request-params from.
        (condition-case err
            (progn
              (scalpel-llm-probe-reasoning)
              (puthash key gptel-model scalpel-llm-probe-reasoning-cache))
          (error
           (message "scalpel-llm-probe: reasoning probe failed for %S: %s"
                    key (error-message-string err))))))))

(defconst scalpel-llm-probe-reasoning-cache (make-hash-table :test #'equal)
  "Per-session cache of reasoning probes.
Keys are (backend-name . model-name) conses; values are the model
symbols that were probed.  The toggle consults this table to strip
:request-params from every probed model.")

(defconst scalpel-llm-probe-retrieval-cache (make-hash-table :test #'equal)
  "Cache of probed server-side retrieval parameters per backend/model.
Key is a cons cell (BACKEND-NAME . MODEL-NAME); value is the plist of
pure server-side retrieval parameters probed for that combination.
Probing is not yet implemented, so this cache starts out empty.")

(cl-defun scalpel-llm-probe-retrieval-params ()
  "Return the cached retrieval plist for the current gptel backend/model.
Serves as the source of retrieval parameters when restoring deep-reasoning
turns.  Builds the cache key from `gptel-backend' and `gptel-model' and looks
it up in `scalpel-llm-probe-retrieval-cache'.  Returns nil when the backend
or model is unset, or on cache miss; since probing is not yet implemented
the cache is empty, so nil is the safe degraded outcome."
  (when (and gptel-backend gptel-model)
    (gethash (cons (gptel-backend-name gptel-backend) gptel-model)
             scalpel-llm-probe-retrieval-cache)))

(defvar scalpel-llm-probe-retrieval-cache nil
  "Alist of (backend-name . model) to winning retrieval probe plist.")

(defun scalpel-llm--retrieval-markers-p (resp)
  "Return non-nil when RESP carries retrieval annotations of any kind."
  (let ((text (prin1-to-string resp))
        (hit nil))
    (dolist (marker '("groundingMetadata"
                      "url_citation"
                      "annotations"
                      "citations")
                   hit)
      (when (string-match-p (regexp-quote marker) text)
        (setq hit t)))))

(cl-defun scalpel-llm-probe-retrieval ()
  "Synchronously probe retrieval params for the current backend/model.

Sends a baseline request, then one request per candidate plist,
reporting which candidate (if any) elicits retrieval annotations in
the response.  Winning candidates are recorded in
`scalpel-llm-probe-retrieval-cache'; the cache is never read here, and
the regular request path is untouched."
  (interactive)
  (let ((backend gptel-backend)
        (model gptel-model))
    (when (or (null backend) (null model))
      (message "Scalpel: retrieval probe: backend or model not available; aborting.")
      (cl-return-from scalpel-llm-probe-retrieval))
    (let* ((backend-name (gptel-backend-name backend))
           (model-name (gptel--model-name model))
           (candidates
            (if (string-match-p "openai" backend-name)
                (list '(:web_search_options (:search_context_size "low")))
              nil)))
      (when (null candidates)
        (message
         "Scalpel: retrieval probe: only OpenAI-compatible chat-completions endpoints are supported by probe-send; Anthropic /messages and native Gemini endpoints cannot be probed; aborting.")
        (cl-return-from scalpel-llm-probe-retrieval))
      ;; Baseline request.
      (let ((baseline
             (condition-case err
                 (scalpel-llm--probe-send nil)
               (error
                (message "Scalpel: retrieval probe: baseline request failed: %s"
                         (decode-coding-string
                          (prin1-to-string (cdr err)) 'utf-8))
                nil))))
        (when baseline
          (if (scalpel-llm--retrieval-markers-p baseline)
              (message
               "Scalpel: retrieval probe: baseline response already contains retrieval annotations; no extra params needed.")
            (let ((winner nil))
              (dolist (candidate candidates)
                (if winner
                    nil
                  (let ((resp
                         (condition-case err
                             (scalpel-llm--probe-send candidate)
                           (error
                            (message "Scalpel: retrieval probe: candidate %S failed: %s"
                                     candidate
                                     (decode-coding-string
                                      (prin1-to-string (cdr err)) 'utf-8))
                            nil))))
                    (cond
                     ((null resp)
                      (message "Scalpel: retrieval probe: candidate %S FAILED." candidate))
                     ((plist-get resp :error)
                      (message "Scalpel: retrieval probe: candidate %S REJECTED (%s)."
                               candidate (plist-get resp :error)))
                     ((scalpel-llm--retrieval-markers-p resp)
                      (setq winner candidate)
                      (message "Scalpel: retrieval probe: candidate %S HIT retrieval annotations." candidate))
                     (t
                      (message "Scalpel: retrieval probe: candidate %S accepted but produced no annotations." candidate))))))
              (if winner
                  (progn
                    (puthash (cons backend-name model-name)
                             winner
                             scalpel-llm-probe-retrieval-cache)
                    (message
                     "Scalpel: retrieval probe: cached winner %S for backend %S model %S."
                     winner backend-name model-name))
                (message
                 "Scalpel: retrieval probe: no candidate produced retrieval annotations; nothing cached, request path unaffected.")))))))))

(provide 'scalpel-llm)

;;; scalpel-llm.el ends here
