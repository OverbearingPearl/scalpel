;;; scalpel-console.el --- Persistent interactive agent console for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Provides the *scalpel* buffer where users type a textual instruction and
;; press RET to send it to the agent.  S-RET inserts a newline, so an
;; instruction may span several lines and is still sent as one message.  The
;; buffer is a normal editable text buffer, so users can review prior turns.
;;
;; The console is also where the accompanying policy lives: edits take effect as
;; the plan is dispatched, with no per-hunk approval, and the only shell prompt
;; is for a command the planner flagged long-running -- that one asks because
;; Emacs is frozen until the command returns.  A round whose shell output is
;; small flows straight back to the planner; only a noisy round stops to ask.
;;
;; Because nothing is approved beforehand, the buffer is also the record: every
;; edit report and shell report stays in it, so a session can be read back, and
;; later diffed, after the fact.  What the agent is sent is a projection of that
;; record: a report's output body is spent once the following round has read it,
;; so only the newest report carries its body into the prompt, while the buffer
;; keeps every byte.
;;
;; A report's body is also folded in the display by default: a shell command may
;; dump thousands of lines, and the header -- what ran, how it exited, how large
;; the output was -- is what a reader needs at a glance.  The fold is display
;; only, so the record and the projection above are both untouched.
;;
;; The projection is visible too: a report whose body the planner no longer
;; reads is marked on its header lines, in the display only, so the trimming is
;; not silent.

;;; Code:

(require 'cl-lib)
(require 'scalpel-agent)

(defcustom scalpel-console-buffer-name-format "*scalpel: %s*"
  "Format string for the Scalpel console buffer name.
The single `%s' is substituted with the abbreviated console root,
so a console opened at /home/me/proj is named \"*scalpel: ~/proj*\"."
  :type 'string
  :group 'scalpel)

(defcustom scalpel-console-max-rounds 30
  "Maximum agent rounds one instruction may trigger.
A round is one request/execute cycle.  A round that ran shell
commands may be followed by another so the agent can act on their
output; this caps the chain, because a command the agent cannot fix
would otherwise loop forever.  The continuation question is not
asked once this limit is reached, and reaching it is reported in
the console buffer."
  :type 'integer
  :group 'scalpel)

(defcustom scalpel-console-continue-after-shell 'ask
  "Whether a round that ran shell commands is followed by another.
A round whose command output is small continues silently: that
output is what the planner needs to finish the work the user asked
for, so stopping to ask costs a keystroke and buys nothing.  A
round whose output is noisy \(see
`scalpel-console-continue-after-shell-max-bytes') is put to the
user, who is shown every command together with its output size.
`ask' questions only that noisy round; `always' continues without
asking even then; nil never continues, and never asks.  Batch runs
never continue regardless of this value."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask when output is large" ask)
                 (const :tag "Always continue" always))
  :group 'scalpel)

(defcustom scalpel-console-continue-after-shell-max-bytes 4096
  "Shell output above this size makes a round worth questioning.
When a round ran a shell command whose raw output exceeded this
many bytes -- or that was truncated or binary -- the round stops
for a question under `scalpel-console-continue-after-shell' set to
`ask'; under `always' it continues regardless.  Raise this to let
more output flow back unquestioned."
  :type 'integer
  :group 'scalpel)

(defcustom scalpel-console-trim-consumed-output t
  "Whether spent report bodies are dropped from what the agent reads.
The console buffer always keeps every report in full; this controls
only the text `scalpel-console--history' returns.  A round's output
is read back once, by the round that follows it, so every later
prompt would carry the same body for nothing, and a long session
re-sends it on every turn.  With this set, only the newest report
keeps its body; older ones keep their header lines and a
placeholder.  Set it to nil to send every report whole: that costs
the repeated growth, but never asks the planner to re-run a command
for output it was already given.  Whichever way it is set, the console
marks every report whose body it drops, on the display only, so the
trimming is never silent."
  :type 'boolean
  :group 'scalpel)

(defcustom scalpel-console-collapse-output t
  "Whether a report's fenced output body is folded in the display.
A shell or read report may hold thousands of lines that the user
does not have to read: its output already goes back to the planner
by itself, and the header -- what ran, how it exited, how large
the output was -- is what a reader needs at a glance.  With this
set, the body between a report's fences is hidden behind a one-line
placeholder; the text stays in the buffer, so
`scalpel-console--history' and `scalpel-console--trim-report' see
it unchanged, and `scalpel-console-toggle-output' expands it again.
Set it to nil to show every body."
  :type 'boolean
  :group 'scalpel)

(defconst scalpel-console--consumed-output-marker "[output consumed]"
  "Placeholder left where a consumed report body was trimmed.
Structural contract shared by `scalpel-console--trim-report' and
`scalpel-console-trim-consumed-output'.")

(defconst scalpel-console--consumed-body-note
  "Output already read back to the planner; no longer re-sent.  C-c C-o shows the text."
  "Tooltip for a report the planner no longer reads in full.
Structural contract shared by `scalpel-console--history', which
drops such a body from the conversation, and
`scalpel-console--refresh-consumed-body-markers', which says so on
screen.  The console carries it as display properties only, never as
buffer text, so it can reach neither the conversation nor the
pending-input scanner.")

(defconst scalpel-console--continuation-instruction
  (concat "The shell command from the previous round already ran; its\n"
          "output is in the conversation above.  Read that output and\n"
          "decide now: if it already answers the user's request, reply\n"
          "with the conclusion; otherwise issue at most one concrete\n"
          "next action.  Do not run the same shell command again.")
  "Instruction sent on a continued round after a shell command ran.
The user's original instruction is already in the conversation, so
re-sending it makes the planner re-issue the same shell action.
Structural contract shared with `scalpel-console--run-rounds'.")

(defvar-local scalpel-console--root nil
  "Absolute directory this console session is anchored to.
Set by `scalpel-console-open'; while non-nil, `default-directory'
in the console buffer is pinned to this value.")

(defvar-local scalpel-console--context-baseline 'none-yet
  "Context entries shown by the previous refresh.
The symbol `none-yet' means no refresh has happened in this
console buffer yet, so nothing is highlighted as changed.")

(defface scalpel-console-context-added-face
  '((t :inherit bold))
  "Face for files newly added to the Scalpel context."
  :group 'scalpel)

(defface scalpel-console-context-removed-face
  '((t :inherit shadow :strike-through t))
  "Face for files removed from the Scalpel context."
  :group 'scalpel)

(defface scalpel-console-context-unchanged-face
  '((t :inherit shadow))
  "Face for files unchanged in the Scalpel context."
  :group 'scalpel)

(defface scalpel-console-consumed-body-face
  '((t :inherit shadow :slant italic))
  "Face for the header lines of a report the planner no longer reads in full.
`scalpel-console--refresh-consumed-body-markers' applies it on the
display only: the text, the conversation and the pending-input
scanner are all unchanged."
  :group 'scalpel)

(defun scalpel-console--buffer-name (root)
  "Return the Scalpel console buffer name for ROOT.
ROOT is expanded, normalized with `file-name-as-directory' and
`directory-file-name', then abbreviated with `abbreviate-file-name'."
  (format scalpel-console-buffer-name-format
          (abbreviate-file-name
           (directory-file-name
            (file-name-as-directory (expand-file-name root))))))

(defun scalpel-console--target-buffer ()
  "Return the console buffer the current command should write to.
Return the current buffer when it is a console buffer; otherwise
return the console buffer whose root contains `default-directory',
or signal a `user-error' when there is none."
  (cond
   ((derived-mode-p 'scalpel-console-mode)
    ;; The mode function is reachable via M-x; a buffer entered that way
    ;; has no root, so fail loud instead of anchoring to the caller's
    ;; default-directory.
    (unless scalpel-console--root
      (user-error "Scalpel: console buffer has no root; use `scalpel-open'"))
    (current-buffer))
   (t
    (let ((dir (file-name-as-directory (expand-file-name default-directory)))
          found)
      (dolist (buf (buffer-list))
        (unless found
          (with-current-buffer buf
            (when (and (bound-and-true-p scalpel-console--root)
                       (string-prefix-p scalpel-console--root dir))
              (setq found buf)))))
      (or found
          (user-error "Scalpel: no console for %s; run `scalpel-open'" dir))))))

(defun scalpel-console--tag-change (pos)
  "Return the next position after POS where the console tags change.
POS must be below `point-max'.  Tags are `scalpel-console-role'
\\(the conversation) and `scalpel-console-output' (display-only
output)."
  (min (next-single-property-change
        pos 'scalpel-console-role nil (point-max))
       (next-single-property-change
        pos 'scalpel-console-output nil (point-max))))

(defun scalpel-console--pending-input-regions ()
  "Return the (BEG . END) regions the user typed but has not sent.
A region is text that carries neither `scalpel-console-role' nor
`scalpel-console-output'.  Regions come back in buffer order, so a
caller can delete them without invalidating earlier ones."
  (let ((pos (point-min))
        (regions nil))
    (while (< pos (point-max))
      (let ((next (scalpel-console--tag-change pos)))
        (unless (or (get-text-property pos 'scalpel-console-role)
                    (get-text-property pos 'scalpel-console-output))
          (push (cons pos next) regions))
        (setq pos next)))
    (nreverse regions)))

(defvar scalpel-console-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'scalpel-console-send-line)
    ;; S-RET inserts a literal newline; RET sends the whole composed block.
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "C-c C-c") #'scalpel-console-send-line)
    (define-key map (kbd "C-c C-b") #'scalpel-llm-select-backend)
    (define-key map (kbd "C-c C-a") #'scalpel-console-add-file)
    (define-key map (kbd "C-c C-d") #'scalpel-console-remove-file)
    (define-key map (kbd "C-c C-r") #'scalpel-console-reset-context)
    (define-key map (kbd "C-c C-f") #'scalpel-console-forget-history)
    (define-key map (kbd "C-c C-o") #'scalpel-console-toggle-output)
    map)
  "Keymap used in Scalpel console buffers.")

(defvar-local scalpel-console--busy nil
  "Non-nil while an agent request is in flight for this console.
Buffer-local: a busy console must not refuse instructions in
another console.")

(defun scalpel-console--status-start ()
  "Insert a one-line status display at point-max.
Return a cons (REFRESH . STOP) of zero-arg functions.  REFRESH
rewrites the line with the current token counters and whole
elapsed seconds; STOP removes the line together with its trailing
newline, so the cursor returns to the line the status occupied."
  (let ((inhibit-read-only t)
        (start (float-time))
        beg)
    (goto-char (point-max))
    (setq beg (point-marker))
    (insert (format "Scalpel: %d up, %d down, 0s\n"
                    scalpel-llm--tokens-uploaded
                    scalpel-llm--tokens-received))
    (let ((refresh
           (lambda ()
             (when (marker-buffer beg)
               (with-current-buffer (marker-buffer beg)
                 (save-excursion
                   (let ((inhibit-read-only t))
                     (goto-char beg)
                     (delete-region (point) (1+ (line-end-position)))
                     (insert (format "Scalpel: %d up, %d down, %ds\n"
                                     scalpel-llm--tokens-uploaded
                                     scalpel-llm--tokens-received
                                     (round (- (float-time) start))))))))))
          (stop
           (lambda ()
             (when (marker-buffer beg)
               (with-current-buffer (marker-buffer beg)
                 (save-excursion
                   (let ((inhibit-read-only t))
                     (goto-char beg)
                     (delete-region (point) (1+ (line-end-position)))))
                 (set-marker beg nil))))))
      (cons refresh stop))))

(define-derived-mode scalpel-console-mode text-mode "Scalpel Console"
  "Major mode for Scalpel's interactive console buffer.

Type a natural-language instruction and press RET to send it; S-RET
inserts a newline instead, so an instruction may span several lines.
The agent processes the whole pending instruction and appends its
reply to this buffer.
Not a user entry point: open a console with `scalpel-open', which
also anchors the buffer to a root directory."
  (setq-local electric-indent-mode nil)
  (setq-local comment-start "")
  (setq buffer-read-only nil))

(defun scalpel-console--insert-tagged (text role)
  "Insert TEXT at point tagged with ROLE in `scalpel-console-role'.
The tag is what `scalpel-console--history' reads back, so text
inserted through here becomes part of the conversation sent to the
LLM.  Display-only output must not use it.  The tag is not
permanent: `scalpel-console-forget-history' clears it, which
removes the region from the conversation while leaving the visible
text alone."
  (let ((beg (point)))
    (insert text)
    (put-text-property beg (point) 'scalpel-console-role role)
    ;; Keyboard input arrives through `insert-and-inherit', which copies
    ;; the text properties of the character before point.  Without
    ;; `rear-nonsticky', the very first character the user types after
    ;; a reply or a context tree would inherit these tags, and the
    ;; pending-input scanner would then skip the whole instruction.  The
    ;; fold properties are listed too: a `display' inherited from a
    ;; folded body would replace the user's own first keystroke.
    (put-text-property beg (point) 'rear-nonsticky
                       '(scalpel-console-role scalpel-console-output
                         scalpel-console-collapsed display))))

(defun scalpel-console--trim-report (text)
  "Replace the fenced body of a report in TEXT with a placeholder.
TEXT is one assistant turn as recorded in the console buffer.  Only
the text between \"--- output ---\" and \"--- end output ---\" is
replaced; the report's header lines stay, so the reader still knows
what ran, how it exited, and how large the output was.  TEXT with
no fence, or with an unterminated one, comes back unchanged."
  (let ((body-start (string-match "\n--- output ---\n" text)))
    (if (null body-start)
        text
      (let* ((start (+ body-start (length "\n--- output ---\n")))
             (end (string-match "\n--- end output ---" text start)))
        (if (null end)
            text
          (concat (substring text 0 start)
                  scalpel-console--consumed-output-marker
                  (substring text end)))))))

(defun scalpel-console--assistant-report-regions ()
  "Return one (BEG . END) per assistant round, in buffer order.
A round is a stretch of text under one `scalpel-console-role' value,
which is what `scalpel-console--history' returns, so two rounds not
separated by a user round come back as one region here too."
  (let ((pos (point-min))
        regions)
    (while (< pos (point-max))
      (let ((next (next-single-property-change
                   pos 'scalpel-console-role nil (point-max))))
        (when (eq (get-text-property pos 'scalpel-console-role) 'assistant)
          (push (cons pos next) regions))
        (setq pos next)))
    (nreverse regions)))

(defun scalpel-console--consumed-body-regions ()
  "Return the assistant rounds whose output body the planner no longer processes.
The newest round keeps its body, for the round that consumes it.  Of the
older rounds, only those whose body `scalpel-console--trim-report'
would replace are returned: that is the condition
`scalpel-console--history' trims under, and the only one there is to
report.  Return nil when `scalpel-console-trim-consumed-output' is
nil, because then no body is dropped at all."
  (when scalpel-console-trim-consumed-output
    (cl-loop
     for region in (butlast (scalpel-console--assistant-report-regions))
     for text = (buffer-substring-no-properties (car region) (cdr region))
     for trimmed = (scalpel-console--trim-report text)
     unless (string= trimmed text)
     collect region)))

(defun scalpel-console--make-nonsticky (beg end props)
  "Extend the `rear-nonsticky' list of BEG..END with PROPS.
The list already there is preserved: a header line declares the
console's own tags nonsticky, and replacing it would let keyboard
input inherit them and stop reading as pending input."
  (let ((existing (get-text-property beg 'rear-nonsticky)))
    (put-text-property
     beg end 'rear-nonsticky
     (if (eq existing t)
         t
       (cl-union (and (listp existing) existing) props)))))

(defun scalpel-console--mark-consumed-body (region)
  "Mark one consumed report REGION on the display.
REGION is (BEG . END) of an assistant round whose output body the
planner no longer processes.  Nothing is inserted and no text is
replaced: only properties are set, so the record and the
conversation keep every byte.  The report's header lines carry the
mark, not its body: the body may be folded out of sight, and the
header is what stays readable.  The caller binds
`inhibit-read-only'."
  (let* ((beg (car region))
         (end (min (cdr region)
                   (save-excursion
                     (goto-char beg)
                     ;; The header runs to the body fence, so every
                     ;; header line carries the mark; a report whose
                     ;; fence is missing falls back to its first line.
                     (if (re-search-forward "\n--- output ---\n"
                                            (cdr region) t)
                         (match-beginning 0)
                       (line-end-position))))))
    (when (< beg end)
      (put-text-property beg end 'scalpel-console-consumed-body t)
      (put-text-property beg end 'face
                         'scalpel-console-consumed-body-face)
      (put-text-property beg end 'help-echo
                         scalpel-console--consumed-body-note)
      ;; Keyboard input arrives through `insert-and-inherit', which
      ;; copies the preceding character's properties unless they are
      ;; declared `rear-nonsticky'.  A face left out of that list would
      ;; make the user's own typing inside a header look spent.
      (scalpel-console--make-nonsticky
       beg end '(face help-echo scalpel-console-consumed-body)))))

(defun scalpel-console--clear-consumed-body-markers ()
  "Remove every consumed-body mark from the current buffer.
The stickiness guard `scalpel-console--mark-consumed-body' adds is
left in place: it only suppresses property inheritance, and the
range carries no marker properties afterwards."
  (let ((inhibit-read-only t)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change
                   pos 'scalpel-console-consumed-body nil (point-max))))
        (when (get-text-property pos 'scalpel-console-consumed-body)
          (remove-text-properties
           pos next '(scalpel-console-consumed-body nil face nil
                       help-echo nil)))
        (setq pos next)))))

(defun scalpel-console--refresh-consumed-body-markers ()
  "Mark every report whose output body the planner no longer processes.
The mark is display-only, so `scalpel-console--history' returns the same
bytes and `scalpel-console--pending-input-regions' sees no new region.
Which reports are marked is derived from the buffer on every call,
exactly as `scalpel-console--history' derives the trimming, so the
marks and the conversation cannot disagree.  Clearing before marking
keeps the result idempotent, and a call with
`scalpel-console-trim-consumed-output' nil clears the marks left from
before it was turned off."
  (let ((inhibit-read-only t))
    (scalpel-console--clear-consumed-body-markers)
    (dolist (region (scalpel-console--consumed-body-regions))
      (scalpel-console--mark-consumed-body region))))

(defun scalpel-console--history ()
  "Return the conversation recorded in the current buffer, as text.
Every region carrying a `scalpel-console-role' property is joined in
buffer order; regions without one — the header, context trees,
status lines — are dropped.  The buffer is the only record: nothing
is kept in a variable, so what the user sees is what the agent
gets.

Only the newest assistant report keeps its output body.  A report
is read back exactly once, by the round that follows it, so with
`scalpel-console-trim-consumed-output' set the bodies of every
older report are replaced by
`scalpel-console--consumed-output-marker'; their headers remain, so
the planner still knows what ran and how much it produced.  The
trim is a projection over the buffer text, never an edit of it, so
the console keeps the whole record.  Which reports are trimmed is
shown on screen by `scalpel-console--refresh-consumed-body-markers',
which derives it from the same regions and the same
`scalpel-console--trim-report'."
  (let ((pos (point-min))
        (turns nil))
    (while (< pos (point-max))
      (let ((next (next-single-property-change
                   pos 'scalpel-console-role nil (point-max))))
        (let ((role (get-text-property pos 'scalpel-console-role)))
          (when role
            (push (cons role (buffer-substring-no-properties pos next))
                  turns)))
        (setq pos next)))
    (let ((turns (nreverse turns))
          (newest-assistant nil))
      (cl-loop for turn in turns
               for i from 0
               when (eq (car turn) 'assistant)
               do (setq newest-assistant i))
      (string-trim
       (string-join
        (cl-loop for turn in turns
                 for i from 0
                 collect (if (and scalpel-console-trim-consumed-output
                                  (eq (car turn) 'assistant)
                                  (not (eql i newest-assistant)))
                             (scalpel-console--trim-report (cdr turn))
                           (cdr turn)))
        "")))))

(defun scalpel-console--collapse-report-bodies (beg end)
  "Hide each fenced report body between BEG and END.
The fold is display-only: the body keeps its text, so
`buffer-substring-no-properties' and `scalpel-console--history'
still return it whole, and `scalpel-console--trim-report' keeps
working on it.  Each hidden body is tagged
`scalpel-console-collapsed' with its placeholder, which
`scalpel-console-toggle-output' re-installs.  A report is a
\"--- output ---\"/\"--- end output ---\" pair, and one round's
report may hold several, so this loops to END.  Does nothing when
`scalpel-console-collapse-output' is nil."
  (when scalpel-console-collapse-output
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "\n--- output ---\n" end t)
          (let ((body-start (point)))
            (if (null (re-search-forward "\n--- end output ---" end t))
                ;; Unterminated fence: the remainder cannot be trusted
                ;; to pair up, so no later body is folded either.
                (goto-char end)
              (let ((body-end (match-beginning 0)))
                (when (> body-end body-start)
                  (let* ((lines (count-lines body-start body-end))
                         (placeholder
                          (format "[%d line%s of output; C-c C-o shows]"
                                  lines (if (= lines 1) "" "s"))))
                    (put-text-property body-start body-end
                                       'scalpel-console-collapsed placeholder)
                    (put-text-property body-start body-end
                                       'display placeholder)))))))))))

(defun scalpel-console--append (text &optional role)
  "Append TEXT to the end of the console buffer.
ROLE, when non-nil, tags TEXT as part of the conversation
\(`user' or `assistant'), so `scalpel-console--history' returns it
back; output appended without a role is display-only, as context
trees are, and is tagged so it can never be mistaken for an
instruction the user still has to send.  Point moves to the new
end, so the user always sees the latest output after a context
refresh or reply.  Appending an assistant round also refreshes the
consumed-body marks: that round is the cycle that processes the previous
report's output, so the previous body stops being sent."
  (let ((buf (scalpel-console--target-buffer)))
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((inhibit-read-only t)
            (beg (point)))
        (scalpel-console--insert-tagged (format "%s\n\n" text) role)
        (unless role
          (put-text-property beg (point) 'scalpel-console-output t))
        ;; Fold every fenced report body in what was just appended, so a
        ;; large shell dump does not bury the conversation.  Display
        ;; only: the record above and the projection `--history' builds
        ;; are both unchanged.
        (scalpel-console--collapse-report-bodies beg (point))
        ;; An assistant turn is the round that reads the previous
        ;; report's output, so that earlier body stops being sent.  Only
        ;; this role can change which report is newest, so only this
        ;; role pays for the rescan.
        (when (eq role 'assistant)
          (scalpel-console--refresh-consumed-body-markers)))
      (goto-char (point-max)))))

(defun scalpel-console-toggle-output ()
  "Show or hide every fenced report body in the console.
Hiding is display-only: the text stays in the buffer, so the
conversation built by `scalpel-console--history' is unaffected.
The state of the first folded body found decides for the whole
buffer, so one call never leaves a half-expanded console."
  (interactive)
  (with-current-buffer (scalpel-console--target-buffer)
    (let ((inhibit-read-only t)
          (pos (point-min))
          found
          expand)
      ;; One decision for the whole buffer: a mixed state would make the
      ;; next toggle ambiguous.
      (while (and (< pos (point-max)) (not found))
        (let ((next (next-single-property-change
                     pos 'scalpel-console-collapsed nil (point-max))))
          (when (get-text-property pos 'scalpel-console-collapsed)
            ;; A body still wearing its placeholder is folded, so this
            ;; call expands; one without is already expanded, so this
            ;; call folds.  `expand' names the action to take, not the
            ;; state found.
            (setq found t
                  expand (not (null (get-text-property pos 'display)))))
          (setq pos next)))
      (when found
        (setq pos (point-min))
        (while (< pos (point-max))
          (let ((next (next-single-property-change
                       pos 'scalpel-console-collapsed nil (point-max))))
            (when (get-text-property pos 'scalpel-console-collapsed)
              (if expand
                  (remove-text-properties pos next '(display nil))
                (put-text-property
                 pos next 'display
                 (get-text-property pos 'scalpel-console-collapsed))))
            (setq pos next)))
        (message "Scalpel: report bodies %s" (if expand "shown" "hidden"))))))

(defun scalpel-console--render-diff (lines)
  "Return LINES as text with per-name change highlighting.
LINES is a list of display cells as returned by
`scalpel-agent-context-update'.  Added names are bolded; removed
names are dimmed and struck through.  The tree graphics are never
highlighted."
  (mapconcat
   (lambda (cell)
     (let* ((text (plist-get cell :text))
            (start (plist-get cell :name-start))
            (graphics (substring text 0 start))
            (name (substring text start)))
       (pcase (plist-get cell :status)
         ('added (concat graphics
                         (propertize name
                                     'face 'scalpel-console-context-added-face)))
         ('removed (concat graphics
                           (propertize name
                                       'face 'scalpel-console-context-removed-face)))
         ('same (concat graphics
                        (propertize name
                                    'face 'scalpel-console-context-unchanged-face)))
         (_ text))))
   lines
   "\n"))

(defun scalpel-console--show-context ()
  "Append the context tree, marking the delta since the last refresh.
Files git would ignore are marked in the tree.  Lines dropped since
the previous refresh are dimmed and struck through, newly added
lines are bolded.  The baseline is replaced afterwards, so each
delta is highlighted exactly once."
  (with-current-buffer (scalpel-console--target-buffer)
    (let* ((ignored (scalpel-agent--git-ignored-files
                     scalpel-agent--context-files))
           (result (scalpel-agent-context-update
                    scalpel-console--context-baseline ignored))
           (lines (car result)))
      (setq scalpel-console--context-baseline (cdr result))
      (scalpel-console--append
       (if (null lines)
           "Context: none"
         (concat "Context:\n" (scalpel-console--render-diff lines)))))))

(defun scalpel-console-add-file (&optional ignore-gitignore)
  "Prompt for a file or directory and add it to the context.
With prefix argument IGNORE-GITIGNORE, do not filter directory
expansion through gitignore rules."
  (interactive "P")
  ;; The session context is buffer-local to the console, so the add has
  ;; to happen there: run from another buffer it would set that buffer's
  ;; local value and the console would appear unchanged.
  (with-current-buffer (scalpel-console--target-buffer)
    (let ((path (read-file-name "Add to Scalpel context: ")))
      (scalpel-agent-context-add path ignore-gitignore)
      (scalpel-console--show-context))))

(defun scalpel-console-remove-file ()
  "Prompt for a context file or directory and remove it."
  (interactive)
  ;; Read the list and remove from it in the console buffer, where the
  ;; buffer-local session context lives.
  (with-current-buffer (scalpel-console--target-buffer)
    (let ((candidates scalpel-agent--context-files))
      (if (null candidates)
          (message "Scalpel: context is empty")
        (let ((path (completing-read "Remove from Scalpel context: "
                                     candidates nil nil)))
          (scalpel-agent-context-remove path)
          (scalpel-console--show-context))))))

(defun scalpel-console-forget-history ()
  "Stop sending the current conversation to the agent.
Every prior turn keeps its place in the buffer but loses its
`scalpel-console-role' tag, so `scalpel-console--history' no
longer returns it.  Forgetting is about what the agent reads, not
about what the user sees: the turns stay on screen and can still
be reviewed.  The next instruction is sent as the first turn of a
new session, so the request no longer grows with the length of the
session.  The header, the context tree and the file list are kept:
the context is input, not memory — to reset it use
`scalpel-console-reset-context'.  The consumed-body marks go with
the turns: a forgotten turn is not read at all, so a mark saying its
body was dropped would describe the wrong thing."
  (interactive)
  (with-current-buffer (scalpel-console--target-buffer)
    (let ((inhibit-read-only t)
          (pos (point-min))
          ranges)
      ;; Collect every conversation region before touching anything:
      ;; clearing the role places a new property boundary, and
      ;; `next-single-property-change' below must see the original
      ;; layout.
      (while (< pos (point-max))
        (let ((next (next-single-property-change
                     pos 'scalpel-console-role nil (point-max))))
          (when (get-text-property pos 'scalpel-console-role)
            (push (cons pos next) ranges))
          (setq pos next)))
      ;; Drop the role tag instead of the text: `scalpel-console--history'
      ;; reads only tagged regions, so clearing the tag removes the turn
      ;; from the conversation while leaving it visible in the buffer.
      (dolist (range ranges)
        (put-text-property (car range) (cdr range)
                           'scalpel-console-role nil)
        ;; A forgotten turn is history on screen, not an instruction
        ;; still to send: tag it so it never reads back as input.
        (put-text-property (car range) (cdr range)
                           'scalpel-console-output t))
      ;; Re-deriving the marks clears them: with the roles gone, no turn
      ;; is an assistant turn any more.
      (scalpel-console--refresh-consumed-body-markers)
      (goto-char (point-max))
      (message "Scalpel: conversation forgotten; the text stays on screen."))))

(defun scalpel-console-reset-context ()
  "Clear the agent context.
The context is the set of files in scope; it is not the
conversation, and no file is added back from the open buffers.  To
clear the conversation instead, use
`scalpel-console-forget-history', which leaves this list alone."
  (interactive)
  ;; Reset the console's own context, not the caller buffer's.
  (with-current-buffer (scalpel-console--target-buffer)
    (scalpel-agent-context-reset)
    (scalpel-console--show-context)))

(defun scalpel-console-open ()
  "Open (or switch to) the Scalpel console buffer and clear its contents.
Internal setup routine of `scalpel-open'; not an interactive command.
The console is anchored to `default-directory' at call time: the
buffer name embeds the path and the buffer's `default-directory' is
pinned to it, so any file-system command run inside the console uses
that path."
  (let* ((root (file-name-as-directory (expand-file-name default-directory)))
         (buf (get-buffer-create (scalpel-console--buffer-name root))))
    (switch-to-buffer buf)
    (setq buffer-read-only nil)
    (unless (eq major-mode 'scalpel-console-mode)
      (scalpel-console-mode))
    (setq-local scalpel-console--root root)
    (setq-local default-directory root)
    (setq-local scalpel-console--context-baseline 'none-yet)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (insert "Scalpel console.\n")
      (insert "Type an instruction; S-RET inserts a newline, RET sends it.\n")
      (insert "\n")
      ;; The header is output, not pending input.  `rear-nonsticky'
      ;; keeps `insert-and-inherit' from copying the tag into whatever
      ;; the user types right after it.
      (put-text-property (point-min) (point) 'scalpel-console-output t)
      (put-text-property (point-min) (point) 'rear-nonsticky
                         '(scalpel-console-role scalpel-console-output))
      (goto-char (point-max)))
    (scalpel-agent-context-reset)
    (scalpel-console--show-context)
    (goto-char (point-max))))

(defun scalpel-console--run-round (instruction history on-complete)
  "Run one agent round for INSTRUCTION without blocking.
HISTORY is the conversation text to send along.  ON-COMPLETE
receives the plist `scalpel-agent-run' delivers, or nil when the
round failed; a failure is appended like a reply, so the next
round can read it instead of losing it.  The console buffer is
captured up front: if the user kills it while the request is in
flight, writes are skipped but the round still settles.  The
progress callback is installed for the duration and cleared before
ON-COMPLETE, so it is never left pointing at a dead buffer."
  (let* ((target (scalpel-console--target-buffer))
         (status (with-current-buffer target
                   (scalpel-console--status-start)))
         (refresh (car status))
         (stop (cdr status))
         (settled nil)
         (settle (lambda (result)
                   (unless settled
                     (setq settled t)
                     (setq scalpel-llm--progress-callback nil)
                     (when (buffer-live-p target)
                       (with-current-buffer target
                         (funcall stop)))
                     (if (buffer-live-p target)
                         (funcall on-complete result)
                       ;; The round callback re-selects the console
                       ;; buffer, so with the console gone it cannot
                       ;; run at all.  Nothing needs releasing here:
                       ;; the busy guard is buffer-local and died with
                       ;; the buffer.
                       nil)))))
    (setq scalpel-llm--progress-callback refresh)
    (with-current-buffer target
      (scalpel-agent-run
       instruction history
       (lambda (result)
         (when (buffer-live-p target)
           (with-current-buffer target
             ;; Point belongs to the user now that the status refresh is
             ;; wrapped in `save-excursion'; a bare insert would land at
             ;; the cursor.  `--append' goes to point-max and is the same
             ;; conversation-tagged writer the rest of the console uses.
             (scalpel-console--append
              (format "Scalpel: %s" (plist-get result :report))
              'assistant)))
         (funcall settle result))
       (lambda (err)
         (when (buffer-live-p target)
           (with-current-buffer target
             (if (eq (plist-get err :type) 'sandbox)
                 ;; A sandbox failure is infrastructure, not
                 ;; conversation.  Its message names the backend, so it
                 ;; is shown to the user but never recorded as an
                 ;; assistant turn: sending it back would hand the
                 ;; planner the very boundary the prompt omits.  Only a
                 ;; round failure that says something about the planner
                 ;; -- malformed JSON, for one -- stays in the history.
                 (scalpel-console--append
                  (format "Scalpel error: %s" (plist-get err :message)))
               (let ((inhibit-read-only t))
                 (scalpel-console--insert-tagged
                  (format "Scalpel error: %s\n\n" (plist-get err :message))
                  'assistant))
               ;; An error turn is a conversation turn too: it becomes the
               ;; newest assistant turn, so the report before it has to be
               ;; marked.  The insertion stays direct rather than going
               ;; through `--append', to leave point placement as it was.
               (scalpel-console--refresh-consumed-body-markers))))
         (funcall settle nil))))))

(defun scalpel-console--shell-description (shell)
  "Describe one entry of the :shells list for a confirmation prompt.
SHELL is a plist carrying at least :command and :bytes."
  (format "%s (%d bytes%s)"
          (plist-get shell :command)
          (or (plist-get shell :bytes) 0)
          (cond ((plist-get shell :binary) ", binary")
                ((plist-get shell :truncated) ", truncated")
                (t ""))))

(defun scalpel-console--noisy-round-p (result)
  "Return non-nil when RESULT's shell output is too large to continue.
A round is noisy when any command was truncated or produced binary
data, or when the commands together produced more than
`scalpel-console-continue-after-shell-max-bytes' bytes."
  (let ((shells (plist-get result :shells)))
    (or (cl-some (lambda (shell)
                   (or (plist-get shell :truncated)
                       (plist-get shell :binary)))
                 shells)
        (> (cl-reduce #'+ shells
                      :key (lambda (shell)
                             (or (plist-get shell :bytes) 0))
                      :initial-value 0)
           scalpel-console-continue-after-shell-max-bytes))))

(defun scalpel-console--continue-p (result)
  "Return non-nil when another round should follow RESULT.
RESULT is a `scalpel-agent-run' result whose round ran shell
commands or read files.  A read is always continued: the planner
asked for it, so there is nothing to question.  Small shell output
continues without a question too; only a
round `scalpel-console--noisy-round-p' rejects is put to the
user, because its output may cost more in tokens than it is
worth.  The question names every command together with its output
size, so a command that dumped a large file is visible before its
output is sent back.  Batch runs never continue, so an unattended
run can never block on a prompt."
  (pcase scalpel-console-continue-after-shell
    ('always t)
    ('ask (or (not (scalpel-console--noisy-round-p result))
              (and (not noninteractive)
                   (yes-or-no-p
                    (format "Send the output of %s back to Scalpel anyway? (%s)?"
                            (if (= (length (plist-get result :shells)) 1)
                                "this command"
                              (format "these %d commands"
                                      (length (plist-get result :shells))))
                            (string-join
                             (mapcar #'scalpel-console--shell-description
                                     (plist-get result :shells))
                             ", "))))))
    (_ nil)))

(defun scalpel-console--run-rounds (instruction history)
  "Run agent rounds for INSTRUCTION until the loop ends.
HISTORY is the conversation recorded before INSTRUCTION.  Return
after dispatching the first round; each round's outcome is handled
in its own callback, which re-fetches the conversation from the
buffer, so shell output reaches the next round without anything
being carried in a variable.  A round that ran shell commands, or
read a file, may be followed by another, up to
`scalpel-console-max-rounds'.
Whether a round actually continues is decided by
`scalpel-console--continue-p', which questions the user only when
the round's output is noisy.  A continued round sends
`scalpel-console--continuation-instruction' instead of INSTRUCTION:
the original instruction is already inside the history, and
re-sending it makes the planner run the same shell command again.
`scalpel-console--busy' is set here and cleared at every terminal
point, so a second RET during a round is refused."
  (setq scalpel-console--busy t)
  (let ((target (scalpel-console--target-buffer))
        (round 0)
        (conversation history)
        (next-instruction instruction))
    (cl-labels
        ((run-next ()
           (setq round (1+ round))
           (scalpel-console--run-round
            next-instruction conversation
            (lambda (result)
              (with-current-buffer target
                (setq conversation (scalpel-console--history))
                (cond
                 ((not (and result (or (plist-get result :shells)
                                       (plist-get result :reads))))
                  (setq scalpel-console--busy nil))
                 ((>= round scalpel-console-max-rounds)
                  ;; No round is left, so asking would throw the answer
                  ;; away and the console would look hung.  Report the
                  ;; limit instead.  Echo it as well as append it: the
                  ;; user may not be looking at the end of the console
                  ;; buffer when the loop stops.
                  (setq scalpel-console--busy nil)
                  (let ((notice
                         (format "Scalpel: round limit (%d) reached; send the next instruction when ready"
                                 scalpel-console-max-rounds)))
                    (scalpel-console--append notice)
                    (message "%s" notice)))
                 ((scalpel-console--continue-p result)
                  ;; A continued round must not re-send the user's
                  ;; original instruction: it is already in the history
                  ;; above, and repeating it makes the planner re-issue
                  ;; the same shell action in a loop.
                  (setq next-instruction
                        scalpel-console--continuation-instruction)
                  (run-next))
                 (t
                  (setq scalpel-console--busy nil))))))))
      (run-next))))

(defun scalpel-console--busy-p ()
  "Return non-nil when the target console has a request in flight.
`scalpel-console--busy' is buffer-local to the console, so it has
to be read there: read from whichever buffer the command was
invoked in, it would answer for that buffer instead."
  (with-current-buffer (scalpel-console--target-buffer)
    scalpel-console--busy))

(defun scalpel-console-send-line ()
  "Send the pending instruction to the Scalpel agent and append the reply.
The pending instruction is every line typed since the last appended
output, so text composed with S-RET is sent as a single message.
Every round re-sends the conversation recorded in this buffer, so
the agent can access its own earlier replies and shell output."
  (interactive)
  (if (scalpel-console--busy-p)
      (progn
        (message "Scalpel: still working on the previous instruction...")
        (ding))
    (let ((buf (scalpel-console--target-buffer)))
      (unless (eq (current-buffer) buf)
        (switch-to-buffer buf))
      (let* ((regions (scalpel-console--pending-input-regions))
             (instr (string-trim
                     (mapconcat
                      (lambda (region)
                        (buffer-substring-no-properties
                         (car region) (cdr region)))
                      regions
                      ""))))
        (if (string-empty-p instr)
            (message "Scalpel: nothing to send.")
          ;; Read the conversation before this instruction joins it.
          (let ((history (scalpel-console--history)))
            ;; Rewrite the typed input into the logged user message, so the
            ;; instruction is not shown twice (once raw, once prefixed).
            ;; Regions are deleted back to front so positions stay valid.
            (let ((inhibit-read-only t))
              (dolist (region (reverse regions))
                (delete-region (car region) (cdr region)))
              (goto-char (point-max))
              (scalpel-console--insert-tagged
               (format "User: %s\n" instr) 'user))
            (scalpel-console--run-rounds instr history)
            (goto-char (point-max))
            (message "Scalpel: instruction sent.")))))))

(provide 'scalpel-console)

;;; scalpel-console.el ends here
