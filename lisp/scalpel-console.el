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

(require 'scalpel-redact)

(require 'cl-lib)
(require 'scalpel-commit)
(require 'scalpel-agent)
(require 'scalpel-diagnose)
(require 'scalpel-token)

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
A shell or file-peek report may hold thousands of lines that the user
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

(defcustom scalpel-console-self-heal-max 2
  "Maximum automatic retries per instruction for self-healable planner errors.
A planner error whose type is in `scalpel-diagnose-self-heal-types' is
retried automatically -- the failure report stays in the conversation,
so the model can correct its own reply -- until this many retries have
been spent or the same error type keeps recurring.  Exceeding the budget
falls through to the usual retry header for the user."
  :type 'natnum
  :group 'scalpel-console)

(defcustom scalpel-console-unattended-max-rounds 30
  "Rounds an unattended run may spend before it stops itself.
`scalpel-console-unattended' accepts a prefix argument to override
this for one run.  The limit is a guard rail, not a goal: the run
stops earlier when the task completes or the user aborts."
  :type 'natnum
  :group 'scalpel)

(defcustom scalpel-console-unattended-max-minutes 60
  "Maximum duration in minutes for an unattended run.
When the budget runs out, the unattended state exits:
`scalpel-agent-unattended-confirm' is cleared so actions ask
again, but the task's rounds keep running attended until they
finish or the user aborts.  This is a guard rail, not a goal; the
run ends earlier when the task completes or the user aborts."
  :type 'natnum
  :group 'scalpel)

(defvar-local scalpel-console--unattended-p nil
  "Non-nil while an unattended run owns this console.
Suppresses every question a round could ask and arms the round
limit; cleared at every terminal point of the run.")

(defvar-local scalpel-console--unattended-limit nil
  "Round limit of the unattended run in flight, or nil.")
(defvar-local scalpel-console--unattended-deadline nil
  "Absolute time when the unattended run in flight ends itself, or nil.")

(defvar-local scalpel-console--unattended-start nil
  "Time when the unattended run in flight began, or nil.")

(defconst scalpel-console--consumed-output-marker "[output consumed]"
  "Placeholder left where a consumed report body was trimmed.
Structural contract shared by `scalpel-console--trim-report' and
`scalpel-console-trim-consumed-output'.")

(defconst scalpel-console--consumed-body-note
  "Output already read back to the planner; no longer re-sent.  \\[scalpel-console-toggle-output] shows the text."
  "Tooltip for a report the planner no longer reads in full.
Structural contract shared by `scalpel-console--history', which
drops such a body from the conversation, and
`scalpel-console--refresh-consumed-body-markers', which says so on
screen.  The console carries it as display properties only, never as
buffer text, so it can reach neither the conversation nor the
pending-input scanner.")

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

(defface scalpel-console-planner-error-face
  '((t (:inherit error)))
  "Face for a non-sandbox error turn in the console.
The face inherits `error', so the turn's colour follows the
active theme's definition of `error' instead of hard-coding a
foreground.  Unlike `scalpel-console-consumed-body-face' --
which marks a body the planner no longer reads -- an error turn
still joins the conversation and is read by the planner on the
next round, so the face must not read as \"not sent\".  It only
needs to stand apart from consumed and output text.  A sandbox
failure never gets this face: it stays out of the conversation
entirely and must stay loud."
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
preferring the console with the most specific (longest) matching
root, or signal a `user-error' when there is none."
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
          found found-length)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (bound-and-true-p scalpel-console--root)
                     (string-prefix-p scalpel-console--root dir)
                     (> (length scalpel-console--root) (or found-length 0)))
            (setq found buf
                  found-length (length scalpel-console--root)))))
      (or found
          (user-error "Scalpel: no console for %s; run `scalpel-open'" dir))))))

(defun scalpel-console--confirm-kill ()
  "Ask the user before killing a Scalpel console buffer.
The buffer is the only record of the conversation, so killing it
destroys the session irrecoverably; every killer -- user command,
`kill-matching-buffer', a package -- passes through this query.
Two cases skip the question because there is no session record to
lose: batch runs have no user to answer, and a console rooted under
the variable `temporary-file-directory' is a test artifact, not a
user session -- without this exemption an in-process test run
blocks on the prompt."
  (or noninteractive
      (null scalpel-console--root)
      (string-prefix-p
       (file-name-as-directory
        (expand-file-name temporary-file-directory))
       (expand-file-name scalpel-console--root))
      (yes-or-no-p
       (format "Kill Scalpel console %s?  Its conversation record will be lost? "
               (buffer-name)))))

(defun scalpel-console--kill-reasoning-buffer ()
  "Kill the console's reasoning buffer along with the console.
Runs from the console's buffer-local `kill-buffer-hook': killing the
console kills its thinking buffer so per-console reasoning buffers
never linger after the session is gone.  Safe when the buffer does
not exist."
  (let ((buf (get-buffer (scalpel-llm--reasoning-buffer-name (current-buffer)))))
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defconst scalpel-console--session-variables
  '(scalpel-console--root
    scalpel-console--context-baseline
    scalpel-console--last-instruction
    scalpel-agent--context-files
    scalpel-commit--style
    scalpel-commit--language)
  "Buffer-local variables that together hold one console session.
`scalpel-console--session-snapshot' captures exactly these and
`scalpel-console--session-restore' puts them back, so a module reload
never costs a live console its session.  A new buffer-local session
variable belongs in this list; nothing else enumerates them, so
forgetting to add it here is the only way to lose one.

Transient state is deliberately left out.  `scalpel-console--busy'
and `scalpel-console--operation-generation' must not survive a reload,
and the reload is refused while busy is set, so restoring them would
only risk resurrecting a dead round.
`scalpel-agent--shell-output' is reset before and read within a
single action, so its value belongs to no session.
The commit style and language defaults chosen from a console are
carried here so they survive a module reload.")

(defconst scalpel-console--session-globals
  '(scalpel-token--console-totals
    scalpel-token--grand-up
    scalpel-token--grand-down)
  "Global variables carrying session accounting across consoles.
Unlike `scalpel-console--session-variables' these are not
buffer-local: the token totals span every console, so they are
captured once and set back once.  They are worth carrying because the
token buffer keeps its lines across a reload, and a counter reset to
zero would make its later lines disagree with its earlier ones.
A global that no reader depends on across a reload does not belong
here.")

(defconst scalpel-console--session-format-version 2
  "Format version of a value `scalpel-console--session-snapshot' returns.
Structural contract shared by the snapshot, which writes it, and
`scalpel-console--session-restore', which refuses any other value.

The version exists because the two halves are not always the same
code: `scalpel-test-reload-modules' snapshots with the code loaded at
that moment and restores with the code it has just read from disk, so
a file edited between the two is read back by a newer reader than its
writer.  The shapes differ -- a captured value is wrapped in a list --
and a reader that guessed at a shape it did not write met a bare string
where it expected a pair: the console's own root reached `car' as text
and signalled `wrong-type-argument', killing the reload before it had
put a single variable back.
Bump this whenever the shape changes.")

(defun scalpel-console--session-snapshot ()
  "Return the session state to carry across a module reload.
The per-buffer part is a list of (BUFFER-NAME . ((VAR . (VALUE)) ...))
covering the buffer-local variables in
`scalpel-console--session-variables'; the global part holds the same
\(VAR . (VALUE)) shape for `scalpel-console--session-globals'.  The
result is consumed by `scalpel-console--session-restore'.

The value is wrapped in a list so that a variable that held nil and a
variable that was unbound are different answers.  A restore that
could not tell them apart set the variable to nil either way, and a
module's own `defvar' -- which has already run by then -- never
overwrites a symbol that is already bound, so the nil outlived the
reload for every later reader.

A console's state lives in these variables, which die when the module
is unloaded, while the buffer text survives -- which is why they are
captured here and put back afterwards.  A variable carrying session
state goes in one of the two lists; nothing else enumerates them, so
there is one place, not several, to update when one is added.

The plist names the shape it was written in, under :version, because
the reader is not always the same code as the writer: the test runner
snapshots with the code loaded at that moment and restores with the
code it has just read from disk.  See
`scalpel-console--session-format-version'."
  (let (buffers)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p scalpel-console--root)
          (push (cons (buffer-name)
                      (mapcar (lambda (var)
                                (cons var
                                      (and (boundp var)
                                           (list (symbol-value var)))))
                              scalpel-console--session-variables))
                buffers))))
    (list :version scalpel-console--session-format-version
          :buffers (nreverse buffers)
          :globals (mapcar (lambda (var)
                             (cons var
                                   (and (boundp var)
                                        (list (symbol-value var)))))
                           scalpel-console--session-globals))))

(defun scalpel-console--session-restore (snapshot)
  "Put a console's session back after the modules were reloaded.
SNAPSHOT is a value `scalpel-console--session-snapshot' returned, in
the format `scalpel-console--session-format-version' names.  Install it
when it is that format and refuse it otherwise: the two halves of a
reload are not always the same code -- the test runner snapshots with
the code loaded at that moment and restores with the code it has just
read from disk -- and a reader that guessed at a shape it did not write
met a bare string where it expected a pair and signalled
`wrong-type-argument' on the console's own root, killing the reload
before it had put a single variable back.

A refused snapshot is named in the echo area, because the session state
goes with it, and the reload itself carries on: one stale snapshot must
not fail a whole test run.  A console left that way keeps its buffer and
starts with fresh session state; reopen it with `scalpel-open' to anchor
it again.  `scalpel-console--session-format-version' is what makes the
two halves comparable at all."
  (if (equal (plist-get snapshot :version)
             scalpel-console--session-format-version)
      (scalpel-console--session-install snapshot)
    (message (concat "Scalpel: session snapshot is format %S, not %d; "
                     "console sessions keep their buffers and start with "
                     "fresh session state (reopen one with `scalpel-open')")
             (plist-get snapshot :version)
             scalpel-console--session-format-version)))

(defun scalpel-console--session-install (snapshot)
  "Put back the values SNAPSHOT captured, where they were read from.
SNAPSHOT is a value `scalpel-console--session-snapshot' returned, in
the format `scalpel-console--session-format-version' names; the caller
has checked that, so each entry here really is one of the two pair
shapes that format defines.
The conversation never left the buffer, only the variables listed in
`scalpel-console--session-variables' and
`scalpel-console--session-globals' were lost, so re-installing them
is enough to carry the session on.  A buffer killed while the modules
were reloaded is skipped.  A variable the snapshot did not capture --
one that was unbound when it was taken -- is not set at all, so a
restore can never turn an unbound variable into a nil one."
  (dolist (entry (plist-get snapshot :buffers))
    (let ((buffer (get-buffer (car entry))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (unless (derived-mode-p 'scalpel-console-mode)
            (setq buffer-read-only nil)
            (scalpel-console-mode))
          ;; Every captured variable is restored through
          ;; `make-local-variable', so a binding the unload removed is
          ;; recreated and one it left alone gets its snapshot value
          ;; back.  An uncaptured one is left as the reload left it:
          ;; setting it would bind it to nil, and the module's own
          ;; `defvar' never overwrites a symbol that is already bound.
          (dolist (pair (cdr entry))
            (when (cdr pair)
              (set (make-local-variable (car pair)) (car (cdr pair)))))
          ;; `--root' was restored just above, so the console stays
          ;; anchored to the directory its session belongs to.
          (setq-local default-directory scalpel-console--root)))))
  ;; A reload re-creates each global from its `defvar' form, so the
  ;; captured value -- a hash table, say -- is set back onto the fresh
  ;; symbol and the accounting it held is not reset to empty.  A global
  ;; the snapshot did not capture is left exactly as the reload left
  ;; it: this restore is the only place this module can put nil there,
  ;; and a nil in the token accounting table is the
  ;; `wrong-type-argument hash-table-p nil' every later round reports,
  ;; because `defvar' cannot repair a symbol that is already bound.
  (dolist (pair (plist-get snapshot :globals))
    (when (cdr pair)
      (set (car pair) (car (cdr pair))))))

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
    ;; Aborts the in-flight round.
    (define-key map (kbd "C-c C-k") #'scalpel-console-abort)
    (define-key map (kbd "C-c C-b") #'scalpel-llm-select-backend)
    (define-key map (kbd "C-c C-a") #'scalpel-console-add-file)
    (define-key map (kbd "C-c C-d") #'scalpel-console-remove-file)
    (define-key map (kbd "C-c C-r") #'scalpel-console-reset-context)
    (define-key map (kbd "C-c C-f") #'scalpel-console-forget-history)
    (define-key map (kbd "C-c C-e") #'scalpel-console-repeat)
    (define-key map (kbd "C-c C-o") #'scalpel-console-toggle-output)
    ;; Fixed-prompt quick messages.
    (define-key map (kbd "C-c C-y") #'scalpel-console-send-decide-for-me)
    (define-key map (kbd "C-c C-n") #'scalpel-console-send-replan)
    (define-key map (kbd "C-c C-w") #'scalpel-console-send-why)
    (define-key map (kbd "C-c C-s") #'scalpel-console-send-summarize)
    ;; Analyze deeply and try again.
    (define-key map (kbd "C-c C-t") #'scalpel-console-send-retry)
    ;; Prepares an LLM commit for the current console's project.
    (define-key map (kbd "C-c C-c") #'scalpel-commit-run)
    map)
  "Keymap used in Scalpel console buffers.")

(defconst scalpel-console--planner-error-types
  '(parse tool-call prose malformed unknown-tool no-replacement)
  "Error types caused by the planner's reply, not by Scalpel or the user.
A round that fails with one of these did not run anything: the
model's output broke the action contract, so the console marks them
with a distinct header.  Which advice sits under that header
depends on the type: `tool-call' means the model answered in
another calling convention and `prose' that it followed no
parseable convention at all -- neither is fixed by sending the same
request to the same backend again -- while `parse' may come back
whole on a second try.")

(defconst scalpel-console--retry-advice
  (concat "(the model's reply was not usable; nothing was executed.  "
          "Press C-c C-e or M-x scalpel-console-repeat to retry)")
  "Advice shown under the header of an ordinary planner failure.
Structural contract shared by `scalpel-console--run-round', which
prints it, and the test that pins it, so the key it names stays the
key `scalpel-console-mode-map' actually binds.")

(defconst scalpel-console--tool-call-advice
  (concat "(the model answered in a tool-calling convention Scalpel does "
          "not parse, so nothing was executed.  A backend that answers "
          "this way tends to answer this way again, so retrying the same "
          "request rarely helps: switch the backend with C-c C-b, or "
          "rephrase.  To try again anyway: C-c C-e)")
  "Advice shown under the header when the planner used tool-call syntax.
Separate from `scalpel-console--retry-advice' because the two
failures differ in what the user can do.  This one names the
backend switch, and says \"tends to\" rather than \"will\" on
purpose: the repetition is an observation from one backend on one
task shape, not a property of every model that leaks this syntax.")

(defconst scalpel-console--prose-advice
  (concat "(the model answered in prose and sent no action array, so "
          "nothing was executed.  A reply is only ever executed as an "
          "action array, and asking for a long answer a second time tends "
          "to get the same shape back: rephrase the request so the answer "
          "fits one reply -- a conclusion, not an analysis -- switch the "
          "backend with C-c C-b, or ask the question in a plain gptel "
          "buffer, where nothing has to be parseable.  To try again "
          "anyway: C-c C-e)")
  "Advice shown under the header when the planner replied in prose.
Separate from `scalpel-console--retry-advice' for the same reason
the tool-call advice is: resending an identical instruction to the
same backend is not what fixes it.  The failure is the shape of the
answer rather than a malformed array, so the advice leads with
rephrasing the request and names the retry binding last, as the
escape hatch it is.")

(defvar-local scalpel-console--last-instruction nil
  "The instruction this console last sent, for `scalpel-console-repeat'.
Buffer-local: each console session repeats its own last instruction.")

(defvar-local scalpel-console--roger-marker nil
  "Start of the acknowledgement line `send-line' just appended.
The first round deletes the line when it installs the status
spinner, so the acknowledgement is never mistaken for a turn of
the conversation.  nil when no acknowledgement is pending.")

(defvar-local scalpel-console--busy nil
  "Non-nil while an agent request is in flight for this console.
Buffer-local: a busy console must not refuse instructions in
another console.")

(defvar-local scalpel-console--round-error nil
  "Per-round error state set by `scalpel-console--run-round'.
Before rendering the header for a round, `scalpel-console--run-round'
sets this to the failing round's error plist; it clears it at the start
of every round.  `scalpel-console--run-rounds' reads it to decide
whether an automatic self-heal retry should be attempted.")

(defvar-local scalpel-console--operation-generation 0
  "Generation identifying the current console operation.
Incremented when an operation starts or is aborted, so callbacks
from an older operation can settle their private resources without
writing reports or continuing rounds.")

(defun scalpel-console--status-start (breakdown)
  "Insert a one-line status display at point-max.
BREAKDOWN is a plist with keys :system, :context, :history and
:instruction, each an integer token estimate for the round.  The
display shows the four segment counts from BREAKDOWN, their sum as
the uploaded total, the received total as the growth of the
cumulative `scalpel-llm--total-received' since this line was
inserted, and whole elapsed seconds.  The cumulative counter is
what survives a new request inside the same round: the per-request
`scalpel-llm--tokens-received' is zeroed by every request, so a
user returning to the console would otherwise see the down count
shrink instead of having grown.
Return a cons (REFRESH . STOP).  REFRESH rewrites the line with the
current received total and whole elapsed seconds, and takes an
optional NEW-BREAKDOWN that replaces the four segment counts it
shows, so a caller can install the line with a placeholder
breakdown and then restate the real counts once they are known;
called with no argument -- as the streaming progress callback and
the per-second timer do -- it re-renders the counts it was last
given.  STOP cancels the timer and removes the line together with
its trailing newline, so the cursor returns to the line the status
occupied."
  (let* ((inhibit-read-only t)
         (start (float-time))
         (sys (plist-get breakdown :system))
         (ctx (plist-get breakdown :context))
         (hist (plist-get breakdown :history))
         (instr (plist-get breakdown :instruction))
         (up (+ sys ctx hist instr))
         (down0 scalpel-llm--total-received)
         (line (lambda (down seconds)
                 (format "Scalpel: up %d = sys %d + ctx %d + hist %d + instr %d, down %d, %ds\n"
                         up sys ctx hist instr down seconds)))
         ;; The status line is display-only output: tag it like other
         ;; output, so a line that outlives its round (an abort, a
         ;; settle that failed to stop the timer, an Unattended run
         ;; leaving one behind) is never read back as typed input by
         ;; `scalpel-console--pending-input-regions' and never joins
         ;; the conversation.  The final character is rear-nonsticky,
         ;; so text typed after the line inherits nothing from it.
         (paint (lambda (beg end)
                  (put-text-property beg end 'scalpel-console-output t)
                  (put-text-property (1- end) end 'rear-nonsticky
                                     '(scalpel-console-output face))))
         timer beg)
    (save-excursion
      (goto-char (point-max))
      (setq beg (point-marker))
      (let ((line-beg (point)))
        (insert (funcall line (- scalpel-llm--total-received down0) 0))
        (funcall paint line-beg (point))))
    (let ((refresh
           (lambda (&optional new-breakdown)
             ;; The line is installed with a placeholder breakdown so the
             ;; busy indicator shows before token counting runs; a caller
             ;; restates the four segment counts here once they are known.
             ;; A zero-arg call -- the streaming progress callback and the
             ;; per-second timer -- re-renders the counts it was last given.
             (when new-breakdown
               (setq sys (plist-get new-breakdown :system))
               (setq ctx (plist-get new-breakdown :context))
               (setq hist (plist-get new-breakdown :history))
               (setq instr (plist-get new-breakdown :instruction))
               (setq up (+ sys ctx hist instr)))
             (when (marker-buffer beg)
               (with-current-buffer (marker-buffer beg)
                 ;; Remember whether the user is parked at point-max
                 ;; (following the tail) before the rewrite; save-excursion
                 ;; would otherwise restore an integer position that lands
                 ;; inside the rewritten spinner line.
                 (let ((follow-tail (= (point) (point-max))))
                   (save-excursion
                     (let ((inhibit-read-only t))
                       (goto-char beg)
                       (delete-region (point) (1+ (line-end-position)))
                       (let ((line-beg (point)))
                         (insert (funcall line
                                          (- scalpel-llm--total-received down0)
                                          (round (- (float-time) start))))
                         (funcall paint line-beg (point)))))
                   (when follow-tail
                     (goto-char (point-max)))))))))
      (setq timer (run-with-timer 1 1 refresh))
      (let ((stop
             (lambda ()
               (when timer
                 (cancel-timer timer)
                 (setq timer nil))
               (when (marker-buffer beg)
                 (with-current-buffer (marker-buffer beg)
                   (save-excursion
                     (let ((inhibit-read-only t))
                       (goto-char beg)
                       (delete-region (point) (1+ (line-end-position)))))
                   (set-marker beg nil))))))
        (cons refresh stop)))))

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
  (setq buffer-read-only nil)
  ;; Yank restores the killed text's properties, so text killed from a
  ;; tagged region (header, context tree, a report) would paste back
  ;; still tagged as output or conversation, and the pending-input
  ;; scanner would never see it as an instruction.  Strip the console's
  ;; own properties on yank; buffer-local, so other buffers are untouched.
  (setq-local yank-excluded-properties
              (append yank-excluded-properties
                      '(scalpel-console-role scalpel-console-output
                        scalpel-console-collapsed scalpel-console-consumed-body
                        display rear-nonsticky)))
  ;; Buffer-local, so only console buffers ask; every killer of this
  ;; buffer goes through the query.
  (add-hook 'kill-buffer-query-functions
            #'scalpel-console--confirm-kill nil t)
  ;; Buffer-local: the console's reasoning buffer dies with the console,
  ;; so it cannot accumulate.
  (add-hook 'kill-buffer-hook #'scalpel-console--kill-reasoning-buffer nil t))

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
report.  Regions beginning with the planner-error face are skipped:
an error turn joins the conversation and is read by the planner next
round, so its body must never be marked consumed, even when the quoted
prose reply it holds contains literal output fences that make
`scalpel-console--trim-report' see a trimmable body.  Return nil when
`scalpel-console-trim-consumed-output' is nil, because then no body is
dropped at all."
  (when scalpel-console-trim-consumed-output
    (cl-loop
     for region in (butlast (scalpel-console--assistant-report-regions))
     unless (or (get-text-property (car region) 'scalpel-console-planner-error)
                (eq (get-text-property (car region) 'face)
                    'scalpel-console-planner-error-face))
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
The consumed-body face is removed together with the mark: it
describes spent output and must go when the mark goes.  Callers
that want their own dimming apply it after clearing.  The
stickiness guard `scalpel-console--mark-consumed-body' adds is
left in place: it only suppresses property inheritance."
  (let ((inhibit-read-only t)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change
                   pos 'scalpel-console-consumed-body nil (point-max))))
        (when (get-text-property pos 'scalpel-console-consumed-body)
          (remove-text-properties
           pos next '(scalpel-console-consumed-body nil
                       face nil
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

Planner-error turns are conversation for the agent too: they join
the history like any other turn, and only their role and text
matter here.  This mirrors the collection rule (not the skip rule)
in `scalpel-console--consumed-body-regions', which drops them from
consumption accounting while the history keeps them.

Only the newest assistant report keeps its output body.  A report
is read back exactly once, by the round that follows it, so with
`scalpel-console-trim-consumed-output' set the bodies of every
older report are replaced by
`scalpel-console--consumed-output-marker'; their headers remain, so
the planner still knows what ran and how much it produced.
Planner-error turns are exempt from trimming: the property
`scalpel-console-planner-error', read at the region's start
position, marks them, and the face-based fallback is deliberately
not used, since inspecting the `face' property with list predicates
broke when it held a bare symbol.  The trim is a projection over
the buffer text, never an edit of it, so the console keeps the
whole record.  Which reports are trimmed is shown on screen by
`scalpel-console--refresh-consumed-body-markers', which derives it
from the same regions and the same
`scalpel-console--trim-report'."
  (let ((pos (point-min))
        (turns nil))
    (while (< pos (point-max))
      (let ((next (next-single-property-change
                   pos 'scalpel-console-role nil (point-max))))
        (let ((role (get-text-property pos 'scalpel-console-role)))
          (when role
            (push (list role
                        (get-text-property pos 'scalpel-console-planner-error)
                        pos (buffer-substring-no-properties pos next))
                  turns))
          (setq pos next))))
    (let ((turns (nreverse turns))
          (newest-assistant nil))
      (cl-loop for turn in turns
               for i from 0
               when (and (eq (nth 0 turn) 'assistant)
                         (not (nth 1 turn)))
               do (setq newest-assistant i))
      (string-trim
       (string-join
        (cl-loop for turn in turns
                 for i from 0
                 collect (if (and scalpel-console-trim-consumed-output
                                  (eq (nth 0 turn) 'assistant)
                                  (not (nth 1 turn))
                                  (not (eql i newest-assistant)))
                             (scalpel-console--trim-report (nth 3 turn))
                           (nth 3 turn)))
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
instruction the user still has to send.  Such role-less output is
dimmed in `shadow', because it is never sent to the LLM, while
conversation turns and the context tree's own faces (declared in
`scalpel-console--render-diff') are left intact.  Its tail is marked
rear-nonsticky, so text the user types at the end inherits
nothing from it.  Consecutive same-role turns are separated by a
role-less newline, so each round forms its own region instead of
merging into one block.  Point
moves to the new end, so the user always sees the latest output after a
context refresh or reply.  Appending an assistant round also refreshes
the consumed-body marks: that round is the cycle that processes the
previous report's output, so the previous body stops being sent."
  (let ((buf (scalpel-console--target-buffer)))
    (with-current-buffer buf
      ;; Drop any pending acknowledgement line: any reply or error that
      ;; arrives now supersedes the "Roger. Working..." placeholder.
      ;; `scalpel-console-send-line' is the only code that puts the
      ;; `scalpel-console-ack' property on text, so we walk the recent
      ;; tail in constant-property runs via
      ;; `previous-single-property-change' and delete each run that
      ;; carries a non-nil `scalpel-console-ack'.  Scanning by runs
      ;; avoids both text comparison and the confusing match-boundary
      ;; semantics of `text-property-search-backward' that caused
      ;; off-by-one deletions ("calpel: Roger...").
      (let ((inhibit-read-only t)
            (scan-limit (max (point-min) (- (point-max) 4096)))
            (pos (point-max))
            done)
        (while (not done)
          (let* ((run-end pos)
                 (run-beg (previous-single-property-change
                           pos 'scalpel-console-ack nil scan-limit)))
            (cond
             ;; No change found within the scan limit: the run extends
             ;; to the limit (or the whole scanned region is one run).
             ((null run-beg)
              (when (get-text-property scan-limit 'scalpel-console-ack)
                (delete-region scan-limit run-end))
              (setq done t))
             (t
              (when (get-text-property run-beg 'scalpel-console-ack)
                (delete-region run-beg run-end))
              (setq pos run-beg)
              (when (<= pos scan-limit)
                (setq done t))))))
        (setq scalpel-console--roger-marker nil))
      ;; Read the user's position BEFORE moving: whether the user was
      ;; reading the end is decided from where they actually were, and
      ;; the move below then places the insertion at the end.
      (let* ((follow (>= (point) (point-max)))
             ;; A marker, not a position: the insert below shifts it,
             ;; and the user's place in the history must follow the
             ;; text they were reading, not the byte offset.
             (user-point (copy-marker (point) t)))
        (goto-char (point-max))
        (let ((inhibit-read-only t)
              (beg (point)))
          ;; Separate consecutive same-role rounds: without this, the
          ;; role runs would merge and only the first output fence of
          ;; the merged block would ever be trimmed or marked.
          (when (and role
                     (> (point) (point-min))
                     (eq (get-text-property (1- (point))
                                            'scalpel-console-role)
                         role))
            (let ((sep-beg (point)))
              (insert "\n")
              (put-text-property sep-beg (point)
                                 'scalpel-console-output t)
              (put-text-property (1- (point)) (point)
                                 'rear-nonsticky
                                 '(scalpel-console-role
                                   scalpel-console-output display))))
          (scalpel-console--insert-tagged (format "%s\n\n" text) role)
          (unless role
            (put-text-property beg (point) 'scalpel-console-output t))
          (unless role
            ;; Dim display-only output: walk the appended region in
            ;; constant-face runs and put `shadow' on exactly the runs
            ;; that carry no face.  Safe again because the context tree
            ;; now declares its own faces in `--render-diff' (graphics
            ;; get shadow there, file names get their per-status faces),
            ;; so this walk only touches plain display-only lines such
            ;; as the retry attempt notice and "Mission complete,
            ;; over.".
            (save-excursion
              (let ((pos beg))
                (while (< pos (point))
                  (let* ((face (get-text-property pos 'face))
                         (run-end (next-single-property-change
                                   pos 'face nil (point))))
                    (unless face
                      (put-text-property pos run-end 'face 'shadow))
                    (setq pos run-end)))))
            ;; Cut the sticky bridge: the properties just laid down (`face',
            ;; `scalpel-console-role', `scalpel-console-output') are
            ;; sticky by default, so text typed at the end would inherit
            ;; them.  Mark the final character rear-nonsticky, so
            ;; freshly typed text starts with no face and no output
            ;; tagging of its own.
            (put-text-property (1- (point)) (point)
                               'rear-nonsticky
                               '(face scalpel-console-role
                                 scalpel-console-output)))
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
            (scalpel-console--refresh-consumed-body-markers))
          ;; Follow the new output only when the user was already reading
          ;; the end.  An unconditional move dragged point to point-max, so
          ;; redisplay scrolled the window back to the bottom while the user
          ;; was reading earlier turns.  The next instruction does not need
          ;; point at the end: `scalpel-console-send-line' reads pending
          ;; input wherever it sits and re-anchors the record itself.
          (if follow
              (goto-char (point-max))
            ;; The insert above left point at the new end: the insertion
            ;; happened at point-max, and point rides it.  Restore the
            ;; reader's place explicitly.
            (goto-char user-point))
          (set-marker user-point nil))))))

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
names are dimmed and struck through; unchanged names keep the
unchanged face.  The tree graphics (the part before :name-start)
are display-only -- they are never sent to the LLM -- and are
dimmed here with the shadow face rather than by
`scalpel-console--append'.  Lines without a status are returned
as their cell text, unhighlighted."
  (mapconcat
   (lambda (cell)
     (let* ((text (plist-get cell :text))
            (start (plist-get cell :name-start))
            (graphics (substring text 0 start))
            (name (substring text start)))
       (pcase (plist-get cell :status)
         ('added (concat (propertize graphics
                                     'face 'shadow)
                         (propertize name
                                     'face 'scalpel-console-context-added-face)))
         ('removed (concat (propertize graphics
                                       'face 'shadow)
                           (propertize name
                                       'face 'scalpel-console-context-removed-face)))
         ('same (concat (propertize graphics
                                    'face 'shadow)
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
      ;; Clear the consumed-body marks before coloring: with the roles
      ;; cleared above, no turn is an assistant turn any more, so
      ;; re-deriving the marks clears them without re-applying any.
      (scalpel-console--refresh-consumed-body-markers)
      ;; Dim the text last: the shadow face says at a glance that what
      ;; the user sees is history, not the live conversation.  Applying
      ;; it after the refresh above means every forgotten turn,
      ;; including the old report headers, ends up shadowed.
      (dolist (range ranges)
        (put-text-property (car range) (cdr range)
                           'face 'shadow))
      ;; Leave a display-only note at the end of the buffer.  It carries
      ;; `scalpel-console-output' and no role, so it never reads back as
      ;; conversation or input; it only tells the user where the old
      ;; conversation ends.
      (goto-char (point-max))
      (let ((inhibit-read-only nil))
        (insert (propertize
                 "\nScalpel: the conversation above was forgotten; it stays visible but is no longer part of what the agent reads.\n\n"
                 'face 'shadow
                 'scalpel-console-output t
                 'rear-nonsticky t)))
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
  "Open a fresh Scalpel console buffer for the current directory.
Internal setup routine of `scalpel-open'; not an interactive command.
Each call creates a new console buffer for the root -- re-running
`scalpel-open' opens a new session rather than switching to an
existing one -- and the buffer's name carries Emacs's standard
uniqueness suffix when the name is taken (e.g. \"*scalpel: ~/proj*<2>\").
The console is anchored to `default-directory' at call time: the
buffer name embeds the path and the buffer's `default-directory' is
pinned to it, so any file-system command run inside the console uses
that path."
  (let* ((root (file-name-as-directory (expand-file-name default-directory)))
         (buf (get-buffer-create (generate-new-buffer-name (scalpel-console--buffer-name root)))))
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
ON-COMPLETE, so it is never left pointing at a dead buffer.  The
status line is installed before the token counts are measured, so
a slow count never leaves the user without feedback: a placeholder
breakdown appears immediately and is replaced with the real counts
by an initial refresh.  The acknowledgement line the send path
appended is removed here, when the status spinner replaces it.  A round
that changed the session's context file list redraws the context tree
before ON-COMPLETE, so the file list on screen still describes the
session the next round will run against.  The failing round's error
plist is recorded in `scalpel-console--round-error' before the header
rendering; that recorded plist is the signal the round loop in
`scalpel-console--run-rounds' consults for the automatic self-heal
retry decision, and it is cleared to nil at the start of each round
on success paths.  Outbound text is redacted through
`scalpel-redact-apply' before it reaches the model -- the
instruction and the history are wrapped so the real user name never
leaves the machine -- and inbound text is restored through
`scalpel-redact-restore' as it arrives: report text in the success
callback and rendered error headers in the error handler.  The
conversation buffer therefore never shows placeholders; it always
holds the real paths the user typed."
  (setq scalpel-console--round-error nil)
  (let* ((target (scalpel-console--target-buffer))
         ;; The acknowledgement `send-line' appended exists only for the
         ;; gap before this status line appears: remove it now, so it
         ;; neither lingers in the buffer nor lands in the conversation
         ;; the next round reads.  Verified against its text, because a
         ;; report may have been appended after it if an earlier settle
         ;; ran first.
         (_ (when (and (markerp scalpel-console--roger-marker)
                       (eq (marker-buffer scalpel-console--roger-marker) target))
              (with-current-buffer target
                (let ((inhibit-read-only t)
                      (beg (marker-position scalpel-console--roger-marker)))
                  (when (and beg (< beg (point-max))
                             (string-prefix-p
                              "Scalpel: Roger. Working..."
                              (buffer-substring-no-properties
                               beg (min (point-max) (+ beg 28)))))
                    (delete-region
                     beg (min (point-max) (1+ (line-end-position beg))))))
                (set-marker scalpel-console--roger-marker nil))))
         ;; Install the status line first with a placeholder breakdown,
         ;; so the busy indicator is visible before token counting runs.
         (status (with-current-buffer target
                   (scalpel-console--status-start
                    (list :system 0
                          :context 0
                          :history 0
                          :instruction 0))))
         (refresh (car status))
         (stop (cdr status))
         (breakdown
          (with-current-buffer target
            (list :system (scalpel-llm--count-tokens
                           scalpel-prompt-system-prompt)
                  :context (scalpel-llm--count-tokens
                            (scalpel-agent-context))
                  :history (scalpel-llm--count-tokens (or history ""))
                  :instruction (scalpel-llm--count-tokens
                                (or instruction "")))))
         ;; Rewrite the status line with the real counts as soon as
         ;; they are known.
         (_ (funcall refresh breakdown))
         ;; The round can change which files the session holds:
         ;; `file-create' adds the new one, `file-rename' moves its entry
         ;; and `file-delete' drops it.  This is the before-image the
         ;; settle path compares against, copied because the tools replace
         ;; the list rather than mutate it and the comparison must not
         ;; depend on that staying true.
         (context-before (with-current-buffer target
                           (copy-sequence scalpel-agent--context-files)))
         ;; Snapshot the cumulative counters before the round: a round
         ;; may issue several LLM requests (plan, then edit/create), so
         ;; the round's cost is the diff of the never-reset totals.
         (up0 scalpel-llm--total-uploaded)
         (down0 scalpel-llm--total-received)
         (settled nil)
         (settle (lambda (round-result)
                   (unless settled
                     (setq settled t)
                     (setq scalpel-llm--progress-callback nil)
                     (condition-case err
                         (scalpel-token-record
                          (buffer-name target)
                          (- scalpel-llm--total-uploaded up0)
                          (- scalpel-llm--total-received down0)
                          breakdown)
                       (error
                        (message "Scalpel: token accounting failed: %S" err)))
                     (condition-case err
                         (when (buffer-live-p target)
                           (with-current-buffer target
                             (funcall stop)))
                       (error
                        (message "Scalpel: status cleanup failed: %S" err)))
                     ;; A round that changed the context file list says so
                     ;; in the console, in the tree the context already
                     ;; uses: the baseline is what the previous refresh
                     ;; left, so the files this round added or dropped are
                     ;; the ones marked.  Before ON-COMPLETE, because a
                     ;; continued round appends its own output from there;
                     ;; the tree belongs to the round that changed the
                     ;; list.  Display only, so neither the conversation
                     ;; nor the next prompt is touched by it.
                     (condition-case err
                         (when (and (buffer-live-p target)
                                    (with-current-buffer target
                                      (not (equal context-before
                                                  scalpel-agent--context-files))))
                           (with-current-buffer target
                             (scalpel-console--show-context)))
                       (error
                        (message "Scalpel: context display failed: %S" err)))
                     (when (buffer-live-p target)
                       (funcall on-complete round-result))))))
    (let ((operation
           (buffer-local-value
            'scalpel-console--operation-generation target)))
      (setq scalpel-llm--progress-callback refresh)
      (with-current-buffer target
        (condition-case err
            (scalpel-agent-run
             ;; Outbound text is redacted here, at the console's single
             ;; send point: the model sees placeholders, never the real
             ;; user name, for both the instruction and the history.
             (scalpel-redact-apply instruction)
             (scalpel-redact-apply history)
             (lambda (round-result)
               (when (and (buffer-live-p target)
                          (= operation
                             (buffer-local-value
                              'scalpel-console--operation-generation target)))
                 (condition-case err
                     (with-current-buffer target
                       ;; Placeholders in the report are restored before
                       ;; formatting: the console -- and the conversation
                       ;; history it records -- always holds real paths,
                       ;; so the next round never round-trips a placeholder.
                       (scalpel-console--append
                        (format "Scalpel: %s"
                                (scalpel-redact-restore
                                 (plist-get round-result :report)))
                        'assistant))
                   ((error quit)
                    (funcall settle nil)
                    (signal (car err) (cdr err)))))
               (funcall settle round-result))
             (lambda (err)
               ;; Record the failing round's error plist before rendering
               ;; the header: the round loop in `scalpel-console--run-rounds'
               ;; reads `scalpel-console--round-error' to decide on a
               ;; self-heal retry.
               (setq scalpel-console--round-error err)
               (condition-case handler-err
                   (when (and (buffer-live-p target)
                              (= operation
                                 (buffer-local-value
                                  'scalpel-console--operation-generation target)))
                     (with-current-buffer target
                       (goto-char (point-max))
                       (if (eq (plist-get err :type) 'sandbox)
                           ;; A sandbox failure is infrastructure, not
                           ;; conversation.  Its message names the backend, so it
                           ;; is shown to the user but never recorded as an
                           ;; assistant turn: sending it back would hand the
                           ;; planner the very boundary the prompt omits.  Only a
                           ;; round failure that says something about the planner
                           ;; -- malformed JSON, for one -- stays in the history.
                           (scalpel-console--append
                            (format "Scalpel error: %s"
                                    (scalpel-redact-restore (or (plist-get err :message) ""))))
                         (let* ((inhibit-read-only t)
                                (err-beg (point))
                                ;; Planner errors may quote the raw reply,
                                ;; which was sent redacted: restore before
                                ;; rendering, so the header shows real paths.
                                (err-message
                                 (scalpel-redact-restore (or (plist-get err :message) "")))
                                (category (scalpel-diagnose-category
                                           (plist-get err :type)))
                                (header
                                 (cond
                                  ((eq category 'planner)
                                   (format "Scalpel planner error: %s\n%s\n\n"
                                           err-message
                                           (scalpel-diagnose-advice err)))
                                  ((eq category 'context)
                                   ;; Retry advice is useless here: the
                                   ;; instruction will fail again until the
                                   ;; missing context exists, but the
                                   ;; remedy must still be stated, so the
                                   ;; category-advice table is no longer dead
                                   ;; code on the render path.
                                   (format "Scalpel context error: %s\n%s\n\n"
                                           err-message
                                           (scalpel-diagnose-advice err)))
                                  (t
                                   (format "Scalpel error: %s\n\n"
                                           err-message)))))
                           (scalpel-console--insert-tagged
                            header
                            'assistant)
                           ;; The failure is dimmed for reading only: the
                           ;; turn still joins the conversation, so this
                           ;; must not read as a trimmed body.  The explicit
                           ;; `scalpel-console-planner-error' property is the
                           ;; durable marker; the face remains only as the
                           ;; legacy signal.  Both are declared
                           ;; `rear-nonsticky' so keyboard input typed after
                           ;; the turn inherits neither.
                           (put-text-property err-beg (point) 'face
                                              'scalpel-console-planner-error-face)
                           (put-text-property err-beg (point)
                                              'scalpel-console-planner-error t)
                           (scalpel-console--make-nonsticky
                            err-beg (point) '(face)))
                       ;; An error turn is a conversation turn too: it becomes the
                       ;; newest assistant turn, so the report before it has to be
                       ;; marked.  The insertion stays direct rather than going
                       ;; through `--append', to leave point placement as it was.
                       (scalpel-console--refresh-consumed-body-markers))))
                 (error
                  (message "Scalpel: error handler failed: %S" handler-err)))
               (funcall settle nil)))
          ((error quit)
           (funcall settle nil)
           (signal (car err) (cdr err))))))))

(defun scalpel-console--shell-description (shell)
  "Describe one entry of the :shells list for a confirmation prompt.
SHELL is a plist carrying at least :command and :bytes."
  (cl-labels ((human-size (bytes)
                (cond ((>= bytes 1073741824)
                       (format "%.1f GB" (/ bytes 1073741824.0)))
                      ((>= bytes 1048576)
                       (format "%.1f MB" (/ bytes 1048576.0)))
                      ((>= bytes 1024)
                       (format "%.1f KB" (/ bytes 1024.0)))
                      (t (format "%d bytes" bytes)))))
    (format "%s (%d bytes, %s%s)"
            (plist-get shell :command)
            (or (plist-get shell :bytes) 0)
            (human-size (or (plist-get shell :bytes) 0))
            (cond ((plist-get shell :binary) ", binary")
                  ((plist-get shell :truncated) ", truncated")
                  (t "")))))

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
commands, read files, or changed files.  A read is always
continued: the planner asked for it, so there is nothing to
question.  A round that changed a file is continued too -- an
edit, an insert, a delete, a rewrite, a created, renamed or
deleted file: the planner split the request across rounds, and
stopping after one file would abandon the rest, the file it just
made included.  Small shell output continues without a question too;
only a
round `scalpel-console--noisy-round-p' rejects is put to the
user, because its output may cost more in tokens than it is
worth.  The question names every command together with its output
size, so a command that dumped a large file is visible before its
output is sent back.  Batch runs never continue, so an unattended
run can never block on a prompt.

Return t to continue normally.  Return the symbol `declined' when
the user refused to send the noisy output back, so the next round
must continue without that output (the caller appends a note
saying the commands ran but their output was withheld from the
conversation).  Return nil to stop."
  (pcase scalpel-console-continue-after-shell
    ('always t)
    ('ask (or (not (scalpel-console--noisy-round-p result))
              (and (not noninteractive)
                   (if (yes-or-no-p
                        (format "Send the output of %s back to Scalpel anyway? (%s)?"
                                (if (= (length (plist-get result :shells)) 1)
                                    "this command"
                                  (format "these %d commands"
                                          (length (plist-get result :shells))))
                                (string-join
                                 (mapcar #'scalpel-console--shell-description
                                         (plist-get result :shells))
                                 ", ")))
                       t
                     'declined))))
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
the round's output is noisy.  An unattended run continues through
noise without asking: the user is away, and the report bodies stay
in the buffer for them to read afterwards.  A continued round sends
`scalpel-prompt--continuation-instruction' instead of INSTRUCTION:
the original instruction is already inside the history, and
re-sending it makes the planner run the same shell command again.
The unattended state is re-read at every round boundary, so
`scalpel-console-unattended' may arm it while a round is in
flight, and the time budget ends only the unattended state, not
the task.  Planner errors that are self-healable are retried
automatically: at most `scalpel-console-self-heal-max' retries per
instruction are allowed, counted both per error type and in total,
and the attempt counters reset with each invocation of this
function.  An unattended run also arms
`scalpel-agent-unattended-confirm', so no action confirms, and
stops at its own round limit with a timestamped mark instead of
the interactive round-limit notice.  Closing marks for an
unattended run name how long the run lasted, not only when it
ended.  `scalpel-console--busy' is set here and cleared at every
terminal point, so a second RET during a round is refused.  When
an operation ends, the cursor in the target buffer is also moved
to `point-max', signalling that the answer is finished.  The
completion acknowledgement is only shown for a successful final
round."
  (let ((operation (cl-incf scalpel-console--operation-generation)))
    (setq scalpel-console--busy t)
    (let ((target (scalpel-console--target-buffer))
          (round 0)
          (round-limit (if scalpel-console--unattended-p
                           (or scalpel-console--unattended-limit
                               scalpel-console-unattended-max-rounds)
                         scalpel-console-max-rounds))
          (conversation history)
          (next-instruction instruction)
          (self-heal-attempts nil)
          (self-heal-total 0))
      (cl-labels
          ((unattended-p () scalpel-console--unattended-p)
           (elapsed-text (start)
             (if (null start)
                 nil
               (let ((secs (round
                            (float-time
                             (time-subtract (current-time) start)))))
                 (if (< secs 60)
                     (format "%ds" secs)
                   (format "%dm%02ds" (/ secs 60) (% secs 60))))))
           (stop-unattended (reason)
             (let ((elapsed (elapsed-text
                             scalpel-console--unattended-start)))
               (setq scalpel-console--unattended-p nil
                     scalpel-console--unattended-deadline nil
                     scalpel-console--unattended-start nil
                     scalpel-agent-unattended-confirm nil)
               (scalpel-console--append
                (if elapsed
                    (format "Scalpel: Unattended stopped at %s \
after %s: %s."
                            (format-time-string "%H:%M") elapsed reason)
                  (format "Scalpel: Unattended stopped at %s: %s."
                          (format-time-string "%H:%M") reason)))))
           (finish-operation (&optional complete)
             (when (and (buffer-live-p target)
                        (= operation
                           (buffer-local-value
                            'scalpel-console--operation-generation target)))
               (with-current-buffer target
                 (setq scalpel-console--busy nil)
                 (cond
                  ((and (unattended-p) complete)
                   (let ((elapsed (elapsed-text
                                   scalpel-console--unattended-start)))
                     (setq scalpel-console--unattended-p nil
                           scalpel-console--unattended-deadline nil
                           scalpel-console--unattended-start nil
                           scalpel-agent-unattended-confirm nil)
                     (scalpel-console--append
                      (if elapsed
                          (format
                           (concat "Scalpel: Mission complete, over. "
                                   "(unattended, %s, ran %s)")
                           (format-time-string "%H:%M") elapsed)
                        (format
                         (concat "Scalpel: Mission complete, over. "
                                 "(unattended, %s)")
                         (format-time-string "%H:%M"))))))
                  (complete
                   (scalpel-console--append
                    "Scalpel: Mission complete, over."))
                  ((unattended-p)
                   (stop-unattended "error ended the run")))
                 (goto-char (point-max)))))
           (run-next ()
             (when (unattended-p)
               (unless scalpel-console--unattended-start
                 (setq scalpel-console--unattended-start (current-time)))
               (setq scalpel-agent-unattended-confirm t))
             (when (and scalpel-console--unattended-deadline
                        (time-less-p scalpel-console--unattended-deadline
                                     (current-time)))
               (let ((elapsed (elapsed-text
                               scalpel-console--unattended-start)))
                 (setq scalpel-console--unattended-p nil
                       scalpel-console--unattended-deadline nil
                       scalpel-console--unattended-start nil
                       scalpel-agent-unattended-confirm nil)
                 (scalpel-console--append
                  (if elapsed
                      (format
                       (concat "Scalpel: Unattended stopped at %s after "
                               "%s: time limit reached; continuing "
                               "attended.")
                       (format-time-string "%H:%M") elapsed)
                    (format
                     (concat "Scalpel: Unattended stopped at %s: time "
                             "limit reached; continuing attended.")
                     (format-time-string "%H:%M"))))))
             (setq round (1+ round))
             (scalpel-console--run-round
              next-instruction conversation
              (lambda (round-result)
                (when (and (buffer-live-p target)
                           (= operation
                              (buffer-local-value
                               'scalpel-console--operation-generation
                               target)))
                  (condition-case err
                      (with-current-buffer target
                        (setq conversation (scalpel-console--history))
                        (let ((round-error
                               (prog1 (buffer-local-value
                                       'scalpel-console--round-error
                                       target)
                                 (setq scalpel-console--round-error nil))))
                          (cond
                           ((and round-error
                                 (scalpel-diagnose-self-heal-p round-error)
                                 (< self-heal-total
                                    scalpel-console-self-heal-max)
                                 (< (or (cdr (assq (plist-get round-error :type)
                                                   self-heal-attempts))
                                        0)
                                    scalpel-console-self-heal-max))
                            (setq self-heal-total (1+ self-heal-total))
                            (let* ((etype (plist-get round-error :type))
                                   (count
                                    (1+ (or (cdr (assq etype
                                                       self-heal-attempts))
                                            0))))
                              (setq self-heal-attempts
                                    (cons (cons etype count)
                                          (assq-delete-all
                                           etype self-heal-attempts)))
                              (scalpel-console--append
                               (format
                                (concat
                                 "Scalpel: retrying after %s error "
                                 "(attempt %d/%d)")
                                etype count scalpel-console-self-heal-max))
                              (setq next-instruction
                                    scalpel-prompt--continuation-instruction)
                              (run-next)))
                           ((not
                             (and round-result
                                  (or (plist-get round-result :shells)
                                      (plist-get round-result :reads)
                                      (plist-get round-result :changes))))
                            (finish-operation (not (null round-result))))
                           ((>= round round-limit)
                            (if (unattended-p)
                                (progn
                                  (stop-unattended "round limit reached")
                                  (finish-operation))
                              (finish-operation)
                              (let ((notice
                                     (format
                                      (concat
                                       "Scalpel: round limit (%d) reached; "
                                       "send the next instruction when ready")
                                      round-limit)))
                                (scalpel-console--append notice)
                                (message "%s" notice))))
                           ((if (unattended-p)
                                t
                              (scalpel-console--continue-p round-result))
                            (setq next-instruction
                                  scalpel-prompt--continuation-instruction)
                            (run-next))
                           (t
                            (finish-operation t)))))
                    ((error quit)
                     (finish-operation)
                     (signal (car err) (cdr err)))))))))
        (run-next)))))

(defun scalpel-console--planner-error-p (err)
  "Return non-nil when ERR names a planner-output failure.
Such a round executed nothing and failed because the model's reply
did not follow the action contract, so retrying the same
instruction is the natural next step.  Delegates to
`scalpel-diagnose-planner-error-p', which owns the type list."
  (scalpel-diagnose-planner-error-p err))

(defun scalpel-console-repeat ()
  "Re-send the previous instruction without retyping it.
The instruction is inserted as pending input and sent through the
ordinary send path, so the conversation, the context and the busy
guard all behave exactly as if the user had typed it again."
  (interactive)
  (with-current-buffer (scalpel-console--target-buffer)
    (unless scalpel-console--last-instruction
      (user-error "Scalpel: no previous instruction to repeat"))
    (goto-char (point-max))
    (insert scalpel-console--last-instruction "\n")
    (scalpel-console-send-line)))

(defun scalpel-console-send-decide-for-me ()
  "Send a fixed prompt telling the model to decide for the user.
Interactive companion to `scalpel-console-repeat': the fixed prompt
lives in `scalpel-prompt--decide-for-me' on purpose, beside
the planner's other prompt text."
  (interactive)
  (with-current-buffer (scalpel-console--target-buffer)
    (goto-char (point-max))
    (insert (concat scalpel-prompt--decide-for-me "\n"))
    (scalpel-console-send-line)))

(defun scalpel-console-send-replan ()
  "Send a fixed prompt asking the model to propose a better plan.
The fixed prompt lives in `scalpel-prompt--replan' on
purpose, beside the planner's other prompt text."
  (interactive)
  (with-current-buffer (scalpel-console--target-buffer)
    (goto-char (point-max))
    (insert scalpel-prompt--replan "\n")
    (scalpel-console-send-line)))

(defun scalpel-console-send-why ()
  "Send a fixed prompt asking for the root cause.

The fixed prompt lives in `scalpel-prompt--why' on purpose, beside the
planner's other prompt text."
  (interactive)
  (with-current-buffer (scalpel-console--target-buffer)
    (goto-char (point-max))
    (insert scalpel-prompt--why "\n")
    (scalpel-console-send-line)))

(defun scalpel-console-send-summarize ()
  "Send a fixed prompt asking the model to summarize more concisely.
The fixed prompt lives in `scalpel-prompt--summarize' on
purpose, beside the planner's other prompt text."
  (interactive)
  (with-current-buffer (scalpel-console--target-buffer)
    (goto-char (point-max))
    (insert (concat scalpel-prompt--summarize "\n"))
    (scalpel-console-send-line)))

(defun scalpel-console-send-retry ()
  "Send a fixed prompt asking the model to analyze deeply and try again.
The fixed prompt lives in `scalpel-prompt--retry' on purpose, beside
the planner's other prompt text."
  (interactive)
  (with-current-buffer (scalpel-console--target-buffer)
    (goto-char (point-max))
    (insert (concat scalpel-prompt--retry "\n"))
    (scalpel-console-send-line)))

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
the agent can access its own earlier replies and shell output.
The typed input is rewritten into the logged user message, and an
acknowledgement line is inserted immediately after that logged
message, so the user sees both before the synchronous work of the
round starts; the acknowledgement is removed by the first round once
the status spinner takes over, so it never accumulates in the
conversation.
A send is refused while another console anchored to the same root
has a round in flight -- two consoles on one root serialize their
writes instead of interleaving them.  The gate reads only
buffer-local busy state, so a sibling that aborts or whose buffer
is killed releases its hold with no separate lock cleanup."
  (interactive)
  (if (scalpel-console--busy-p)
      (progn
        (message "Scalpel: still working on the previous instruction...")
        (ding))
    (let ((buf (scalpel-console--target-buffer)))
      (unless (eq (current-buffer) buf)
        (switch-to-buffer buf))
      ;; Prune context files that no longer exist on disk, so the agent
      ;; never trips over a stale 'File no longer exists' reference and
      ;; the send is never blocked by it.  This runs in the console
      ;; buffer, so the buffer-local context list is the one pruned.
      (let ((dropped (scalpel-agent-context-prune-missing)))
        (when dropped
          (scalpel-console--show-context)
          (message "Scalpel: pruned missing context file(s): %s"
                   (mapconcat #'identity dropped ", "))))
      (let ((sibling
             (cl-find-if
              (lambda (b)
                (and (buffer-live-p b)
                     (not (eq b buf))
                     (buffer-local-value 'scalpel-console--busy b)
                     (equal (buffer-local-value 'scalpel-console--root buf)
                            (buffer-local-value 'scalpel-console--root b))))
              (buffer-list))))
        (if sibling
            (progn
              (message "Scalpel: console %s is working on the same root %s; wait for it to settle or abort it there."
                       (buffer-name sibling)
                       (buffer-local-value 'scalpel-console--root buf))
              (ding))
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
                ;; Remember the instruction so `scalpel-console-repeat' can
                ;; resubmit it verbatim after a planner-output failure.
                (setq scalpel-console--last-instruction instr)
                ;; Rewrite the typed input into the logged user message, so the
                ;; instruction is not shown twice (once raw, once prefixed).
                ;; Regions are deleted back to front so positions stay valid.
                (let ((inhibit-read-only t))
                  (dolist (region (reverse regions))
                    (delete-region (car region) (cdr region)))
                  (goto-char (point-max))
                  (scalpel-console--insert-tagged
                   (format "User: %s\n" instr) 'user))
                ;; Display-only acknowledgement on the line after the logged
                ;; user message, so the user sees the round was accepted
                ;; before the spinner appears.  It carries
                ;; `scalpel-console-ack' and a dim `shadow' face, so it reads
                ;; as secondary feedback, and is deleted by the first round
                ;; once the status line replaces it.  The
                ;; `scalpel-console-output' property makes both
                ;; `scalpel-console--history' and
                ;; `scalpel-console--pending-input-regions' skip this line;
                ;; rear-nonsticky keeps later text from inheriting the face or
                ;; anything else into typed input.
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  ;; The marker must not advance on insert, so it stays at the
                  ;; start of the Roger line for --run-round to delete.
                  (setq scalpel-console--roger-marker (copy-marker (point)))
                  (insert (propertize "Scalpel: Roger. Working...\n"
                                      'face 'shadow
                                      'scalpel-console-ack t
                                      'scalpel-console-output t
                                      'rear-nonsticky '(scalpel-console-output face))))
                ;; Paint the logged user message and the acknowledgement now,
                ;; so they are visible before the synchronous work inside
                ;; --run-rounds (e.g. token counting) hogs the display.
                (redisplay)
                (scalpel-console--run-rounds instr history)
                (goto-char (point-max))
                (message "Scalpel: instruction sent.")))))))))
(defun scalpel-console-abort ()
  "Cancel the operation currently in flight for this console, if any.
This cancels the whole console operation -- not only the current
LLM request -- so a console busy with a synchronous action that has
no current LLM request can still be aborted.  The operation's
generation is advanced, so any callback from the aborted operation
is dropped instead of writing a report or continuing a round.
An unattended run is also disarmed here, with its own timestamped
mark, so the transcript records why the run ended.  The cancel
closure is per-console (buffer-local), read from this console's
target buffer.  The cursor is moved to the newest output so the
user can confirm the cancellation took hold and send the next
instruction."
  (interactive)
  (let ((target (scalpel-console--target-buffer)))
    (with-current-buffer target
      (if scalpel-console--busy
          (let ((cancel (prog1 (buffer-local-value 'scalpel-llm--cancel-current target)
                          (cl-incf scalpel-console--operation-generation))))
            (setq scalpel-console--busy nil)
            (when scalpel-console--unattended-p
              (setq scalpel-console--unattended-p nil
                    scalpel-agent-unattended-confirm nil)
              (scalpel-console--append
               (format "Scalpel: Unattended aborted at %s."
                       (format-time-string "%H:%M"))))
            (scalpel-console--append
             "Scalpel: Mission aborted, breaking off, out.")
            ;; Abort demands the user's attention, so jump to the
            ;; newest text rather than following.
            (goto-char (point-max))
            (when cancel
              (funcall cancel))
            (message "Scalpel: current operation aborted."))
        (ding)
        (message "Scalpel: no request in flight to cancel.")))))

(defun scalpel-console-unattended (&optional rounds)
  "Arm the current task to run unattended, callable at any time.
ROUNDS, from a prefix argument, overrides
`scalpel-console-unattended-max-rounds' for this run.  This
command arms the run at any point: when idle, it governs the
rounds the user sends next; when an operation is already running,
the in-flight loop picks the flags up at its next round boundary
\(and the confirm flag is re-armed immediately here), so the user
can leave mid-task.  A time budget also applies: when
`scalpel-console-unattended-max-minutes' elapse, unattended ends and
confirms ask again, but rounds keep running attended.  The run
confirms nothing, continues through noisy output, and stops at the
latest when the round limit is reached; it stops earlier when the
planner reports the task complete or the user calls
`scalpel-console-abort'.  The run's start time is recorded here so
the closing mark can say how long it lasted.  A timestamped mark
opens the run, and a mark naming the stop reason closes it, so the
console transcript shows where to start reading when the user
comes back.  Run this after sending the instruction it should
carry out: subsequent rounds continue from the callbacks of the
operation already in flight; this command itself sends no
request."
  (interactive "P")
  (let ((just-started (null scalpel-console--unattended-p)))
    (setq scalpel-console--unattended-p t
          scalpel-agent-unattended-confirm t
          scalpel-console--unattended-limit
          (or (and (numberp rounds) rounds)
              scalpel-console-unattended-max-rounds))
    (setq-local scalpel-console--unattended-deadline
                (time-add (current-time)
                          (* 60 scalpel-console-unattended-max-minutes)))
    (when just-started
      (setq scalpel-console--unattended-start (current-time))))
  (scalpel-console--append
   (if (scalpel-console--busy-p)
       (format "Scalpel: Unattended armed at %s. Takes over after the current round; auto-stop after %d rounds or %d minutes."
               (format-time-string "%H:%M")
               scalpel-console--unattended-limit
               scalpel-console-unattended-max-minutes)
     (format "Scalpel: Unattended begin at %s. Auto-stop after %d rounds."
             (format-time-string "%H:%M")
             scalpel-console--unattended-limit))))

(defun scalpel-console-unload-function ()
  "Suppress `unload-feature's default cleanup for this module.
The default cleanup kills every buffer whose major mode is defined
here, which would destroy live console sessions; a reload that
follows redefines every function and variable anyway, so the
cleanup buys nothing and costs the session.  Returning non-nil
tells `unload-feature' to skip its default work.  Hooks registered
by this module are none, so nothing needs manual removal; revisit
this if one is added."
  t)

(provide 'scalpel-console)

;;; scalpel-console.el ends here
