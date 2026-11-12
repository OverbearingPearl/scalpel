;;; scalpel-proc.el --- Subprocess execution with deadlines and diagnostics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Every external program Scalpel runs goes through `scalpel-proc-run'.
;;
;; The helper exists because `call-process' is unusable on this path.
;; While it waits, Emacs services neither timers nor the window-system
;; event queue, so a child that never exits leaves the whole
;; application in the state the window server calls "not responding":
;; the frame cannot be raised, C-g is never delivered, and the only way
;; out is a forced quit.  `scalpel-proc-run' starts the child with
;; `make-process' and waits with `accept-process-output' instead, which
;; keeps the event loop alive, so the interface stays usable and C-g
;; works; it enforces a wall-clock deadline and kills the child that
;; overruns it; and it caps the bytes a child may deliver, because an
;; uncapped stream into a buffer is the second way a child freezes
;; Emacs, through memory exhaustion rather than through waiting.
;;
;; Every run is journalled to `scalpel-proc-log-file' in paired lines:
;; one "spawn" line before the child starts, one "exit", "timeout" or
;; "capped" line after it ends.  An unmatched spawn line therefore names
;; the culprit by itself, even when Emacs is force-killed before the
;; terminal line can be written -- the same argument that makes the
;; gptel advice pairs useful.  A timeout additionally writes a "verdict"
;; line attributing the stall, a "bundle" line naming a directory of
;; diagnostics captured while the child was still alive, and "advice"
;; lines holding the commands that capture the same evidence by hand on
;; the next occurrence, so a freeze becomes an investigation instead of
;; a lost session.
;;
;; Two details keep the wait from turning into the freeze it exists to
;; prevent.  Input is written in chunks with the child's output drained
;; between them, because a pipe holds about 64 kilobytes and a single
;; `process-send-string' of a whole file blocks inside the send, where no
;; deadline is checked.  And a run that was killed instead of answering
;; reports the symbols `timeout' or `capped' rather than an exit status,
;; which callers turn into the conditions `scalpel-proc-timeout-error'
;; and `scalpel-proc-oversized-error' through
;; `scalpel-proc-check-result', so the console attributes the failure to
;; the command instead of reporting a Scalpel bug.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom scalpel-proc-timeout 30
  "Seconds a child process may run before `scalpel-proc-run' kills it.
Callers that know their program is slower, such as a shell command or
a whole-file perl rewrite, pass a larger value in :timeout."
  :type 'number
  :group 'scalpel)

(defcustom scalpel-proc-max-output-bytes (* 4 1024 1024)
  "Bytes of child output `scalpel-proc-run' will collect.
A child that exceeds the cap is killed and reported with status
`capped'.  The cap exists because collecting an unbounded stream into
a buffer exhausts memory and takes the whole Emacs down with it; four
megabytes is far above what any Scalpel tool reports after truncation."
  :type 'integer
  :group 'scalpel)

(defcustom scalpel-proc-diagnostics-directory
  (expand-file-name "scalpel-diag" user-emacs-directory)
  "Directory holding the diagnostics captured for a timed-out child.
One subdirectory is created per timeout, named after the UTC time and
the child's process id."
  :type 'directory
  :group 'scalpel)

(defconst scalpel-proc-log-file
  (expand-file-name "scalpel-freeze.log" user-emacs-directory)
  "Journal file of every child process Scalpel starts.
Shared with the gptel advice timeline so one file tells the whole
story: the request, the reply, and the commands that ran afterwards.")

(defvar scalpel-proc--counter 0
  "Number of `scalpel-proc-run' calls in this session.
Used as the run identifier in the journal, so the spawn line and the
terminal line of one run can be paired by eye and by grep.")

(defun scalpel-proc--timestamp ()
  "Return the current UTC time as a sortable ISO 8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t))

(defun scalpel-proc-log (format &rest args)
  "Append one timestamped line to `scalpel-proc-log-file'.
FORMAT and ARGS follow `format'.  Every write reaches the disk
immediately so the journal survives a freeze or a forced quit.
Degradation point: all errors are swallowed on purpose, because
journaling must never change what the caller observes."
  (condition-case nil
      (with-temp-buffer
        (insert (scalpel-proc--timestamp))
        (insert " ")
        (insert (apply #'format format args))
        (insert "\n")
        (write-region (point-min) (point-max)
                      scalpel-proc-log-file 'append 'silent))
    (error nil)))

(defconst scalpel-proc-arg-limit 200
  "Characters of one argument kept in a journal line.
The sandbox passes its whole policy as one argument, which is several
kilobytes and several lines long; the journal records one event per
line, so an argument that long must be shortened there.  The complete
command is written to the diagnostics bundle of a timeout instead.")

(defun scalpel-proc--flatten (text)
  "Return TEXT with every whitespace run collapsed to one space.
Embedded newlines would otherwise split one journal event across lines
and make the file impossible to grep."
  (replace-regexp-in-string "[\t\n\r ]+" " " text))

(defun scalpel-proc--argv-string (program args)
  "Return PROGRAM and ARGS as one single-line string for the journal.
Whitespace is flattened and each argument is shortened to
`scalpel-proc-arg-limit' characters, so a sandboxed command's policy
does not bury the event it belongs to; the untruncated command is
written to the diagnostics bundle."
  (string-join
   (mapcar (lambda (arg)
             (let ((flat (scalpel-proc--flatten arg)))
               (if (> (length flat) scalpel-proc-arg-limit)
                   (concat (substring flat 0 scalpel-proc-arg-limit)
                           (format "...<%d chars>" (length flat)))
                 flat)))
           (cons program args))
   " "))

(defun scalpel-proc--argv-full (program args)
  "Return PROGRAM and ARGS exactly, with only whitespace flattened.
Used for the diagnostics bundle, where the untruncated command is the
evidence a reader needs."
  (string-join (mapcar #'scalpel-proc--flatten (cons program args)) " "))

(defun scalpel-proc--capture-quiet (program args target)
  "Run PROGRAM with ARGS and append its output to buffer TARGET.
The capture is best effort: a missing program, a failing one, or one
that outlives the three-second cap leaves TARGET holding whatever it
managed to deliver.  Used only for diagnostics, so it journals
nothing and captures no diagnostics of its own."
  (when (executable-find program)
    (let ((buffer (generate-new-buffer " *scalpel-proc-capture*")))
      (unwind-protect
          (progn
            (condition-case nil
                (let ((proc (make-process
                             :name "scalpel-proc-capture"
                             :buffer buffer
                             :command (cons program args)
                             :noquery t))
                      (deadline (+ (float-time) 3)))
                  (while (and (process-live-p proc) (< (float-time) deadline))
                    (accept-process-output proc 0.05))
                  (when (process-live-p proc)
                    (delete-process proc)))
              (error nil))
            (with-current-buffer target
              (goto-char (point-max))
              (insert (format "\n$ %s\n" (scalpel-proc--argv-full program args)))
              (insert-buffer-substring buffer)))
        (kill-buffer buffer)))))

(defun scalpel-proc--capture-diagnostics (pid program args run-id)
  "Capture what a stuck child PID is doing, and return the bundle path.
PROGRAM and ARGS record how the child was started; RUN-ID names the run
in the journal.  The capture happens before the caller kills the child,
which is the whole point: a forced quit destroys this evidence and a
post-mortem cannot recover it.  Return the bundle directory, or nil when
it could not be created."
  (condition-case nil
      (let* ((stamp (format-time-string "%Y%m%dT%H%M%SZ" nil t))
             (dir (expand-file-name (format "%s-%s" stamp pid)
                                    scalpel-proc-diagnostics-directory))
             (summary (generate-new-buffer " *scalpel-proc-diag*")))
        (make-directory dir t)
        (unwind-protect
            (progn
              (with-current-buffer summary
                (insert (format "run      : #%d\n" run-id))
                (insert (format "pid      : %s\n" pid))
                (insert (format "command  : %s\n"
                                (scalpel-proc--argv-full program args)))
                (insert (format "captured : %s\n" (scalpel-proc--timestamp))))
              (if (eq system-type 'darwin)
                  (progn
                    (scalpel-proc--capture-quiet
                     "ps" (list "-o" "pid,ppid,stat,etime,time,command"
                                "-p" (format "%s" pid))
                     summary)
                    (scalpel-proc--capture-quiet
                     "pgrep" (list "-P" (format "%s" pid)) summary)
                    (scalpel-proc--capture-quiet
                     "lsof" (list "-p" (format "%s" pid)) summary)
                    (let ((sample-file (expand-file-name "sample.txt" dir)))
                      (scalpel-proc--capture-quiet
                       "sample" (list (format "%s" pid) "2" "-file" sample-file)
                       summary)
                      (when (file-exists-p sample-file)
                        (with-current-buffer summary
                          (insert (format "\nsample   : %s\n" sample-file))))))
                (scalpel-proc--capture-quiet
                 "ps" (list "-o" "pid,ppid,stat,etime,time,args"
                            "-p" (format "%s" pid))
                 summary)
                (scalpel-proc--capture-quiet
                 "pgrep" (list "-P" (format "%s" pid)) summary)
                (scalpel-proc--capture-quiet
                 "lsof" (list "-p" (format "%s" pid)) summary)
                (scalpel-proc--capture-quiet
                 "cat" (list (format "/proc/%s/stack" pid)) summary))
              (with-current-buffer summary
                (write-region (point-min) (point-max)
                              (expand-file-name "summary.txt" dir)
                              nil 'silent))
              dir)
          (kill-buffer summary)))
    (error nil)))

(defun scalpel-proc--verdict (bytes last-output start kill-time)
  "Return a one-line attribution for a child that missed its deadline.
BYTES is what the child delivered, LAST-OUTPUT the time of its last
chunk or nil, START when it was spawned and KILL-TIME when the deadline
fired.  The three shapes separate the cases a stall can have: a child
that never produced anything is waiting on its environment, one that
fell silent well before the deadline is blocked on a call of its own,
and one still delivering at the deadline is doing unbounded work."
  (let ((runtime (- kill-time start))
        (silent-for (and last-output (- kill-time last-output))))
    (cond
     ((and (= bytes 0) (null last-output))
      (concat "no output at all before the deadline, so the child never "
              "got to work: it is waiting on its environment (sandbox "
              "grant, permission prompt, network, lock file, or a GUI it "
              "cannot reach).  Blame the environment, not the command."))
     ((and silent-for (> silent-for (/ runtime 2.0)))
      (format (concat "%d bytes arrived, then nothing for the last %.1fs "
                      "of a %.1fs run: the child started, then blocked in a "
                      "call of its own (stdin, socket, lock, or a grandchild "
                      "it waits on).  Blame the child program.")
              bytes silent-for runtime))
     (t
      (format (concat "%d bytes and still delivering at the deadline: the "
                      "child is doing unbounded work, a runaway loop or a "
                      "catastrophic pattern.  Blame the command the planner "
                      "wrote.")
              bytes)))))

(defun scalpel-proc--advice-lines (pid bundle program args)
  "Return the journal advice lines for a timed-out child PID.
BUNDLE is the captured diagnostics directory, or nil; PROGRAM and ARGS
name the command so the advice can quote it.  Each line is one shell
command the reader can run by hand, either against the bundle just
captured or against the next occurrence while it is still stuck."
  (let ((pid (format "%s" pid))
        (replay (scalpel-proc--argv-string program args)))
    (delq nil
          (list
           (and bundle
                (format "advice #: this run was captured before the kill; read %s/summary.txt first"
                        bundle))
           (format "advice #: reproduce the stall outside Emacs, where C-g is not needed:  %s"
                   replay)
           (format "advice #: while it is still stuck, capture its native stack:  sample %s 3 -file /tmp/scalpel-sample.txt && open -R /tmp/scalpel-sample.txt"
                   pid)
           (format "advice #: see the files and sockets it holds:  lsof -p %s | head -50"
                   pid)
           (format "advice #: find the real offender behind a shell wrapper:  pgrep -P %s ; ps -o pid,ppid,stat,etime,command -p %s"
                   pid pid)
           (format "advice #: if the stack shows a wait on the window server or on mach, the command needed a GUI session and must not run here")
           (format "advice #: if it shows a lock or a socket wait, name the peer with:  lsof -p %s | grep -E 'TCP|REG'"
                   pid)))))

(defun scalpel-proc--terminate (proc status)
  "Kill PROC and return the exit status to report for STATUS.
STATUS is `timeout' or `capped'; a child that already exited keeps its
own status."
  (if (process-live-p proc)
      (progn
        (condition-case nil
            (delete-process proc)
          (error nil))
        status)
    (or status (process-exit-status proc))))

(defconst scalpel-proc-input-chunk 32768
  "Characters written to a child's stdin between output drains.
A pipe holds about 64 kilobytes.  Writing a whole file into it with one
`process-send-string' blocks once the pipe is full, and it blocks
*inside* the send, where no deadline is checked and no output is
drained, so a child that writes while still reading deadlocks the
session.  Sending in chunks and draining between them keeps the wait
interruptible.")

(defun scalpel-proc--send-input (proc text deadline)
  "Send TEXT to PROC in chunks, draining output between them.
Return non-nil when everything was sent before DEADLINE, nil when the
child stopped accepting input in time or died."
  (let ((pos 0)
        (length (length text))
        (ok t))
    (while (and ok (< pos length))
      (let ((end (min length (+ pos scalpel-proc-input-chunk))))
        (condition-case nil
            (process-send-string proc (substring text pos end))
          (error (setq ok nil)))
        (setq pos end)
        ;; Drain whatever the child already wrote, so its output pipe
        ;; cannot fill and leave it blocked while it still owes us a
        ;; read of the input pipe.
        (while (and ok (accept-process-output proc 0)))
        (when (and ok deadline (> (float-time) deadline))
          (setq ok nil))))
    ok))

(cl-defun scalpel-proc-run (program args &key input timeout label)
  "Run PROGRAM with ARGS, waiting without blocking Emacs's event loop.
INPUT, when non-nil, is the text written to the child's stdin before
end-of-file is sent.  TIMEOUT is the wall-clock deadline in seconds and
defaults to `scalpel-proc-timeout'; LABEL names the run in the journal
and defaults to PROGRAM.

Return a plist describing the run: :status is the integer exit status,
or the symbol `timeout' when the deadline fired, or `capped' when
`scalpel-proc-max-output-bytes' was exceeded; :status-symbol is what
`process-status' reported, so `signal' separates a child killed by a
signal from one that ran and returned a status; :output is the
collected text, never longer than that cap; :bytes is everything the
child delivered, including bytes dropped by the cap; :elapsed and :pid
describe the run; :label and :command identify it.

The wait uses `accept-process-output', so timers, process filters and
the window-system event queue keep running and \\[keyboard-quit] stays deliverable:
a child that hangs costs a deadline, not the session.  Each run writes
a spawn line and a terminal line to `scalpel-proc-log-file'; a timeout
additionally writes the verdict, the diagnostics bundle and the
handler's advice, so one grep answers which program stalled and what to
run next time."
  (let* ((label (or label program))
         (budget (or timeout scalpel-proc-timeout))
         (buffer (generate-new-buffer " *scalpel-proc*"))
         (start (float-time))
         (deadline (+ start budget))
         (run-id (setq scalpel-proc--counter (1+ scalpel-proc--counter)))
         (total 0)
         (kept 0)
         (last-output nil)
         (capped nil)
         (done nil)
         (exit-status nil)
         (term-status nil)
         (pid nil)
         proc elapsed status)
    (unwind-protect
        (progn
          (scalpel-proc-log "proc spawn #%d label=%s timeout=%ss id=%s argv=[%s]"
                            run-id label budget
                            (emacs-pid) (scalpel-proc--argv-string program args))
          (setq proc
                (make-process
                 :name (format "scalpel-proc-%d" run-id)
                 :command (cons program args)
                 ;; No process buffer: output reaches the filter below, and
                 ;; a per-run buffer would join the buffer list, fire
                 ;; `buffer-list-update-hook' for every command and outlive
                 ;; the run.
                 :buffer nil
                 :connection-type 'pipe
                 :noquery t
                 :filter
                 (lambda (_proc chunk)
                   (setq last-output (float-time))
                   (setq total (+ total (length chunk)))
                   (if (or capped
                           (> (+ kept (length chunk))
                              scalpel-proc-max-output-bytes))
                       (setq capped t)
                     (with-current-buffer buffer
                       (goto-char (point-max))
                       (insert chunk))
                     (setq kept (+ kept (length chunk)))))
                 :sentinel
                 (lambda (proc _event)
                   ;; Record only; the terminal journal line below is the
                   ;; single place a run reports its outcome, so a run
                   ;; always reads as one spawn line and one terminal line.
                   (unless (process-live-p proc)
                     (setq done t)
                     (setq term-status (process-status proc))
                     (setq exit-status (process-exit-status proc))))))
          (setq pid (process-id proc))
          (when input
            (scalpel-proc--send-input proc input deadline)
            (condition-case nil
                (process-send-eof proc)
              (error nil)))
          (while (and (not done) (not capped) (< (float-time) deadline))
            (accept-process-output proc 0.05))
          (setq elapsed (- (float-time) start))
          (cond
           ;; The cap is checked first: a child that exited on its own
           ;; after overrunning it still handed us only a prefix, and a
           ;; caller reading that prefix as the whole output would act on
           ;; truncated data.
           (capped
            (scalpel-proc--terminate proc nil)
            (setq status 'capped)
            (scalpel-proc-log
             "proc capped #%d pid=%s elapsed=%.3fs bytes=%d limit=%d"
             run-id pid elapsed total scalpel-proc-max-output-bytes)
            (scalpel-proc-log
             "proc verdict #%d blame=command reason=\"the child exceeded the byte cap, so it is producing unbounded output rather than stalling\"" run-id)
            (scalpel-proc-log
             "proc advice #%d narrow the command: pipe it through head, add a path filter, or ask for a single file instead of a tree" run-id))
           (done
            (setq status exit-status)
            (scalpel-proc-log
             "proc exit #%d pid=%s status=%s status-symbol=%s elapsed=%.3fs bytes=%d"
             run-id pid status term-status elapsed total))
           (t
            (let* ((kill-time (float-time))
                   (bundle (scalpel-proc--capture-diagnostics
                            pid program args run-id)))
              (scalpel-proc--terminate proc nil)
              (setq status 'timeout)
              (scalpel-proc-log
               "proc timeout #%d pid=%s elapsed=%.3fs bytes=%d silent-for=%s"
               run-id pid elapsed total
               (if last-output
                   (format "%.3fs" (- kill-time last-output))
                 "never"))
              (scalpel-proc-log "proc verdict #%d blame=%s reason=\"%s\""
                                run-id
                                (cond ((and (= total 0) (null last-output))
                                       "environment")
                                      ((and last-output
                                            (> (- kill-time last-output)
                                               (/ (- kill-time start) 2.0)))
                                       "child")
                                      (t "command"))
                                (scalpel-proc--verdict
                                 total last-output start kill-time))
              (when bundle
                (scalpel-proc-log "proc bundle #%d dir=%s" run-id bundle))
              (dolist (line (scalpel-proc--advice-lines
                             pid bundle program args))
                (scalpel-proc-log "%s #%d" line run-id)))))
          (list :status status
                :status-symbol term-status
                :output (with-current-buffer buffer (buffer-string))
                :bytes total
                :elapsed elapsed
                :pid pid
                :label label
                :command (cons program args)))
      (when (process-live-p proc)
        (condition-case nil
            (delete-process proc)
          (error nil)))
      (kill-buffer buffer))))

(defun scalpel-proc-timeout-p (result)
  "Return non-nil when RESULT, a `scalpel-proc-run' plist, timed out.
A timed-out run has no exit status to report, so callers must tell it
apart from a program that ran and failed."
  (eq (plist-get result :status) 'timeout))

(defun scalpel-proc-capped-p (result)
  "Return non-nil when RESULT exceeded the output cap.
The child was killed for producing too much, which is a different
diagnosis from a stall and needs a different remedy."
  (eq (plist-get result :status) 'capped))

(define-error 'scalpel-proc-timeout-error
  "Scalpel: child process exceeded its deadline")

(define-error 'scalpel-proc-oversized-error
  "Scalpel: child process exceeded the output cap")

(defun scalpel-proc-check-result (result)
  "Return RESULT, or signal a condition naming why the run failed.
A `timeout' or `capped' run has no exit status to compare, so a caller
that read one as a command failure would attribute Scalpel's own
decision to the command, and the console would render it as a bug.
Signalling distinct conditions lets the caller report the failure as
the planner's to fix.  A plist with an exit status, whatever it is, is
returned unchanged."
  (cond
   ((scalpel-proc-timeout-p result)
    (signal 'scalpel-proc-timeout-error
            (list (scalpel-proc-timeout-message result))))
   ((scalpel-proc-capped-p result)
    (signal 'scalpel-proc-oversized-error
            (list (format (concat "Scalpel: the command produced %d bytes, "
                                  "over the %d-byte cap, so it was killed; "
                                  "narrow it (a path filter, head, or a single "
                                  "file) and retry")
                          (or (plist-get result :bytes) 0)
                          scalpel-proc-max-output-bytes))))
   (t result)))

(defun scalpel-proc-timeout-message (result)
  "Return the user-facing text describing timed-out RESULT.
The message names the program, the deadline and the journal, so the
console error itself points at the investigation instead of only
reporting that something is wrong."
  (format (concat "Scalpel: %s produced no exit status within %.0fs (%d bytes captured); "
                  "it was killed so Emacs stays responsive. "
                  "Diagnosis and the commands to run next time are in %s -- "
                  "grep for \"proc timeout\".")
          (or (plist-get result :label) "the child process")
          (or (plist-get result :elapsed) 0)
          (or (plist-get result :bytes) 0)
          scalpel-proc-log-file))

(provide 'scalpel-proc)

;;; scalpel-proc.el ends here
