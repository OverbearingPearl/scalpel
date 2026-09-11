;;; scalpel-console-test.el --- Tests for scalpel-console -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-console.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-console)
(require 'scalpel-agent)

(defun scalpel-console-test--new-console-buffer ()
  "Create a fresh console buffer anchored to the temp directory.
The temp directory is the value of the variable
`temporary-file-directory'.  The buffer is put into
`scalpel-console-mode' with `scalpel-console--root' and
`default-directory' pinned to the same directory.  Any pre-existing
buffer of that name is killed first.  The caller must kill the
returned buffer when done."
  (let* ((root (file-name-as-directory
                (expand-file-name temporary-file-directory)))
         (name (scalpel-console--buffer-name root)))
    (when (get-buffer name) (kill-buffer name))
    (let ((buf (get-buffer-create name)))
      (with-current-buffer buf
        (erase-buffer)
        (scalpel-console-mode)
        (setq-local scalpel-console--root root)
        (setq-local default-directory root))
      buf)))

(ert-deftest scalpel-console-test-sends-text-typed-through-keyboard ()
  "Keyboard-typed input is sent even when it follows display output.
Regression: `self-insert-command' inserts through `insert-and-inherit',
which copies the text properties of the preceding character.  Console
output carried `scalpel-console-output' but nothing marked it
rear-nonsticky, so the first character the user typed inherited the
tag; `--pending-input-regions' then skipped the whole instruction and
RET answered \"nothing to send\"."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        (prompt-sent nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (prompt &optional _system)
                       (setq prompt-sent prompt)
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (with-current-buffer buf
              (erase-buffer)
              ;; Output written by the same path the console uses, so the
              ;; text carries exactly the tags `--append' really sets.
              (scalpel-console--append "Scalpel console.")
              (scalpel-console--append "Context: none")
              ;; Insert through the keyboard path, not plain `insert':
              ;; only `insert-and-inherit' copies the surrounding tags.
              (goto-char (point-max))
              (insert-and-inherit "hello")
              (scalpel-console-send-line))
            (ert-info ((format "Prompt:\n%S" prompt-sent))
              (should prompt-sent)
              (should (string-suffix-p "User instruction:\nhello"
                                       prompt-sent)))
            (with-current-buffer buf
              (ert-info ((format "Buffer:\n%S" (buffer-string)))
                (should (string-match-p "User: hello" (buffer-string)))
                (should (string-match-p "Scalpel: done" (buffer-string)))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-send-line ()
  "Send a line to the agent and verify the reply is appended."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (_prompt &optional _system)
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (with-current-buffer buf
              (erase-buffer)
              (insert "test instruction")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "User: test instruction" nil t))
              (ert-info ((format "Console contents:\n%S" (buffer-string)))
                (should (search-forward "Scalpel: done" nil t)))
              (should-not (search-forward "thinking" nil t))
              (should (string= (buffer-string)
                               "User: test instruction\nScalpel: done\n\n")))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-send-line-error ()
  "When the agent errors, the error message is appended to the console."
  (let ((buf (scalpel-console-test--new-console-buffer)))
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
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-send-line-rejected-while-busy ()
  "A new instruction is rejected while a request is in flight."
  (let ((buf (scalpel-console-test--new-console-buffer)))
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
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-progress-callback-bound-during-request ()
  "The progress callback must be bound while the agent request runs."
  (let ((buf (scalpel-console-test--new-console-buffer))
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
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-open-shows-context ()
  "Opening the console shows a Context line and does not repeat it on send."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())"))
    (let ((scalpel-agent--context-files (list this-file))
          (buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'scalpel-agent-context-reset)
                     (lambda () nil))
                    ((symbol-function 'scalpel-llm-request)
                     (lambda (_p &optional _s)
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (scalpel-console-open)
            (setq buf (current-buffer))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "Context:" nil t))
              (should (search-forward (file-name-nondirectory this-file)
                                      nil t)))
            (with-current-buffer buf
              (goto-char (point-max))
              (insert "hello")
              (scalpel-console-send-line))
            (with-current-buffer buf
              (goto-char (point-min))
              (should (search-forward "Scalpel: done" nil t))
              (should (= (how-many "Context:" (point-min) (point-max))
                         1))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest scalpel-console-test-add-file-updates-context-line ()
  "Adding a file appends an updated Context line."
  (let ((scalpel-agent--context-files nil))
    (scalpel-utils-test-with-temp-file ".el"
      (with-temp-file this-file (insert "(defun foo ())"))
      (let ((buf nil))
        (unwind-protect
            (cl-letf (((symbol-function 'scalpel-agent-context-reset)
                       (lambda () nil))
                      ((symbol-function 'read-file-name)
                       (lambda (&rest _) this-file)))
              (scalpel-console-open)
              (setq buf (current-buffer))
              (with-current-buffer buf
                (scalpel-console-add-file))
              (with-current-buffer buf
                ;; `scalpel-console--append' leaves point at point-max;
                ;; pin the search start instead of relying on it.
                (goto-char (point-min))
                (should (search-forward "Context:" nil t))
                (should (search-forward (file-name-nondirectory this-file)
                                        nil t))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest scalpel-console-test-context-diff-face-covers-name-only ()
  "The change face starts at the file name, never at the tree graphics."
  (let ((scalpel-agent--context-files '("/tmp/scalpel-diff-name.el"))
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq scalpel-console--context-baseline 'none-yet)
            (scalpel-console--show-context)
            (setq scalpel-agent--context-files nil)
            (scalpel-console--show-context)
            (goto-char (point-min))
            (let (pos)
              (while (search-forward "scalpel-diff-name.el" nil t)
                (setq pos (match-beginning 0)))
              (should pos)
              (should (eq (get-text-property pos 'face)
                          'scalpel-console-context-removed-face))
              (should (null (get-text-property (1- pos) 'face))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-status-line-own-line-and-clean-stop ()
  "Status line sits on its own line; STOP leaves no residue.
Regression: `beg' was derived by subtracting a hard-coded length,
so non-zero counters made STOP delete a character inside
\"Scalpel:\" instead of the whole line."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (scalpel-llm--tokens-uploaded 12)
        (scalpel-llm--tokens-received 34))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "User: hi\n")
          (goto-char (point-max))
          (let* ((status (scalpel-console--status-start))
                 (refresh (car status))
                 (stop (cdr status)))
            (should (= (point) (point-max)))
            (should (eq (char-before) ?\n))
            (save-excursion
              (goto-char (point-min))
              (should (search-forward
                       "Scalpel: 12 up, 34 down, 0s\n" nil t)))
            ;; Refreshing rewrites the same single line.
            (funcall refresh)
            (funcall refresh)
            (should (= (how-many "^Scalpel:" (point-min) (point-max)) 1))
            ;; STOP removes the whole line, leaving no residue.
            (funcall stop)
            (should (string= (buffer-string) "User: hi\n"))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-open-is-not-a-command ()
  "Only `scalpel-open' is the user entry; `scalpel-console-open' is internal."
  (should-not (commandp 'scalpel-console-open)))

(ert-deftest scalpel-console-test-send-line-fails-loudly-without-root ()
  "A console buffer entered without `scalpel-console-open' fails loudly.
The mode function stays reachable via \\[execute-extended-command],
since major modes are commands, so a rootless buffer must signal
instead of silently anchoring agent side effects to the current
directory."
  (with-temp-buffer
    (scalpel-console-mode)
    (should (null scalpel-console--root))
    (ert-info ("Rootless console must refuse to send")
      (should-error (scalpel-console-send-line) :type 'user-error))))

(ert-deftest scalpel-console-test-context-diff-unchanged-face ()
  "Unchanged context entries are dimmed without strike-through."
  (let ((scalpel-agent--context-files '("/tmp/scalpel-diff-keep.el"))
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq scalpel-console--context-baseline 'none-yet)
            ;; First refresh establishes the baseline; second sees no delta.
            (scalpel-console--show-context)
            (scalpel-console--show-context)
            (goto-char (point-min))
            (let (pos)
              (while (search-forward "scalpel-diff-keep.el" nil t)
                (setq pos (match-beginning 0)))
              (should pos)
              (ert-info ((format "Face at %d: %S" pos
                                 (get-text-property pos 'face)))
                (should (eq (get-text-property pos 'face)
                            'scalpel-console-context-unchanged-face))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-reset-context-leaves-point-at-max ()
  "Context commands leave point at the buffer end, not on the context block.
Regression: `scalpel-console--append' restored point via
`save-excursion', so after a refresh the cursor sat just before the
newly inserted Context block."
  (let ((scalpel-agent--context-files '("/tmp/scalpel-reset-cursor.el"))
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (setq scalpel-console--context-baseline 'none-yet)
          (scalpel-console-reset-context)
          (ert-info ((format "Point %d of %d; buffer:\n%S"
                             (point) (point-max) (buffer-string)))
            (should (= (point) (point-max)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-shift-return-inserts-newline ()
  "S-RET is bound to a newline insertion, not to sending."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'scalpel-llm-request)
                   (lambda (&rest _)
                     (error "S-RET must not send the instruction"))))
          (with-current-buffer buf
            (erase-buffer)
            (insert "line one")
            (goto-char (point-max))
            (let ((cmd (key-binding (kbd "S-<return>"))))
              (ert-info ((format "S-<return> resolves to: %S" cmd))
                (should (eq cmd #'newline)))
              (call-interactively cmd))
            (insert "line two")
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (string= (buffer-string) "line one\nline two")))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-send-line-sends-multiline-block ()
  "RET sends every pending line, not only the line point is on."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        prompt-sent)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (prompt &optional _system)
                       (setq prompt-sent prompt)
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (with-current-buffer buf
              (erase-buffer)
              (insert "line one\nline two\n")
              ;; Point on the first line: the whole block must still be sent.
              (goto-char (point-min))
              (scalpel-console-send-line))
            (with-current-buffer buf
              (ert-info ((format "Buffer:\n%S" (buffer-string)))
                (should (string= (buffer-string)
                                 "User: line one\nline two\nScalpel: done\n\n")))))
          (ert-info ((format "Prompt sent to the LLM:\n%S" prompt-sent))
            (should (string-suffix-p "User instruction:\nline one\nline two"
                                     prompt-sent))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-send-line-ignores-previous-output ()
  "Text appended by earlier turns is never re-sent as the instruction."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'scalpel-llm-request)
                   (lambda (&rest _)
                     "[{\"tool\":\"reply\",\"text\":\"ack\"}]")))
          (with-current-buffer buf
            (erase-buffer)
            (scalpel-console--insert-tagged "User: old\n" 'user)
            (scalpel-console--insert-tagged
             "Scalpel: old reply\n\n" 'assistant)
            (insert "new instruction")
            (goto-char (point-max))
            (scalpel-console-send-line)
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (string= (buffer-string)
                               (concat "User: old\nScalpel: old reply\n\n"
                                       "User: new instruction\n"
                                       "Scalpel: ack\n\n"))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-history-keeps-only-conversation ()
  "Display-only output never reaches the LLM.
Regression: the buffer holds context trees and status lines next to
the conversation, so dumping the buffer would send all of it."
  (let ((scalpel-agent--context-files '("/tmp/scalpel-history.el"))
        (scalpel-agent--context-readonly-files nil)
        (scalpel-console--context-baseline 'none-yet)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "Scalpel console.\n\n")
          (scalpel-console--insert-tagged "User: hello\n" 'user)
          (scalpel-console--show-context)
          (ert-info ((format "Buffer:\n%S" (buffer-string)))
            (should (string= (scalpel-console--history) "User: hello"))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-send-line-sends-recorded-conversation ()
  "Each round re-sends the replies recorded in the buffer."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (prompt &optional _system)
                       (push prompt prompts)
                       "[{\"tool\":\"reply\",\"text\":\"first reply\"}]")))
            (with-current-buffer buf
              (erase-buffer)
              (insert "first instruction\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (with-current-buffer buf
              (goto-char (point-max))
              (insert "second instruction\n")
              (scalpel-console-send-line)))
          (ert-info ((format "Prompts:\n%S" prompts))
            (should (= (length prompts) 2))
            ;; `prompts' is pushed, so its head is the second request.
            ;; The newest instruction is framed under "User instruction:";
            ;; "User: " belongs to the recorded history only.
            (should (string-match-p "Conversation so far:\nUser: first instruction"
                                    (car prompts)))
            (should (string-match-p "Scalpel: first reply" (car prompts)))
            (should (string-suffix-p "User instruction:\nsecond instruction"
                                     (car prompts)))
            (should-not (string-match-p "Conversation so far:"
                                        (cadr prompts)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-send-line-continues-after-shell ()
  "A round that ran a shell command is followed by another round.
Regression: the agent had no way to read the output of the command
it asked for, so \"run the tests\" could not lead to a fix."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'always)
        (scalpel-console-max-rounds 30)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (prompt &optional _system)
                       (push prompt prompts)
                       (if (= (length prompts) 1)
                           "[{\"tool\":\"shell\",\"command\":\"echo hello\",\"reason\":\"check the loop\",\"read-only\":true,\"long-running\":false}]"
                         "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "run the tests\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (should (= (length prompts) 2))
          (ert-info ((format "Second prompt:\n%S" (car prompts)))
            (should (string-match-p "Shell: echo hello" (car prompts))))
          (with-current-buffer buf
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (string-match-p "Shell: echo hello" (buffer-string)))
              (should (string-match-p "Scalpel: done" (buffer-string))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-continuation-does-not-repeat-instruction ()
  "A continued round asks the planner to act on output, not to repeat the request.
Regression: every round re-sent the original instruction, so the
planner re-issued the same shell action and the user had to confirm
the same command over and over."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'always)
        (scalpel-console-max-rounds 3)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (prompt &optional _system)
                       (push prompt prompts)
                       (if (= (length prompts) 1)
                           "[{\"tool\":\"shell\",\"command\":\"echo hello\",\"reason\":\"check\",\"read-only\":true,\"long-running\":false}]"
                         "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "run the tests\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (should (= (length prompts) 2))
          (ert-info ((format "Second prompt:\n%S" (car prompts)))
            ;; History still carries the first turn's shell report...
            (should (string-match-p "Shell: echo hello" (car prompts)))
            ;; ...but the trailing instruction is the continuation, not
            ;; the user's original words.
            (should (string-suffix-p
                     scalpel-console--continuation-instruction
                     (car prompts)))
            (should-not (string-match-p "User instruction:\nrun the tests"
                                        (car prompts)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-forget-history-keeps-context ()
  "Forgetting history clears the conversation but keeps the context.
Regression: the only way to shed a growing conversation was to
reopen the console, which also discarded the context file list."
  (let ((scalpel-agent--context-files '("/tmp/scalpel-forget-ctx.el"))
        (scalpel-agent--context-readonly-files nil)
        (scalpel-console--context-baseline 'none-yet)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          ;; Same shape as `scalpel-console-open' writes: the header is
          ;; display output, never pending input.
          (let ((beg (point)))
            (insert "Scalpel console.\n\n")
            (put-text-property beg (point) 'scalpel-console-output t))
          (scalpel-console--show-context)
          (scalpel-console--insert-tagged "User: first\n" 'user)
          (scalpel-console--insert-tagged "Scalpel: reply\n\n" 'assistant)
          (scalpel-console-forget-history)
          (ert-info ((format "After forget:\n%S" (buffer-string)))
            ;; Conversation is no longer sent to the agent.
            (should (string= (scalpel-console--history) ""))
            ;; The turns stay visible: forgetting is not erasing.
            (should (string-match-p "User: first" (buffer-string)))
            (should (string-match-p "Scalpel: reply" (buffer-string)))
            ;; Context tree kept.
            (should (string-match-p "scalpel-forget-ctx.el"
                                    (buffer-string)))
            ;; Nothing pending: forgotten turns read as display output.
            (should (null (scalpel-console--pending-input-regions)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-forget-history-frees-next-request ()
  "After a forget, the next request carries no earlier conversation.
Regression: `forget-history' deleted the text, so nothing verified
that a live console (text kept, tags dropped) really starts the
next turn clean."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (scalpel-console--context-baseline 'none-yet)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (prompt &optional _system)
                       (push prompt prompts)
                       "[{\"tool\":\"reply\",\"text\":\"ack\"}]")))
            (with-current-buffer buf
              (erase-buffer)
              (insert "first instruction\n")
              (goto-char (point-min))
              (scalpel-console-send-line)
              (scalpel-console-forget-history)
              (goto-char (point-max))
              (insert "second instruction\n")
              (scalpel-console-send-line))
            (ert-info ((format "Second prompt:\n%S" (car prompts)))
              (should-not (string-match-p "first instruction" (car prompts)))
              (should-not (string-match-p "Scalpel: ack" (car prompts)))
              (should (string-suffix-p "User instruction:\nsecond instruction"
                                       (car prompts))))
            (with-current-buffer buf
              ;; The old turns are still readable in the console.
              (should (string-match-p "first instruction" (buffer-string)))
              (should (string-match-p "Scalpel: ack" (buffer-string))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-round-limit-does-not-ask-to-continue ()
  "The continuation question is not asked when no round is left to run.
Regression: `scalpel-console--continue-p' was consulted on the last
round too, so the user answered a question whose answer was thrown
away, and the console then stopped without saying why."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'ask)
        (scalpel-console-max-rounds 2)
        (buf (scalpel-console-test--new-console-buffer))
        (requests 0)
        (asked 0)
        (notices nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (&rest _)
                       (setq requests (1+ requests))
                       "[{\"tool\":\"shell\",\"command\":\"echo hi\",\"reason\":\"check\",\"read-only\":true,\"long-running\":false}]"))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (setq asked (1+ asked)) t))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) notices))))
            ;; `scalpel-console--continue-p' asks only outside batch.
            (let ((noninteractive nil))
              (with-current-buffer buf
                (erase-buffer)
                (insert "run it\n")
                (goto-char (point-min))
                (scalpel-console-send-line))))
          (ert-info ((format "requests=%d asked=%d buffer:\n%S"
                             requests asked
                             (with-current-buffer buf (buffer-string))))
            (should (= requests 2))
            ;; Only the round that still has a successor asks.
            (should (= asked 1))
            (should (string-match-p "round limit (2) reached"
                                    (with-current-buffer buf (buffer-string))))
            ;; The console buffer may be scrolled away, so the limit
            ;; must also reach the echo area.
            (should (cl-some (lambda (m)
                               (string-match-p "round limit (2) reached" m))
                             notices))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-sends-input-typed-above-buffer-end ()
  "Input typed before the buffer end is sent, not treated as absent.
Regression: the pending instruction was located by a position
snapshot, so text typed above that snapshot was invisible to RET
and the user got \"nothing to send\" (or a byte-shifted
instruction) after the round-limit notice."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        (prompt-sent nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (prompt &optional _system)
                       (setq prompt-sent prompt)
                       "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
            (with-current-buffer buf
              (erase-buffer)
              (insert "Scalpel console.\n")
              (put-text-property (point-min) (point)
                                 'scalpel-console-output t)
              (insert "Scalpel: round limit (3) reached\n\n")
              (put-text-property (point-min) (point)
                                 'scalpel-console-output t)
              ;; Type the instruction on the empty line above the end,
              ;; not at point-max.
              (goto-char (point-max))
              (forward-line -1)
              (insert "increase the round limit")
              (scalpel-console-send-line))
            (ert-info ((format "Prompt:\n%S" prompt-sent))
              (should prompt-sent)
              (should (string-suffix-p
                       "User instruction:\nincrease the round limit"
                       prompt-sent)))
            (with-current-buffer buf
              (ert-info ((format "Buffer:\n%S" (buffer-string)))
                (should (string-match-p "Scalpel: done" (buffer-string)))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-continue-prompt-reports-output-size ()
  "The continuation prompt names each command and its output size.
Regression: the prompt showed only command names, so a command
that dumped a large file could be approved unnoticed."
  (let ((scalpel-console-continue-after-shell 'ask)
        (noninteractive nil)
        prompt)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (p) (setq prompt p) t)))
      (should (scalpel-console--continue-p
               '(:shells ((:command "cat big.log" :bytes 12345
                                    :truncated t :binary nil))))))
    (ert-info ((format "Prompt:\n%S" prompt))
      (should (string-match-p "cat big.log" prompt))
      (should (string-match-p "12345 bytes, truncated" prompt)))))

(ert-deftest scalpel-console-test-noisy-round-p ()
  "A round is noisy when output is huge, truncated, or binary."
  (should (scalpel-console--noisy-round-p
           '(:shells ((:command "a" :bytes 5000)))))
  (should (scalpel-console--noisy-round-p
           '(:shells ((:command "a" :bytes 10 :truncated t)))))
  (should (scalpel-console--noisy-round-p
           '(:shells ((:command "a" :bytes 10 :binary t)))))
  (should-not (scalpel-console--noisy-round-p
               '(:shells ((:command "a" :bytes 10))))))

(ert-deftest scalpel-console-test-always-skips-noisy-output ()
  "Under `always', a noisy round is refused and the user is told.
Regression: `always' sent truncated megabytes back automatically."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'always)
        (scalpel-console-max-rounds 5)
        (buf (scalpel-console-test--new-console-buffer))
        (requests 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (&rest _)
                       (setq requests (1+ requests))
                       "[{\"tool\":\"shell\",\"command\":\"seq 1 2000\",\"reason\":\"noise\",\"read-only\":true,\"long-running\":false}]"))
                    ((symbol-function 'scalpel-sandbox-run)
                     (lambda (&rest _ignore)
                       (cons 0 (make-string 5000 ?x)))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "dump it\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (ert-info ((format "requests=%d" requests))
            (should (= requests 1))
            (with-current-buffer buf
              (should (string-match-p "not sent back automatically"
                                      (buffer-string))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(provide 'scalpel-console-test)

;;; scalpel-console-test.el ends here
