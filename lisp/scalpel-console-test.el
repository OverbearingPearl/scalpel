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
        (buf (scalpel-console-test--new-console-buffer))
        (prompt-sent nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (setq prompt-sent prompt)
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
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
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
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
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt _on-success on-error &optional _system)
                       (funcall on-error (list :type 'api :message "Boom")))))
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
  "A new instruction is rejected while a request is in flight.
Regression: the guard is buffer-local to the console, so a test
that binds it in its own buffer never reaches the rejection
branch."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (setq scalpel-console--busy t)
          (insert "second instruction\n")
          (goto-char (point-max))
          (scalpel-console-send-line)
          (should-not (search-forward "User: second instruction" nil t)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-progress-callback-bound-during-request ()
  "The progress callback must be bound while the agent request runs."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (seen nil))
    (unwind-protect
        (progn
          (with-current-buffer buf (erase-buffer))
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (setq seen (functionp scalpel-llm--progress-callback))
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
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
    (let ((buf nil))
      (unwind-protect
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_p on-success _on-error &optional _s)
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
            (scalpel-console-open)
            (setq buf (current-buffer))
            (with-current-buffer buf
              ;; The session context is buffer-local to the console, so
              ;; it is set in the console buffer, not in the test's own
              ;; buffer; rendering it here also proves the console holds
              ;; its own copy.
              (erase-buffer)
              (setq scalpel-agent--context-files (list this-file))
              (scalpel-console--show-context)
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
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq scalpel-console--context-baseline 'none-yet)
            ;; Session variables are buffer-local to the console.
            (setq scalpel-agent--context-files
                  '("/tmp/scalpel-diff-name.el"))
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

(ert-deftest scalpel-console-test-history-trims-consumed-reports ()
  "Only the newest report keeps its body; older bodies become a placeholder.
Regression: every report's full output stayed in the history, so a
long session re-sent the same bytes on every prompt."
  (let ((scalpel-console--context-baseline 'none-yet)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (scalpel-console--insert-tagged "User: first\n" 'user)
          (scalpel-console--insert-tagged
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---\n\n")
           'assistant)
          (scalpel-console--insert-tagged "User: second\n" 'user)
          (scalpel-console--insert-tagged
           (concat "Scalpel: Shell: pwd\nExit: 0\nOutput: 5 bytes\n"
                   "--- output ---\n/tmp\n--- end output ---\n\n")
           'assistant)
          (let ((history (scalpel-console--history)))
            (ert-info ((format "History:\n%S" history))
              ;; The newest body survives: the next round still reads it.
              (should (string-match-p "/tmp" history))
              ;; The older one is spent and is replaced.
              (should-not (string-match-p "a.el" history))
              (should (string-match-p
                       (regexp-quote scalpel-console--consumed-output-marker)
                       history))
              ;; Its header survives, so the planner still knows what ran.
              (should (string-match-p "Shell: ls" history)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-history-keeps-bodies-when-trim-is-off ()
  "With the trim disabled, every report body reaches the planner."
  (let ((scalpel-console--context-baseline 'none-yet)
        (scalpel-console-trim-consumed-output nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (scalpel-console--insert-tagged
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---\n\n")
           'assistant)
          (scalpel-console--insert-tagged
           (concat "Scalpel: Shell: pwd\nExit: 0\nOutput: 5 bytes\n"
                   "--- output ---\n/tmp\n--- end output ---\n\n")
           'assistant)
          (let ((history (scalpel-console--history)))
            (ert-info ((format "History:\n%S" history))
              (should (string-match-p "a.el" history))
              (should (string-match-p "/tmp" history))
              (should-not (string-match-p
                           (regexp-quote
                            scalpel-console--consumed-output-marker)
                           history)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-read-round-continues ()
  "A round that only read a file is followed by another round.
Regression: the continuation judge tested :shells alone, so a read
produced output the planner could never be given."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'always)
        (scalpel-console-max-rounds 5)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (push prompt prompts)
                       (funcall on-success
                                (if (= (length prompts) 1)
                                    "[{\"tool\":\"read\",\"file\":\"/tmp/a.el\",\"symbol\":\"foo\"}]"
                                  "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
                    ((symbol-function 'scalpel-agent-read)
                     (lambda (file symbol)
                       (format (concat "Read: %s in %s\nOutput: 14 bytes\n"
                                       "--- output ---\n(defun foo ())\n"
                                       "--- end output ---")
                               symbol file))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "read foo\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (should (= (length prompts) 2))
          (ert-info ((format "Second prompt:\n%S" (car prompts)))
            (should (string-match-p "(defun foo ())" (car prompts))))
          (with-current-buffer buf
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (string-match-p "Scalpel: done" (buffer-string))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

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
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq scalpel-console--context-baseline 'none-yet)
            ;; Session variables are buffer-local to the console.
            (setq scalpel-agent--context-files
                  '("/tmp/scalpel-diff-keep.el"))
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
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (setq scalpel-console--context-baseline 'none-yet)
          ;; Session variables are buffer-local to the console.
          (setq scalpel-agent--context-files
                '("/tmp/scalpel-reset-cursor.el"))
          (scalpel-console-reset-context)
          (ert-info ((format "Point %d of %d; buffer:\n%S"
                             (point) (point-max) (buffer-string)))
            (should (= (point) (point-max)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-shift-return-inserts-newline ()
  "S-RET is bound to a newline insertion, not to sending."
  (let ((scalpel-agent--context-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'scalpel-llm-request-async)
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
        (buf (scalpel-console-test--new-console-buffer))
        prompt-sent)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (setq prompt-sent prompt)
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
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
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'scalpel-llm-request-async)
                   (lambda (_prompt on-success _on-error &optional _system)
                     (funcall on-success
                              "[{\"tool\":\"reply\",\"text\":\"ack\"}]"))))
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
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (setq scalpel-console--context-baseline 'none-yet)
          ;; Session variables are buffer-local to the console.
          (setq scalpel-agent--context-files '("/tmp/scalpel-history.el"))
          (insert "Scalpel console.\n\n")
          (scalpel-console--insert-tagged "User: hello\n" 'user)
          (scalpel-console--show-context)
          (ert-info ((format "Buffer:\n%S" (buffer-string)))
            (should (string= (scalpel-console--history) "User: hello"))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-send-line-sends-recorded-conversation ()
  "Each round re-sends the replies recorded in the buffer."
  (let ((scalpel-agent--context-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (push prompt prompts)
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"first reply\"}]"))))
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
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'always)
        (scalpel-console-max-rounds 30)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (push prompt prompts)
                       (funcall on-success
                                (if (= (length prompts) 1)
                                    "[{\"tool\":\"shell\",\"command\":\"echo hello\",\"reason\":\"check the loop\",\"long-running\":false}]"
                                  "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
                    ((symbol-function 'scalpel-sandbox-run)
                     (lambda (&rest _ignore) (cons 0 "hello\n"))))
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
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'always)
        (scalpel-console-max-rounds 3)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (push prompt prompts)
                       (funcall on-success
                                (if (= (length prompts) 1)
                                    "[{\"tool\":\"shell\",\"command\":\"echo hello\",\"reason\":\"check\",\"long-running\":false}]"
                                  "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
                    ((symbol-function 'scalpel-sandbox-run)
                     (lambda (&rest _ignore) (cons 0 "hello\n"))))
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
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (setq scalpel-console--context-baseline 'none-yet)
          ;; Session variables are buffer-local to the console.
          (setq scalpel-agent--context-files '("/tmp/scalpel-forget-ctx.el"))
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
        (scalpel-console--context-baseline 'none-yet)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (push prompt prompts)
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"ack\"}]"))))
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
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'ask)
        (scalpel-console-max-rounds 2)
        (buf (scalpel-console-test--new-console-buffer))
        (requests 0)
        (asked 0)
        (notices nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (setq requests (1+ requests))
                       (funcall on-success
                                "[{\"tool\":\"shell\",\"command\":\"echo hi\",\"reason\":\"check\",\"long-running\":false}]")))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (setq asked (1+ asked)) t))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) notices)))
                    ((symbol-function 'scalpel-sandbox-run)
                     ;; Output must be noisy, or the round never
                     ;; reaches the question this test is about.
                     (lambda (&rest _ignore)
                       (cons 0 (make-string 5000 ?x)))))
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
        (buf (scalpel-console-test--new-console-buffer))
        (prompt-sent nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (setq prompt-sent prompt)
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
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

(ert-deftest scalpel-console-test-always-continues-through-noisy-output ()
  "Under `always', even a noisy round continues without a question.
The size gate decides only whether the user is asked; `always'
means the question was waived, so the large output goes back."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'always)
        (scalpel-console-max-rounds 5)
        (buf (scalpel-console-test--new-console-buffer))
        (requests 0)
        (asked 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (setq requests (1+ requests))
                       (funcall on-success
                                (if (= requests 1)
                                    "[{\"tool\":\"shell\",\"command\":\"seq 1 2000\",\"reason\":\"noise\",\"long-running\":false}]"
                                  "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (setq asked (1+ asked)) t))
                    ((symbol-function 'scalpel-sandbox-run)
                     (lambda (&rest _ignore)
                       (cons 0 (make-string 5000 ?x)))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "dump it\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (ert-info ((format "requests=%d asked=%d" requests asked))
            (should (= requests 2))
            (should (= asked 0))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-small-output-continues-without-asking ()
  "Small shell output is fed back with no question.
Regression: the continuation question was asked on every round that
ran a shell command, so finishing a short inspection cost the user
a keystroke and bought no information."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'ask)
        (scalpel-console-max-rounds 3)
        (buf (scalpel-console-test--new-console-buffer))
        (requests 0)
        (asked 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (setq requests (1+ requests))
                       (funcall on-success
                                (if (= requests 1)
                                    "[{\"tool\":\"shell\",\"command\":\"ls\",\"reason\":\"look\",\"long-running\":false}]"
                                  "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (setq asked (1+ asked)) t))
                    ((symbol-function 'scalpel-sandbox-run)
                     (lambda (&rest _ignore) (cons 0 "a.el\n"))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "list it\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (ert-info ((format "requests=%d asked=%d" requests asked))
            (should (= requests 2))
            (should (= asked 0))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-sandbox-error-stays-out-of-conversation ()
  "A sandbox failure reaches the user but never the planner.
Regression: every round error was recorded as an assistant turn, so
a message naming bubblewrap or sandbox-exec was re-sent in the next
request -- leaking the boundary the prompt deliberately omits."
  (let ((scalpel-agent--context-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (push prompt prompts)
                       (funcall on-success
                                (concat "[{\"tool\":\"shell\",\"command\":\"ls\","
                                        "\"reason\":\"look\","
                                        "\"long-running\":false}]"))))
                    ((symbol-function 'scalpel-sandbox-run)
                     (lambda (&rest _ignore)
                       (signal 'scalpel-sandbox-error
                               (list "Scalpel: no supported command sandbox")))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "look around\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (with-current-buffer buf
              (ert-info ((format "Buffer:\n%S" (buffer-string)))
                ;; The user must still see what went wrong...
                (should (string-match-p "no supported command sandbox"
                                        (buffer-string)))
                ;; ...without the failure joining the conversation.
                (should-not (string-match-p
                             "no supported command sandbox"
                             (scalpel-console--history))))))
          (ert-info ((format "Prompts:\n%S" prompts))
            (should prompts)
            (should-not (string-match-p "sandbox" (car prompts)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-busy-until-deferred-callback-settles ()
  "The busy guard covers the window after `send-line' has returned.
Regression: every other console test answers the request from
inside `scalpel-console-send-line', so the interval in which the
request is genuinely outstanding was never exercised and a guard
released too early -- or never -- would still look correct."
  (let ((buf (scalpel-console-test--new-console-buffer))
        pending)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success on-error &optional _system)
                       (setq pending (cons on-success on-error)))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "first instruction\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (ert-info ("the guard must hold once `send-line' has returned")
              (with-current-buffer buf
                (should scalpel-console--busy)
                (goto-char (point-max))
                (insert "second instruction\n")
                (scalpel-console-send-line)
                (should-not (string-match-p "User: second instruction"
                                            (buffer-string)))))
            ;; Settle from outside the call stack of `send-line'.
            (funcall (car pending)
                     "[{\"tool\":\"reply\",\"text\":\"done\"}]")
            (with-current-buffer buf
              (ert-info ((format "Buffer:\n%S" (buffer-string)))
                (should-not scalpel-console--busy)
                (should (string-match-p "Scalpel: done" (buffer-string)))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-progress-callback-cleared-after-settle ()
  "The status refresh does not outlive the round that installed it.
Regression: the binding was only ever asserted from inside a
synchronous mock, so an asynchronous round that left the refresh
pointing at a console it no longer owns would still pass."
  (let ((scalpel-llm--progress-callback nil)
        (buf (scalpel-console-test--new-console-buffer))
        pending)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success on-error &optional _system)
                       (setq pending (cons on-success on-error)))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "check it\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (should (functionp scalpel-llm--progress-callback))
            (funcall (car pending)
                     "[{\"tool\":\"reply\",\"text\":\"done\"}]")
            (ert-info ("the refresh must be unbound once the round settles")
              (should-not scalpel-llm--progress-callback))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-settles-after-console-buffer-is-killed ()
  "A callback arriving after the console is killed must not signal.
Regression: the round's callbacks re-selected the console buffer
unconditionally, so killing it mid-flight made the agent's plan
callback raise \"Selecting deleted buffer\" from inside gptel's
process filter; the user got no report and the error surfaced only
in *Messages*.  The busy guard needs no release here: it is
buffer-local and died with the buffer."
  (let ((buf (scalpel-console-test--new-console-buffer))
        pending)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success on-error &optional _system)
                       (setq pending (cons on-success on-error)))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "inspect it\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (should (with-current-buffer buf scalpel-console--busy))
            (kill-buffer buf)
            ;; The deferred callback arrives after the buffer is gone;
            ;; it must return normally rather than signal.
            (funcall (car pending)
                     "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest scalpel-console-test-add-file-from-another-buffer ()
  "Adding a file outside the console still reaches the console context.
Regression: the session context is buffer-local to the console, so
an add performed in another buffer set that buffer's local value
and the console context stayed empty."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (scalpel-utils-test-with-temp-file ".el"
          (with-temp-file this-file (insert "(defun foo ())"))
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _) this-file)))
            (with-temp-buffer
              (setq default-directory
                    (file-name-as-directory
                     (expand-file-name temporary-file-directory)))
              (scalpel-console-add-file)))
          (with-current-buffer buf
            (ert-info ((format "Console context: %S"
                               scalpel-agent--context-files))
              (should (member (file-truename (expand-file-name this-file))
                              scalpel-agent--context-files)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(provide 'scalpel-console-test)

;;; scalpel-console-test.el ends here
