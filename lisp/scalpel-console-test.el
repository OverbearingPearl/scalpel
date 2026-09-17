;;; scalpel-console-test.el --- Tests for scalpel-console -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-console.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-console)
(require 'scalpel-agent)
(require 'scalpel-utils-test)

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

(ert-deftest scalpel-console-test-nested-roots-prefer-the-deepest-console ()
  "Console buffers with nested roots prefer the deepest matching root."
  (let* ((temp-dir (make-temp-file "scalpel-console-test" t))
         (nested-dir (expand-file-name "nested" temp-dir))
         (parent-buffer (get-buffer-create "*scalpel-console-test-parent*"))
         (nested-buffer (get-buffer-create "*scalpel-console-test-nested*")))
    (make-directory nested-dir)
    (unwind-protect
        (progn
          (with-current-buffer parent-buffer
            (scalpel-console-mode)
            (setq-local scalpel-console--root temp-dir)
            (setq default-directory temp-dir))
          (with-current-buffer nested-buffer
            (scalpel-console-mode)
            (setq-local scalpel-console--root nested-dir)
            (setq default-directory nested-dir))
          (with-temp-buffer
            (setq default-directory nested-dir)
            (should (eq (scalpel-console--target-buffer) nested-buffer))
            (should-not (eq (scalpel-console--target-buffer) parent-buffer))))
      (kill-buffer parent-buffer)
      (kill-buffer nested-buffer)
      (delete-directory temp-dir t))))

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
  "Send a line to the agent and verify the reply is appended.
The \"Roger. Working...\" acknowledgement is dropped once the reply arrives."
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
                               "User: test instruction\nScalpel: done\n\nScalpel: Mission complete, over.\n\n")))))
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

(ert-deftest scalpel-console-test-send-line-rejected-when-sibling-busy ()
  "Refuse sending a prompt from a console while a sibling console for the same root is busy."
  (let ((root (make-temp-file "scalpel-console-root" t))
        buf-a buf-b)
    (unwind-protect
        (progn
          (setq buf-a (get-buffer-create "*scalpel console*")
                buf-b (get-buffer-create
                       (generate-new-buffer-name "*scalpel console*")))
          (with-current-buffer buf-a
            (scalpel-console-mode)
            (setq-local scalpel-console--root root))
          (with-current-buffer buf-b
            (scalpel-console-mode)
            (setq-local scalpel-console--root root)
            (setq scalpel-console--busy t))
          (with-current-buffer buf-a
            (insert "user prompt")
            (goto-char (point-min))
            (should-not (scalpel-console-send-line))
            (should-not (string-match-p "User:" (buffer-string))))
          (with-current-buffer buf-b
            (should scalpel-console--busy)
            (should-not (string-match-p "User:" (buffer-string)))))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b)))))

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
            ;; Anchor the console to the temp directory: `open' uses the
            ;; caller's `default-directory' as its root, and the test's
            ;; own buffer would anchor it to the package source tree --
            ;; a user session, which the kill confirmation protects.
            ;; `scalpel-console-open' selects the console in the current
            ;; window, so the selection is confined to an excursion: an
            ;; interactive ERT run must leave the user's display alone.
            (let ((default-directory
                   (file-name-as-directory
                    (expand-file-name temporary-file-directory))))
              (scalpel-utils-test-with-preserved-windows
                (scalpel-console-open)
                (setq buf (current-buffer))))
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
              ;; Anchor the console to the temp directory, for the same
              ;; reason as in `...-open-shows-context' above; the window
              ;; excursion is for the same reason too, since `open'
              ;; selects the console buffer.
              (let ((default-directory
                     (file-name-as-directory
                      (expand-file-name temporary-file-directory))))
                (scalpel-utils-test-with-preserved-windows
                  (scalpel-console-open)
                  (setq buf (current-buffer))))
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

(ert-deftest scalpel-console-test-file-level-round-continues ()
  "A round that changed a file is followed by another round.
Regression: the continuation test read :shells, :reads and :edits, so
a round whose only action created, renamed or deleted a file ended
the loop -- the context tree was drawn for the file the planner had
just created, and then the console stopped without ever giving the
planner the round that would have finished the request."
  (let ((scalpel-agent--context-files nil)
        (scalpel-console-max-rounds 5)
        (buf (scalpel-console-test--new-console-buffer))
        (rounds 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-agent-run)
                     (lambda (_instruction _history on-done _on-error)
                       (setq rounds (1+ rounds))
                       (funcall on-done
                                (if (= rounds 1)
                                    '(:report "Created file /tmp/a.el"
                                      :shells nil :reads nil
                                      :changes ("Created file /tmp/a.el"))
                                  '(:report "done" :shells nil
                                            :reads nil :changes nil))))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "create the file\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (with-current-buffer buf
            (ert-info ((format "rounds=%d buffer:\n%S"
                               rounds (buffer-string)))
              (should (= rounds 2))
              (should (string-match-p "Scalpel: Created file /tmp/a.el"
                                      (buffer-string)))
              (should (string-match-p "Scalpel: done"
                                      (buffer-string))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-context-change-is-shown-as-a-tree ()
  "A round that changes the context file list draws the context tree.
Regression: `file-create', `file-rename' and `file-delete' changed which
files the session holds, and nothing on screen said so, so the tree the
console had drawn described a session that no longer existed."
  (let ((scalpel-agent--context-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        (added "/tmp/scalpel-context-added.el"))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (erase-buffer)
            (setq scalpel-console--context-baseline 'none-yet)
            ;; The baseline the next refresh measures against is the one
            ;; `scalpel-console-open' establishes: a refresh of its own,
            ;; with the context still empty.
            (scalpel-console--show-context)
            (scalpel-console--insert-tagged "User: make the file\n" 'user))
          (cl-letf (((symbol-function 'scalpel-agent-run)
                     (lambda (_instruction _history on-done _on-error)
                       ;; What `file-create' does inside a round: the file
                       ;; exists on disk and the session now holds it.
                       (setq scalpel-agent--context-files (list added))
                       (funcall on-done
                                '(:report "Created file x" :shells nil
                                          :reads nil :changes nil)))))
            (with-current-buffer buf
              (scalpel-console--run-round
               "make the file" "User: make the file\n" #'ignore)))
          (with-current-buffer buf
            (let ((report-pos (progn (goto-char (point-min))
                                     (search-forward
                                      "Scalpel: Created file x" nil t)))
                  (tree-pos (progn (goto-char (point-min))
                                   (search-forward
                                    "scalpel-context-added.el" nil t)
                                   (match-beginning 0))))
              (ert-info ((format "Buffer:\n%S" (buffer-string)))
                ;; The tree lands after the round's own report...
                (should report-pos)
                (should tree-pos)
                (should (< report-pos tree-pos))
                ;; ...and the file this round added is the marked one, so
                ;; the tree is the delta display and not a bare listing.
                (should (eq (get-text-property tree-pos 'face)
                            'scalpel-console-context-added-face)))
              ;; Display only: the tree never joins the conversation.
              (should-not (string-match-p "Context:"
                                          (scalpel-console--history))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

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
        (scalpel-llm--total-received 34))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert "User: hi\n")
          (goto-char (point-max))
          (let* ((status (scalpel-console--status-start
                          (list :system 12 :context 0 :history 0
                                :instruction 0)))
                 (refresh (car status))
                 (stop (cdr status)))
            (should (= (point) (point-max)))
            (should (eq (char-before) ?\n))
            (save-excursion
              (goto-char (point-min))
              (should (search-forward
                       (concat "Scalpel: up 12 = sys 12 + ctx 0 + hist 0"
                               " + instr 0, down 0, 0s\n")
                       nil t)))
            ;; A second request inside the same round must not reset the
            ;; down display: the line reads the growth of the cumulative
            ;; total, never the per-request counter a new request zeroes.
            (setq scalpel-llm--tokens-received 0)
            (setq scalpel-llm--total-received 40)
            (funcall refresh)
            (save-excursion
              (goto-char (point-min))
              ;; 40 received minus the 34 snapshotted at insert time.
              (should (search-forward "down 6," nil t)))
            ;; Refreshing rewrites the same single line.
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
                                    "[{\"tool\":\"file-peek\",\"file\":\"/tmp/a.el\",\"symbol\":\"foo\"}]"
                                  "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
                    ((symbol-function 'scalpel-agent-file-read)
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

(ert-deftest scalpel-console-test-append-follows-point-only-at-the-end ()
  "Appending moves point to the end only when the user is already there.
Regression: `scalpel-console--append' moved point to point-max
unconditionally, so redisplay scrolled the window back to the
bottom while the user was reading earlier turns, and every new
report yanked the view away from the history."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (scalpel-console--insert-tagged "User: first\n" 'user)
          ;; The user scrolled back: point sits mid-buffer.
          (goto-char (point-min))
          (scalpel-console--append "Scalpel: reply one\n\n" 'assistant)
          (ert-info ((format "Point %d of %d; buffer:\n%S"
                             (point) (point-max) (buffer-string)))
            ;; The text still lands at the end...
            (should (string-match-p "Scalpel: reply one" (buffer-string)))
            (should (string-match-p
                     "\\`User: first\nScalpel: reply one"
                     (buffer-string)))
            ;; ...but the user's point stays where it was.
            (should (= (point) (point-min))))
          ;; A user waiting at the end keeps following the output.
          (goto-char (point-max))
          (scalpel-console--append "Scalpel: reply two\n\n" 'assistant)
          (ert-info ((format "Point %d of %d" (point) (point-max)))
            (should (= (point) (point-max)))
            (should (string-match-p "Scalpel: reply two" (buffer-string)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

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
                                 "User: line one\nline two\nScalpel: done\n\nScalpel: Mission complete, over.\n\n")))))
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
                                       ""
                                       "Scalpel: ack\n\n"
                                       "Scalpel: Mission complete, over.\n\n"))))))
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
                     scalpel-prompt--continuation-instruction
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

(ert-deftest scalpel-console-test-round-appends-at-end-not-at-point ()
  "A round's report lands at buffer end, not at the user's cursor.
Regression: the round callbacks inserted at point, relying on the
status refresh to keep point pinned at point-max.  Once the refresh
was wrapped in `save-excursion', point belongs to the user, and a
bare insert drops the reply wherever the cursor happens to sit --
splitting the record and reordering `--history'."
  (let ((scalpel-llm--progress-callback nil)
        (scalpel-llm--tokens-uploaded 0)
        (scalpel-llm--tokens-received 0)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (erase-buffer)
            (scalpel-console--insert-tagged "User: hi\n" 'user))
          (cl-letf (((symbol-function 'scalpel-agent-run)
                     (lambda (_instruction _history on-done _on-error)
                       ;; Simulate the user reviewing the record while
                       ;; the request is in flight: point is mid-buffer
                       ;; when the report arrives.
                       (goto-char (point-min))
                       (funcall on-done
                                '(:report "done" :shells nil :reads nil)))))
            (with-current-buffer buf
              (scalpel-console--run-round "hi" "User: hi\n" #'ignore)))
          (with-current-buffer buf
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (string= (buffer-string)
                               "User: hi\nScalpel: done\n\n"))
              (should (string= (scalpel-console--history)
                               "User: hi\nScalpel: done")))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-collapse-folds-report-body-not-text ()
  "A report body is folded in the display, never removed from the buffer.
Regression: a shell command that dumped thousands of lines sat in
the middle of the conversation, so the console was least readable
exactly when its output was largest.  The fold is display-only, so
`--history' and `--trim-report' see the same bytes as before."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nReason: look\nExit: 0\n"
                   "Output: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (goto-char (point-min))
          (should (search-forward "\na.el\n" nil t))
          (let ((body (1+ (match-beginning 0))))
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (get-text-property body 'display))
              (should (string-match-p
                       "C-c C-o"
                       (get-text-property body 'scalpel-console-collapsed)))
              ;; The body text survives under the fold.
              (should (string-match-p
                       "--- output ---\na.el\n--- end output ---"
                       (buffer-substring-no-properties
                        (point-min) (point-max))))))
          ;; The header line stays readable: only the body is folded.
          (goto-char (point-min))
          (should (search-forward "Shell: ls" nil t))
          (should-not (get-text-property (match-beginning 0) 'display)))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-collapse-keeps-history-whole ()
  "Folding is display-only: the planner still receives the body.
Regression: a fold implemented by deleting or replacing text would
have silently dropped the output the continuation round must read."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (setq scalpel-console--context-baseline 'none-yet)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (let ((history (scalpel-console--history)))
            (ert-info ((format "History:\n%S" history))
              (should (string-match-p "a.el" history))
              (should (string-match-p "--- end output ---" history)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-toggle-output-round-trips ()
  "Toggling expands the folded bodies, and toggling again folds them."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (goto-char (point-min))
          (should (search-forward "\na.el\n" nil t))
          (let ((body (1+ (match-beginning 0))))
            (should (get-text-property body 'display))
            (scalpel-console-toggle-output)
            (should-not (get-text-property body 'display))
            (scalpel-console-toggle-output)
            (should (get-text-property body 'display))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-collapse-off-keeps-bodies-visible ()
  "With folding disabled, no body is hidden."
  (let ((scalpel-console-collapse-output nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (goto-char (point-min))
          (should (search-forward "\na.el\n" nil t))
          (ert-info ((format "Buffer:\n%S" (buffer-string)))
            (should-not (get-text-property (1+ (match-beginning 0)) 'display))
            (should-not (get-text-property (1+ (match-beginning 0))
                                           'scalpel-console-collapsed))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-collapse-folds-every-report-in-a-round ()
  "A round holding several reports folds each of their bodies.
One round's report is the actions' reports joined, so a read
followed by a shell command arrives as a single insertion."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (scalpel-console--append
           (concat "Scalpel: Read: foo in /tmp/foo.el\nOutput: 14 bytes\n"
                   "--- output ---\n(defun foo ())\n--- end output ---\n"
                   "Scalpel: Shell: ls\nExit: 0\nOutput: 7 bytes\n"
                   "--- output ---\nout.txt\n--- end output ---")
           'assistant)
          (goto-char (point-min))
          (should (search-forward "\n(defun foo ())\n" nil t))
          (let ((read-body (1+ (match-beginning 0))))
            (goto-char (point-min))
            (should (search-forward "\nout.txt\n" nil t))
            (let ((shell-body (1+ (match-beginning 0))))
              (ert-info ((format "Buffer:\n%S" (buffer-string)))
                (should (get-text-property read-body 'display))
                (should (get-text-property shell-body 'display))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-collapsed-body-does-not-leak-onto-input ()
  "The fold never spreads onto the text the user types after it.
Regression: `insert-and-inherit' copies the properties of the
preceding character, so a `display' property left out of
`rear-nonsticky' would replace the user's first keystroke with the
fold placeholder."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (goto-char (point-max))
          (let ((pos (point)))
            (insert-and-inherit "hello")
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (string= (buffer-substring-no-properties pos (point-max))
                               "hello"))
              (should-not (get-text-property pos 'display))
              (should-not (get-text-property pos 'scalpel-console-role)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-marks-only-spent-report-bodies ()
  "A report the planner no longer reads is marked; the newest is not.
Regression: `scalpel-console--history' replaced old bodies with a
placeholder silently, so the console showed a body the planner was
never sent."
  (let ((scalpel-console-trim-consumed-output t)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          ;; The reports are separated by a user turn: that is the shape
          ;; the console really produces, since the refresh runs when the
          ;; next instruction arrives.  Two adjacent assistant turns are
          ;; a single turn to `scalpel-console--history', and so are
          ;; trimmed -- and marked -- as one.
          (scalpel-console--insert-tagged "User: first\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (scalpel-console--insert-tagged "User: second\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: pwd\nExit: 0\nOutput: 5 bytes\n"
                   "--- output ---\n/tmp\n--- end output ---")
           'assistant)
          (goto-char (point-min))
          (should (search-forward "Shell: ls" nil t))
          (let ((spent (match-beginning 0)))
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (eq (get-text-property spent 'face)
                          'scalpel-console-consumed-body-face))
              (should (get-text-property spent
                                         'scalpel-console-consumed-body))
              (should (string-match-p
                       (regexp-quote scalpel-console--consumed-body-note)
                       (get-text-property spent 'help-echo)))))
          (goto-char (point-min))
          (should (search-forward "Shell: pwd" nil t))
          (ert-info ((format "Buffer:\n%S" (buffer-string)))
            (should-not (get-text-property (match-beginning 0) 'face))
            (should-not (get-text-property (match-beginning 0)
                                           'scalpel-console-consumed-body))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-consumed-mark-is-display-only ()
  "The mark adds no text and no pending-input region.
Regression: a mark inserted as text would join the record, and
`scalpel-console--history' would send it to the planner as part of
the header."
  (let ((scalpel-console-trim-consumed-output t)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          ;; A user turn between the reports, as the console produces.
          (scalpel-console--insert-tagged "User: first\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (scalpel-console--insert-tagged "User: second\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: pwd\nExit: 0\nOutput: 5 bytes\n"
                   "--- output ---\n/tmp\n--- end output ---")
           'assistant)
          (goto-char (point-min))
          (should (search-forward "Shell: ls" nil t))
          (ert-info ("the premise: the older report really is marked")
            (should (eq (get-text-property (match-beginning 0) 'face)
                        'scalpel-console-consumed-body-face)))
          (let ((history (scalpel-console--history))
                (text (buffer-string)))
            (ert-info ((format "History:\n%S" history))
              (should (string-match-p
                       (regexp-quote scalpel-console--consumed-output-marker)
                       history))
              (should (string-match-p "Output: 3 bytes" history))
              (should-not (string-match-p "a\\.el" history))
              (should (string-match-p "/tmp" history))
              (should-not (string-match-p
                           (regexp-quote scalpel-console--consumed-body-note)
                           history)))
            (ert-info ((format "Buffer:\n%S" text))
              (should-not (string-match-p
                           (regexp-quote scalpel-console--consumed-body-note)
                           text))))
          (ert-info ((format "Buffer:\n%S" (buffer-string)))
            (should (null (scalpel-console--pending-input-regions)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-consumed-mark-does-not-leak-onto-input ()
  "Typing inside a marked header does not inherit the mark's face.
Regression: keyboard input arrives through `insert-and-inherit', which
copies the preceding character's properties unless they are declared
`rear-nonsticky', so a face left out of that list would make the
user's own text look like spent output."
  (let ((scalpel-console-trim-consumed-output t)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          ;; A user turn between the reports, as the console produces.
          (scalpel-console--insert-tagged "User: first\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (scalpel-console--insert-tagged "User: second\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: pwd\nExit: 0\nOutput: 5 bytes\n"
                   "--- output ---\n/tmp\n--- end output ---")
           'assistant)
          (goto-char (point-min))
          (should (search-forward "Output: 3 bytes" nil t))
          ;; Insert at the end of the marked header, so only the
          ;; preceding character can hand properties over.
          (let ((pos (line-end-position)))
            (goto-char pos)
            (should (get-text-property (1- pos) 'face))
            (insert-and-inherit "x")
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (string= (buffer-substring-no-properties pos (1+ pos))
                               "x"))
              (should-not (get-text-property pos 'face))
              (should-not (get-text-property pos 'help-echo))
              (should-not (get-text-property
                           pos 'scalpel-console-consumed-body)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-consumed-marks-follow-the-trim-setting ()
  "Turning the trim off clears the marks it made.
Regression: a mark derived once and cached would leave the console
claiming a body was dropped when it is now sent whole."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (setq-local scalpel-console-trim-consumed-output t)
          ;; A user turn between the reports, as the console produces.
          (scalpel-console--insert-tagged "User: first\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (scalpel-console--insert-tagged "User: second\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: pwd\nExit: 0\nOutput: 5 bytes\n"
                   "--- output ---\n/tmp\n--- end output ---")
           'assistant)
          (goto-char (point-min))
          (should (search-forward "Shell: ls" nil t))
          (let ((spent (match-beginning 0)))
            (should (eq (get-text-property spent 'face)
                        'scalpel-console-consumed-body-face))
            (setq-local scalpel-console-trim-consumed-output nil)
            (scalpel-console--append
             (concat "Scalpel: Shell: whoami\nExit: 0\nOutput: 2 bytes\n"
                     "--- output ---\nme\n--- end output ---")
             'assistant)
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should-not (get-text-property spent 'face))
              (should-not (get-text-property
                           spent 'scalpel-console-consumed-body)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-forget-history-clears-consumed-marks ()
  "A forgotten report is not read at all, so its mark goes with it."
  (let ((scalpel-console-trim-consumed-output t)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          ;; A user turn between the reports, as the console produces.
          (scalpel-console--insert-tagged "User: first\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: ls\nExit: 0\nOutput: 3 bytes\n"
                   "--- output ---\na.el\n--- end output ---")
           'assistant)
          (scalpel-console--insert-tagged "User: second\n" 'user)
          (scalpel-console--append
           (concat "Scalpel: Shell: pwd\nExit: 0\nOutput: 5 bytes\n"
                   "--- output ---\n/tmp\n--- end output ---")
           'assistant)
          (goto-char (point-min))
          (should (search-forward "Shell: ls" nil t))
          (let ((spent (match-beginning 0)))
            (should (eq (get-text-property spent 'face)
                        'scalpel-console-consumed-body-face))
            (scalpel-console-forget-history)
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              ;; The text stays on screen...
              (goto-char (point-min))
              (should (search-forward "Shell: ls" nil t))
              ;; ...but no turn is a conversation turn any more.
              (should-not (get-text-property spent 'face))
              (should-not (get-text-property
                           spent 'scalpel-console-consumed-body))
              (should (null (scalpel-console--pending-input-regions)))
              (should (string= (scalpel-console--history) "")))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-error-turn-marks-previous-report ()
  "An error turn is a conversation turn, so the report before it is marked.
Regression: only `--append' refreshed the marks, so a round that
failed with a non-sandbox error left the previous body looking live
while the planner already saw the error as the newest turn."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent-confirm-tools nil)
        (scalpel-console-continue-after-shell 'always)
        (scalpel-console-max-rounds 5)
        (scalpel-console-trim-consumed-output t)
        (buf (scalpel-console-test--new-console-buffer))
        (requests 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success on-error &optional _system)
                       (setq requests (1+ requests))
                       (if (= requests 1)
                           (funcall on-success
                                    (concat "[{\"tool\":\"shell\","
                                            "\"command\":\"ls\","
                                            "\"reason\":\"look\","
                                            "\"long-running\":false}]"))
                         (funcall on-error
                                  (list :type 'parse
                                        :message "Bad JSON")))))
                    ((symbol-function 'scalpel-sandbox-run)
                     (lambda (&rest _ignore) (cons 0 "a.el\n"))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "list it\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (with-current-buffer buf
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (= requests 2))
              (goto-char (point-min))
              (should (search-forward "Shell: ls" nil t))
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'scalpel-console-consumed-body-face))
              (goto-char (point-min))
              (should (search-forward "Scalpel planner error: Bad JSON" nil t))
              ;; The error turn holds no fenced body, so nothing is
              ;; dropped from it: the consumed-body mark must not land
              ;; on it.  Its face is the planner-error dim, which is a
              ;; reading aid, not a trimming mark.
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'scalpel-console-planner-error-face))
              (should-not (get-text-property
                           (match-beginning 0)
                           'scalpel-console-consumed-body)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-abort-cancels-in-flight-request ()
  "\\[scalpel-console-abort] cancels the request currently in flight."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (cancelled 0))
    (unwind-protect
        (let ((scalpel-llm--cancel-current
               (lambda () (setq cancelled (1+ cancelled)))))
          (with-current-buffer buf
            (setq scalpel-console--busy t)
            (let ((generation scalpel-console--operation-generation))
              (scalpel-console-abort)
              (ert-info ((format "cancelled=%S busy=%S generation=%S"
                                 cancelled
                                 scalpel-console--busy
                                 scalpel-console--operation-generation))
                (should (= cancelled 1))
                (should-not scalpel-console--busy)
                (should (= scalpel-console--operation-generation
                           (1+ generation)))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-abort-without-request-dings ()
  "With no request in flight, abort says so instead of failing."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (notices nil))
    (unwind-protect
        (with-current-buffer buf
          (setq scalpel-console--busy nil)
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) notices))))
            (scalpel-console-abort)
            (should (cl-some (lambda (m) (string-match-p "no request" m)) notices))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-remove-file-removes-from-context ()
  "Removing a context file updates the context and the tree display."
  (let ((buf (scalpel-console-test--new-console-buffer))
        ;; `scalpel-agent-context-remove' compares truenames, so the
        ;; entries must be stored the way `context-add' stores them.
        (a (file-truename "/tmp/scalpel-rm-a.el"))
        (b (file-truename "/tmp/scalpel-rm-b.el")))
    (unwind-protect
        (with-current-buffer buf
          (setq scalpel-console--context-baseline 'none-yet)
          (setq scalpel-agent--context-files (list a b))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) a)))
            (scalpel-console-remove-file))
          (ert-info ((format "Context: %S" scalpel-agent--context-files))
            (should (equal scalpel-agent--context-files
                           (list b))))
          (goto-char (point-min))
          (should (search-forward "scalpel-rm-b.el" nil t)))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-remove-file-reports-empty-context ()
  "Removing from an empty context reports it and changes nothing."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (notices nil))
    (unwind-protect
        (with-current-buffer buf
          (setq scalpel-agent--context-files nil)
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) notices))))
            (scalpel-console-remove-file))
          (should (cl-some (lambda (m) (string-match-p "context is empty" m))
                           notices)))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-planner-error-gets-retry-header ()
  "A planner-output failure is headed so a retry is the obvious step.
Regression: a malformed action arrived as a generic \"Scalpel
error:\", indistinguishable from a Scalpel bug, so the user could
not tell that resending the instruction was the whole fix."
  (let ((scalpel-agent--context-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (funcall on-success
                                "[{\"tool\":\"shell\",\"command\":\"ls\"}]"))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "look around\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (with-current-buffer buf
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (string-match-p "Scalpel planner error" (buffer-string)))
              (should (string-match-p "missing required field" (buffer-string)))
              (should (string-match-p
                       (regexp-quote scalpel-console--retry-advice)
                       (buffer-string)))
              ;; The failure header is dimmed for reading, while the
              ;; turn still joins the conversation.
              (goto-char (point-min))
              (should (search-forward "Scalpel planner error" nil t))
              (should (eq (get-text-property (match-beginning 0) 'face)
                          'scalpel-console-planner-error-face))
              ;; The failure stays in the conversation: it is about the
              ;; planner, not about Scalpel's own boundary.
              (should (string-match-p
                       "planner error"
                       (scalpel-console--history))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-tool-call-advice-names-the-backend-switch ()
  "A reply in tool-call syntax is answered with the backend switch.
Regression: this failure was met with the same retry advice as a
truncated or malformed reply, so a user whose backend reproduces
the reply verbatim had no exit from the loop.
The subject is the advice, not dispatch: no dialect is registered
here, so the reply reaches the default parser and is refused there
whatever backend and model the test session carries."
  (let ((scalpel-agent--context-files nil)
        (scalpel-llm-dialect-providers nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (funcall on-success
                                (concat "<tool_call>shell<arg_key>command"
                                        "</arg_key><arg_value>ls</arg_value>"
                                        "</tool_call>")))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "look around\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (with-current-buffer buf
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              (should (string-match-p "Scalpel planner error"
                                      (buffer-string)))
              (should (string-match-p
                       (regexp-quote scalpel-console--tool-call-advice)
                       (buffer-string)))
              ;; The advice must name the binding that switches
              ;; backends, not a key that happens to exist.
              (should (string-match-p "C-c C-b"
                                      scalpel-console--tool-call-advice)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-prose-reply-is-delivered ()
  "A reply written as prose is delivered as the answer, not refused.
Regression: it arrived as a planner failure carrying the answer
inside the error text, so the round was thrown away and the user
was advised to rephrase -- advice that does not fix a model that
keeps answering in the same shape.  The prose now degrades to a
reply action, so the answer reaches the console like any reply."
  (let ((scalpel-agent--context-files nil)
        (scalpel-llm-dialect-providers nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (funcall on-success
                                (concat "The dependency lives in two layers.\n\n"
                                        "**The gateway** is the hard coupling: "
                                        "it calls gptel.\n")))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "analyse the dependency\n")
              (goto-char (point-min))
              (scalpel-console-send-line)))
          (with-current-buffer buf
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              ;; The answer is delivered verbatim as a reply report.
              (should (string-match-p "The gateway" (buffer-string)))
              (should (string-match-p "calls gptel" (buffer-string)))
              (should-not (string-match-p "Scalpel planner error"
                                          (buffer-string)))
              (should-not (string-match-p
                           (regexp-quote scalpel-console--prose-advice)
                           (buffer-string)))
              (should-not (string-match-p
                           (regexp-quote scalpel-console--retry-advice)
                           (buffer-string)))
              ;; The answer joins the conversation, so a follow-up
              ;; instruction keeps its referent.
              (should (string-match-p "The gateway"
                                      (scalpel-console--history))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-repeat-resends-last-instruction ()
  "Repeat resubmits the last instruction through the ordinary send path.
Regression: after a planner-output failure the user had to retype
the whole instruction by hand to try again."
  (let ((scalpel-agent--context-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        (prompts nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (prompt on-success _on-error &optional _system)
                       (push prompt prompts)
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"done\"}]"))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "first instruction\n")
              (goto-char (point-min))
              (scalpel-console-send-line)
              (scalpel-console-repeat)))
          (ert-info ((format "Prompts: %d" (length prompts)))
            (should (= (length prompts) 2))
            (should (string-suffix-p "User instruction:\nfirst instruction"
                                     (car prompts))))
          (with-current-buffer buf
            (ert-info ((format "Buffer:\n%S" (buffer-string)))
              ;; The repeated turn is recorded like a typed one.
              (should (= (how-many "User: first instruction"
                                   (point-min) (point-max))
                         2))
              (should (string-match-p "Scalpel: done" (buffer-string))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-repeat-without-history-signals ()
  "Repeating with no previous instruction fails loudly."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (setq scalpel-console--last-instruction nil)
          (should-error (scalpel-console-repeat) :type 'user-error))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-session-snapshot-round-trips-every-variable ()
  "A snapshot/restore cycle preserves every registered session variable.
Regression: the snapshot enumerated the variables by hand, so
`scalpel-console--last-instruction' was silently lost on a module
reload and `scalpel-console-repeat' then reported no previous
instruction even though the user had just sent one."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq scalpel-console--context-baseline 'none-yet
                  scalpel-console--last-instruction "send it"
                  scalpel-agent--context-files '("/tmp/scalpel-snap.el")))
          (let ((snapshot (with-current-buffer buf
                            (scalpel-console--session-snapshot))))
            ;; Wipe every registered variable, the way an unload does.
            (with-current-buffer buf
              (setq scalpel-console--context-baseline nil
                    scalpel-console--last-instruction nil
                    scalpel-agent--context-files nil))
            (scalpel-console--session-restore snapshot)
            (with-current-buffer buf
              (ert-info ((format "After restore: last=%S files=%S base=%S"
                                 scalpel-console--last-instruction
                                 scalpel-agent--context-files
                                 scalpel-console--context-baseline))
                (should (equal scalpel-console--last-instruction "send it"))
                (should (equal scalpel-agent--context-files
                               '("/tmp/scalpel-snap.el")))
                (should (eq scalpel-console--context-baseline 'none-yet))
                (should (equal scalpel-console--root
                               (file-name-as-directory
                                (expand-file-name
                                 temporary-file-directory))))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-session-snapshot-round-trips-token-totals ()
  "Token accounting survives the reload the suite performs.
Regression: the counters are global, so an unload reset them and the
token buffer's later lines stopped agreeing with its earlier ones."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (scalpel-token--grand-up 0)
        (scalpel-token--grand-down 0)
        (scalpel-token--console-totals (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash (buffer-name buf) '(10 20) scalpel-token--console-totals)
          (setq scalpel-token--grand-up 10
                scalpel-token--grand-down 20)
          (let ((snapshot (with-current-buffer buf
                            (scalpel-console--session-snapshot))))
            ;; A reload re-creates the global from its `defvar' form.
            (setq scalpel-token--console-totals (make-hash-table :test 'equal)
                  scalpel-token--grand-up 0
                  scalpel-token--grand-down 0)
            (scalpel-console--session-restore snapshot)
            (ert-info ((format "After restore: up=%d down=%d totals=%S"
                               scalpel-token--grand-up
                               scalpel-token--grand-down
                               (gethash (buffer-name buf)
                                        scalpel-token--console-totals)))
              (should (= scalpel-token--grand-up 10))
              (should (= scalpel-token--grand-down 20))
              (should (equal (gethash (buffer-name buf)
                                      scalpel-token--console-totals)
                             '(10 20))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-settle-failure-releases-busy ()
  "A failure in the settle path still releases the busy guard.
Regression: `scalpel-console--run-round' wrapped accounting,
status cleanup and `on-complete' in one `condition-case', so a
failure in the non-critical accounting step skipped
`on-complete' and left `scalpel-console--busy' set forever."
  (let ((scalpel-agent--context-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        (notices nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (funcall on-success
                                "[{\"tool\":\"reply\",\"text\":\"done\"}]")))
                     ((symbol-function 'scalpel-token-record)
                     (lambda (_name _up _down _breakdown)
                       (error "Accounting failed")))
                     ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) notices))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "trigger settle failure\n")
              (goto-char (point-min))
              (let ((err (condition-case e
                             (progn
                               (scalpel-console-send-line)
                               nil)
                           (error e))))
                (ert-info ((format "Unexpected send-line error: %S" err))
                  (should-not err))))
            (with-current-buffer buf
              (ert-info ((format "Buffer:\n%S" (buffer-string)))
                (should (string-match-p "Scalpel: done" (buffer-string)))
                (should (string-match-p "User: trigger settle failure"
                                        (buffer-string))))
              (ert-info ((format "busy=%S progress=%S"
                                 scalpel-console--busy
                                 scalpel-llm--progress-callback))
                (should-not scalpel-console--busy)
                (should-not scalpel-llm--progress-callback)))
            (ert-info ((format "Messages: %S" notices))
              (should
               (cl-some
                (lambda (notice)
                  (string-match-p "token accounting failed.*Accounting failed"
                                  notice))
                notices)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-abort-clears-busy-without-llm-request ()
  "Abort clears busy even when no LLM request is in flight.
Regression: `scalpel-console-abort' only cancelled the LLM request,
so a console busy with a synchronous action that had no current
LLM request could not be aborted."
  (let ((scalpel-agent--context-files nil)
        (buf (scalpel-console-test--new-console-buffer))
        pending
        (run-count 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-agent-run)
                     (lambda (_instruction _history on-done _on-error)
                       (setq pending on-done)
                       (cl-incf run-count))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "start operation\n")
              (goto-char (point-min))
              (scalpel-console-send-line))
            (with-current-buffer buf
              (ert-info ("busy must hold once send-line has returned")
                (should scalpel-console--busy)
                (should-not scalpel-llm--cancel-current))
              (scalpel-console-abort)
              (ert-info ((format "busy=%S after abort"
                                 scalpel-console--busy))
                (should-not scalpel-console--busy)))
            ;; A late callback from the aborted operation must not write.
            (funcall pending
                     '(:report "late result"
                       :shells ((:command "late" :bytes 0))
                       :reads nil
                       :changes nil))
            (with-current-buffer buf
              (ert-info ((format "Buffer:\n%S" (buffer-string)))
                (should-not (string-match-p "late result" (buffer-string)))
                (should-not scalpel-console--busy))
              (ert-info ((format "run-count=%d" run-count))
                (should (= run-count 1))))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-quit-during-dispatch-releases-busy ()
  "A `quit' during dispatch still releases the busy guard.
Regression: a synchronous `quit' during `scalpel-agent-run' left
`scalpel-console--busy' set and the status line in place."
  (let ((scalpel-agent--context-files nil)
        (buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'scalpel-agent-run)
                     (lambda (_instruction _history _on-done _on-error)
                       (signal 'quit nil))))
            (with-current-buffer buf
              (erase-buffer)
              (insert "interrupt me\n")
              (goto-char (point-min))
              (let ((err (condition-case e
                             (scalpel-console-send-line)
                           (quit e))))
                (ert-info ((format "send-line quit: %S" err))
                  (should err)))
              (ert-info ((format "busy=%S progress=%S"
                                 scalpel-console--busy
                                 scalpel-llm--progress-callback))
                (should-not scalpel-console--busy)
                (should-not scalpel-llm--progress-callback)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-restore-does-not-invent-a-nil-binding ()
  "A variable the snapshot did not capture is left as the reload left it.
Regression: the restore set every entry the snapshot held, so a
variable that was unbound when the snapshot was taken came back as a
nil binding.  That is the wrong answer twice over: the module's own
`defvar' has already run by then and never overwrites a symbol that is
already bound, and a nil in the token accounting table is the
`wrong-type-argument' on `hash-table-p' that every later round
reports -- the table's readers meet it through `gethash' and
`clrhash'."
  (let ((snapshot (list :version scalpel-console--session-format-version
                        :buffers nil
                        :globals (list (cons 'scalpel-token--console-totals
                                             nil))))
        (scalpel-token--console-totals (make-hash-table :test 'equal)))
    (scalpel-console--session-restore snapshot)
    (ert-info ((format "Value: %S" scalpel-token--console-totals))
      (should (hash-table-p scalpel-token--console-totals)))))

(ert-deftest scalpel-console-test-snapshot-separates-nil-from-unbound ()
  "A captured nil is captured; that answer is not \"unbound\".
The snapshot's value is wrapped in a list for exactly this reason: a
variable that held nil is session state and has to come back, while a
nil that means \"nothing was captured\" must leave the fresh binding
alone."
  (let ((buf (scalpel-console-test--new-console-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (setq scalpel-console--last-instruction nil)
          (let* ((snapshot (scalpel-console--session-snapshot))
                 (entry (cdr (assq (buffer-name buf)
                                   (plist-get snapshot :buffers))))
                 (captured (cdr (assq 'scalpel-console--last-instruction
                                      entry))))
            (ert-info ((format "Captured: %S" captured))
              (should captured)
              (should (null (car captured))))
            ;; And the nil it was comes back as a nil.
            (setq scalpel-console--last-instruction "stale")
            (scalpel-console--session-restore snapshot)
            (should (null scalpel-console--last-instruction))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(ert-deftest scalpel-console-test-restore-refuses-another-format ()
  "A snapshot in another format is refused, never read.
Regression: the snapshot is written by the code loaded at that moment
and read back by the code taken from disk a moment later, so a file
edited between the two is restored by a newer reader than its writer.
The shapes differ -- a captured value is wrapped in a list -- and the
reader met a bare string where it expected a pair: the console's own
root, the first variable in `scalpel-console--session-variables',
reached `car' as text and signalled `wrong-type-argument', which
killed the reload before it had put a single variable back."
  (let ((buf (scalpel-console-test--new-console-buffer))
        (notices nil))
    (unwind-protect
        (with-current-buffer buf
          (let* ((root scalpel-console--root)
                 ;; The shape before the version tag: each entry is
                 ;; (VAR . VALUE), so the console's root is one bare
                 ;; string rather than a list holding it.
                 (snapshot
                  (list :buffers
                        (list (cons (buffer-name buf)
                                    (list (cons 'scalpel-console--root
                                                root))))
                        :globals nil)))
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) notices))))
              (scalpel-console--session-restore snapshot))
            (ert-info ((format "Root: %S Notices: %S"
                               scalpel-console--root notices))
              (should (equal scalpel-console--root root))
              (should (cl-some (lambda (notice)
                                 (string-match-p "snapshot is format" notice))
                               notices)))))
      (scalpel-utils-test-kill-buffer (buffer-name buf)))))

(provide 'scalpel-console-test)

;;; scalpel-console-test.el ends here
