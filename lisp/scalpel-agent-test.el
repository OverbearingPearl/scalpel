;;; scalpel-agent-test.el --- Tests for scalpel-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-agent.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-agent)
(require 'scalpel-execute)
(require 'scalpel-locate)
(require 'scalpel-locate-elisp)
(require 'scalpel-utils-test)

(ert-deftest scalpel-agent-test-parse-json ()
  "Parse a valid JSON array of actions into a list of plists."
  (let ((raw "[{\"tool\":\"edit\",\"file\":\"/tmp/foo.el\",\"symbol\":\"bar\",\"instruction\":\"do something\"}]"))
    (let ((actions (scalpel-agent--parse-json raw)))
      (should (= (length actions) 1))
      (should (equal (plist-get (car actions) :tool) "edit"))
      (should (equal (plist-get (car actions) :file) "/tmp/foo.el"))
      (should (equal (plist-get (car actions) :symbol) "bar"))
      (should (equal (plist-get (car actions) :instruction) "do something")))))

(ert-deftest scalpel-agent-test-parse-json-invalid ()
  "Invalid JSON should signal a user-error."
  (should-error
   (scalpel-agent--parse-json "not json")
   :type 'error))

(ert-deftest scalpel-agent-test-parse-json-object-not-array ()
  "A JSON object (not an array of actions) should signal user-error."
  (should-error
   (scalpel-agent--parse-json "{\"actions\":[{\"tool\":\"reply\",\"text\":\"hi\"}]}")
   :type 'user-error))

(ert-deftest scalpel-agent-test-parse-json-fenced ()
  "Markdown-fenced JSON should be parsed after stripping fences."
  (let ((actions (scalpel-agent--parse-json
                  "```json\n[{\"tool\":\"reply\",\"text\":\"hi\"}]\n```")))
    (should (equal (plist-get (car actions) :text) "hi"))))

(ert-deftest scalpel-agent-test-parse-json-tolerates-prose-around-payload ()
  "Prose and a fence around the JSON array do not defeat parsing.
Regression: the planner replied with a prose sentence, a json fence,
then the array; stripping only the fences left the prose in place, so
the whole reply went to the JSON parser and a valid plan was rejected
as invalid JSON."
  (let ((actions
         (scalpel-agent--parse-json
          (concat "I'll start by gathering the definitions.\n\n"
                  "```json\n"
                  "[{\"tool\":\"shell\",\"command\":\"ls\",\"reason\":\"look\",\"long-running\":false}]\n"
                  "```\n"))))
    (ert-info ((format "Actions:\n%S" actions))
      (should (= (length actions) 1))
      (should (equal (plist-get (car actions) :command) "ls")))))

(ert-deftest scalpel-agent-test-json-payload-ignores-brackets-in-strings ()
  "Brackets inside JSON strings never end the extracted payload."
  (let ((raw "[{\"tool\":\"shell\",\"command\":\"echo ']'\",\"reason\":\"r\"}] then"))
    (should (string= (scalpel-agent--json-payload raw)
                     "[{\"tool\":\"shell\",\"command\":\"echo ']'\",\"reason\":\"r\"}]"))))

(ert-deftest scalpel-agent-test-json-payload-without-json ()
  "A reply holding no complete JSON value has no payload."
  (should-not (scalpel-agent--json-payload "There is nothing to change."))
  (should-not (scalpel-agent--json-payload "[unterminated")))

(ert-deftest scalpel-agent-test-apply-if-unchanged ()
  "Apply replacement when body is unchanged; abort when it changed."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n"))
    (let* ((range (with-current-buffer (find-file-noselect this-file)
                    (scalpel-locate-elisp--top-definition-range "foo")))
           (beg (car range))
           (end (cdr range))
           (body (with-current-buffer (find-file-noselect this-file)
                   (buffer-substring-no-properties beg end))))
      ;; Unchanged body: should apply
      (let ((report (scalpel-agent--apply-if-unchanged this-file "foo" body "(defun foo (x)\n  (+ x 2))")))
        (should (string-match "Edited foo" report))
        (with-current-buffer (find-file-noselect this-file)
          (should (string= (buffer-string) "(defun foo (x)\n  (+ x 2))\n"))))
      ;; Changed body: should signal
      (let ((modified-body (concat body " ;; modified")))
        (should-error
         (scalpel-agent--apply-if-unchanged this-file "foo" modified-body "(defun foo (x)\n  (+ x 3))")
         :type 'error)))))

(ert-deftest scalpel-agent-test-execute-action-edit ()
  "Execute an edit action by mocking the LLM request."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success "(defun foo (x)\n  (+ x 2))"))))
      (let ((action (list :tool "edit"
                          :file this-file
                          :symbol "foo"
                          :instruction "increment x"
                          :text nil))
            report)
        (scalpel-agent-execute-action
         action
         (lambda (r) (setq report r))
         (lambda (err) (ert-fail (plist-get err :message))))
        (should (string-match "Edited foo" report))
        (with-current-buffer (find-file-noselect this-file)
          (should (string= (buffer-string) "(defun foo (x)\n  (+ x 2))\n")))))))

(ert-deftest scalpel-agent-test-execute-action-delete ()
  "A delete action drops the block, its blank line, and the buffer's state.
The action settles synchronously -- no LLM request is involved -- and
the file on disk must already hold the result."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n\n(defun bar ()\n  nil)\n"))
    (let ((action (list :tool "delete" :file this-file :symbol "foo"))
          report)
      (scalpel-agent-execute-action
       action
       (lambda (r) (setq report r))
       (lambda (err) (ert-fail (plist-get err :message))))
      (ert-info ((format "Report: %S" report))
        (should (string-match "Deleted foo" report)))
      (let ((on-disk (with-temp-buffer
                       (insert-file-contents this-file)
                       (buffer-string))))
        (ert-info ((format "On disk:\n%S" on-disk))
          (should (string= on-disk "(defun bar ()\n  nil)\n")))))))

(ert-deftest scalpel-agent-test-execute-action-reply ()
  "Execute a reply action and deliver its text through ON-SUCCESS."
  (let ((action (list :tool "reply"
                      :file nil
                      :symbol nil
                      :instruction nil
                      :text "Hello, world!"))
        report)
    (scalpel-agent-execute-action
     action
     (lambda (r) (setq report r))
     (lambda (err) (ert-fail (plist-get err :message))))
    (should (string= report "Hello, world!"))))

(ert-deftest scalpel-agent-test-execute-action-unknown ()
  "Unknown action tool is reported through ON-ERROR."
  (let ((action (list :tool "unknown"
                      :file nil
                      :symbol nil
                      :instruction nil
                      :text nil))
        error)
    (scalpel-agent-execute-action
     action
     (lambda (_r) (ert-fail "unknown tool must not succeed"))
     (lambda (e) (setq error e)))
    (should (eq (plist-get error :type) 'unknown-tool))))

(ert-deftest scalpel-agent-test-edit-malformed ()
  "Malformed edit action (missing fields) is reported through ON-ERROR."
  (let (error)
    (scalpel-agent-edit
     nil nil nil
     (lambda (_r) (ert-fail "malformed edit must not succeed"))
     (lambda (e) (setq error e)))
    (should (eq (plist-get error :type) 'malformed))))

(ert-deftest scalpel-agent-test-edit-rejects-prose-response ()
  "When LLM returns prose instead of code, no edit is applied."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success
                          "There are no occurrences of `(+ x 1)` in the body."))))
      (let (error)
        (scalpel-agent-edit
         this-file "foo" "replace x with y"
         (lambda (_r) (ert-fail "prose reply must not produce a report"))
         (lambda (e) (setq error e)))
        (should (eq (plist-get error :type) 'no-replacement)))
      (with-current-buffer (find-file-noselect this-file)
        (should (string= (buffer-string)
                         "(defun foo (x)\n  (+ x 1))\n"))))))

(ert-deftest scalpel-agent-test-parse-json-single-object ()
  "A single JSON action object should be accepted and wrapped."
  (let ((actions (scalpel-agent--parse-json
                  "{\"tool\":\"reply\",\"text\":\"hi\"}")))
    (should (= (length actions) 1))
    (should (equal (plist-get (car actions) :text) "hi"))))

(ert-deftest scalpel-agent-test-parse-json-rejects-tool-call-syntax ()
  "A planner reply that encoded its action as an XML tool call is refused.
Regression: the shell action arrived as an <invoke> block, and the
error reported a bare JSON failure without naming the cause."
  (let ((raw (concat "I'll look around.\n\n"
                     "<invoke name=\"shell\">\n"
                     "<parameter name=\"command\">ls</parameter>\n"
                     "</invoke>")))
    (let ((err (condition-case e
                   (progn (scalpel-agent--parse-json raw) nil)
                 (user-error e))))
      (ert-info ((format "Raw:\n%S" raw))
        (should err)
        (should (string-match-p "tool-call syntax"
                                (error-message-string err)))))))

(ert-deftest scalpel-agent-test-read-whole-file ()
  "A read without a symbol returns the file inside the output fence."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file)))))
      (let ((report (scalpel-agent-read this-file nil)))
        (ert-info ((format "Report:\n%S" report))
          (should (string-match-p "\\`Read: " report))
          (should (string-match-p "\n--- output ---\n" report))
          (should (string-match-p "(defun foo ())" report))
          (should (string-match-p "--- end output ---\\'" report)))))))

(ert-deftest scalpel-agent-test-read-symbol ()
  "A read with a symbol returns that definition, not the whole file."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo ())\n(defun bar ())\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file)))))
      (let ((report (scalpel-agent-read this-file "foo")))
        (ert-info ((format "Report:\n%S" report))
          (should (string-match-p "Read: foo in " report))
          (should (string-match-p "(defun foo ())" report))
          (should-not (string-match-p "bar" report)))))))

(ert-deftest scalpel-agent-test-read-refuses-file-outside-context ()
  "A read through Emacs is bounded by the context list, not the sandbox.
Regression: the sandbox bounds shell commands, but a read runs in
Emacs itself, so without this check the planner could reach any file
the user can read."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let ((scalpel-agent--context-files nil))
      (should-error (scalpel-agent-read this-file nil) :type 'user-error)
      (should-error (scalpel-agent-read this-file "foo")
                    :type 'user-error))))

(ert-deftest scalpel-agent-test-read-refuses-oversized-definition ()
  "An oversized definition is refused, never truncated.
A partial definition can still parse as a complete form, so a
replacement built from one would be applied silently; refusing
keeps the failure loud."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert (format "(defun foo ()\n  (message \"%s\"))\n"
                      (make-string 200 ?x))))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file))))
          (scalpel-agent-read-max-bytes 10))
      (let ((err (condition-case e
                     (progn (scalpel-agent-read this-file "foo") nil)
                   (user-error e))))
        (ert-info ((format "Error: %S" err))
          (should err)
          (should (string-match-p "over the read limit"
                                  (error-message-string err))))))))

(ert-deftest scalpel-agent-test-read-truncates-whole-file-with-marker ()
  "A whole-file read is a partial view and says so.
The marker states the true size, so the planner can tell it is not
holding the whole file."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert (format "(defvar x \"%s\")\n" (make-string 200 ?y))))
    (let* ((scalpel-agent--context-files
            (list (file-truename (expand-file-name this-file))))
           (scalpel-agent-read-max-bytes 20)
           (size (with-temp-buffer
                   (insert-file-contents this-file)
                   (string-bytes (buffer-string))))
           (report (scalpel-agent-read this-file nil)))
      (ert-info ((format "Report:\n%S" report))
        (should (string-match-p
                 (regexp-quote (format "Output: %d bytes" size)) report))
        (should (string-match-p "\\[truncated: showing first " report))
        (should (string-match-p "--- end output ---\\'" report))))))

(ert-deftest scalpel-agent-test-byte-prefix-keeps-characters-whole ()
  "The byte prefix never cuts a character in half."
  (let ((text "中中中"))
    (dotimes (n 10)
      (let ((prefix (scalpel-agent--byte-prefix text n)))
        (ert-info ((format "n=%d prefix=%S" n prefix))
          (should (<= (string-bytes prefix) n))
          (should (string-prefix-p prefix text)))))))

(ert-deftest scalpel-agent-test-read-runs-without-confirmation ()
  "A read has no side effects, so it is never put to the user."
  (should-not (scalpel-agent--confirm-needed-p
               (list :tool "read" :file "/tmp/a.el" :symbol "foo"))))

(ert-deftest scalpel-agent-test-execute-action-read ()
  "A read action settles synchronously through ON-SUCCESS."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file))))
          report)
      (scalpel-agent-execute-action
       (list :tool "read" :file this-file :symbol nil)
       (lambda (r) (setq report r))
       (lambda (err) (ert-fail (plist-get err :message))))
      (ert-info ((format "Report:\n%S" report))
        (should (string-match-p "(defun foo ())" report))))))

(ert-deftest scalpel-agent-test-read-symbol-is-optional ()
  "A read action may omit :symbol, and :symbol survives projection.
Regression: `scalpel-agent--project-actions' kept only declared
fields and `scalpel-agent--validate-action' required every declared
one, so an optional field could be neither declared nor dropped."
  (let ((parsed (scalpel-agent--parse-json
                 (concat "[{\"tool\":\"read\",\"file\":\"/tmp/a.el\"},"
                         "{\"tool\":\"read\",\"file\":\"/tmp/a.el\","
                         "\"symbol\":\"foo\"}]"))))
    (should (= (length parsed) 2))
    (should-not (plist-get (car parsed) :symbol))
    (should (equal (plist-get (cadr parsed) :symbol) "foo")))
  (let ((projected (scalpel-agent--project-actions
                    (list (list :tool "read" :file "/tmp/a.el")
                          (list :tool "read" :file "/tmp/a.el" :symbol "foo")))))
    (should-not (plist-get (car projected) :symbol))
    (should (equal (plist-get (cadr projected) :symbol) "foo"))))

(ert-deftest scalpel-agent-test-read-requires-file ()
  "A read action without :file is rejected at validation."
  (should-error
   (scalpel-agent--parse-json "[{\"tool\":\"read\",\"symbol\":\"foo\"}]")
   :type 'user-error))

(ert-deftest scalpel-agent-test-run-records-reads ()
  "A round reports which definitions it read.
Regression: the round result carried only :shells, so a read
produced output that nothing downstream could see, and the console
never ran the round that would have read it back."
  (let ((scalpel-agent--context-files nil)
        (scalpel-console--root nil)
        (default-directory (file-name-as-directory
                            (expand-file-name temporary-file-directory)))
        (orig-llm-request-async (symbol-function 'scalpel-llm-request-async)))
    (unwind-protect
        (progn
          (fset 'scalpel-llm-request-async
                (lambda (_prompt on-success _on-error &optional _system)
                  (funcall on-success
                           (concat "[{\"tool\":\"read\",\"file\":\"/tmp/a.el\","
                                   "\"symbol\":\"foo\"}]"))))
          (let (result)
            (cl-letf (((symbol-function 'scalpel-agent-read)
                       (lambda (file symbol)
                         (format (concat "Read: %s in %s\nOutput: 8 bytes\n"
                                         "--- output ---\n(defun foo ())\n"
                                         "--- end output ---")
                                 symbol file))))
              (scalpel-agent-run
               "read foo" nil
               (lambda (r) (setq result r))
               (lambda (err) (ert-fail (plist-get err :message)))))
            (let ((read (car (plist-get result :reads))))
              (ert-info ((format "Result:\n%S" result))
                (should (equal (plist-get read :file) "/tmp/a.el"))
                (should (equal (plist-get read :symbol) "foo"))
                (should (string-match-p "(defun foo ())"
                                        (plist-get result :report)))))))
      (fset 'scalpel-llm-request-async orig-llm-request-async))))

(ert-deftest scalpel-agent-test-context-omits-symbols-without-provider ()
  "Files without a locator provider render without a SYMBOLS line."
  (let ((scalpel-agent--context-files '("/tmp/notes.md")))
    (should (string= (scalpel-agent-context) "FILE: /tmp/notes.md"))))

(ert-deftest scalpel-agent-test-context-remove-by-directory-prefix ()
  "Removing a directory removes all files beneath it from the context."
  (let ((scalpel-agent--context-files nil)
        (dir (make-temp-file "scalpel-test-dir-" t)))
    (unwind-protect
        (let ((f1 (expand-file-name "a.el" dir))
              (f2 (expand-file-name "b.el" dir)))
          (with-temp-file f1 (insert "(defun a ())"))
          (with-temp-file f2 (insert "(defun b ())"))
          (scalpel-agent-context-add f1)
          (scalpel-agent-context-add f2)
          (should (= (length scalpel-agent--context-files) 2))
          (scalpel-agent-context-remove dir)
          (should (null scalpel-agent--context-files)))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-context-add-remove ()
  "Add dedupes and normalizes; remove of absent file does not error."
  (let ((scalpel-agent--context-files nil)
        (file (make-temp-file "scalpel-test-" nil ".el")))
    (unwind-protect
        (progn
          (scalpel-agent-context-add file)
          (scalpel-agent-context-add file)  ; dedupe
          (should (equal scalpel-agent--context-files
                         (list (file-truename (expand-file-name file)))))
          (scalpel-agent-context-remove (concat file "/nope"))
          (should (= (length scalpel-agent--context-files) 1))
          (scalpel-agent-context-remove file)
          (should (null scalpel-agent--context-files)))
      (scalpel-utils-test-kill-file-buffer file)
      (scalpel-utils-test-delete-file file))))

(ert-deftest scalpel-agent-test-context-add-directory ()
  "Adding a directory expands to contained located files."
  (let ((scalpel-agent--context-files nil)
        (dir (make-temp-file "scalpel-test-dir-" t))
        (other (make-temp-file "scalpel-test-" nil ".unknown")))
    (unwind-protect
        (progn
          (let ((file (expand-file-name "foo.el" dir)))
            (with-temp-file file (insert "(defun foo ())"))
            (with-temp-file (expand-file-name "notes.md" dir) (insert "hi"))
            (scalpel-agent-context-add dir)
            (should (equal (sort (copy-sequence scalpel-agent--context-files)
                                 #'string<)
                           (sort (list (file-truename file)
                                       (file-truename
                                        (expand-file-name "notes.md" dir)))
                                 #'string<)))))
      (delete-directory dir t)
      (scalpel-utils-test-delete-file other))))

(ert-deftest scalpel-agent-test-context-add-rejects-over-limit-whole ()
  "An add that would exceed the file limit is refused as a whole.
Regression: the context had no size bound, so adding a directory put
every file under it into the planner prompt and the sandbox policy,
with no point at which the growth became visible.  The limit is
checked against the merged list, so successive adds cannot walk past
it, and a refusal leaves the context exactly as it was rather than
partially applied."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent-context-max-files 1)
        (first (make-temp-file "scalpel-test-" nil ".el"))
        (second (make-temp-file "scalpel-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file first (insert "(defun first ())"))
          (with-temp-file second (insert "(defun second ())"))
          (scalpel-agent-context-add first)
          (should (= (length scalpel-agent--context-files) 1))
          (should-error (scalpel-agent-context-add second) :type 'user-error)
          (ert-info ((format "Context: %S" scalpel-agent--context-files))
            ;; The refusal must not have kept the file that fit.
            (should (equal scalpel-agent--context-files
                           (list (file-truename (expand-file-name first)))))
            (should-not (member (file-truename (expand-file-name second))
                                scalpel-agent--context-files))))
      (scalpel-utils-test-kill-file-buffer first)
      (scalpel-utils-test-delete-file first)
      (scalpel-utils-test-kill-file-buffer second)
      (scalpel-utils-test-delete-file second))))

(ert-deftest scalpel-agent-test-context-summary-and-empty ()
  "Summary renders a tree; empty context reports 'none'."
  (let ((scalpel-agent--context-files nil))
    (should (string= (scalpel-agent-context-summary) "none"))
    (should (string= (scalpel-agent-context) "No files in context."))
    (let ((scalpel-agent--context-files '("/a.el" "/b.el")))
      (should (string= (scalpel-agent-context-summary)
                       "└── /\n    ├── a.el\n    └── b.el")))))

(ert-deftest scalpel-agent-test-context-summary-compacts-single-child-chains ()
  "Single-child directory chains collapse into one node."
  (let ((scalpel-agent--context-files '("/a/b/c.el")))
    (should (string= (scalpel-agent-context-summary)
                     "└── /a/b/\n    └── c.el"))))

(ert-deftest scalpel-agent-test-context-summary-tree ()
  "Summary nests files, orders directories first, marks attributes."
  (let ((scalpel-agent--context-files
         '("/repo/lisp/a.el" "/repo/lisp/c.el" "/repo/z.el")))
    (should (string=
             (scalpel-agent-context-summary "/repo" '("/repo/lisp/a.el"))
             (string-join
              '("└── /repo/"
                "    ├── lisp/"
                "    │   ├── a.el (gitignored)"
                "    │   └── c.el"
                "    └── z.el")
              "\n")))))

(ert-deftest scalpel-agent-test-git-ignored-files ()
  "Files matched by .gitignore are reported; others are not."
  (skip-unless (executable-find "git"))
  (let ((dir (file-name-as-directory
              (make-temp-file "scalpel-test-repo-" t))))
    (unwind-protect
        (progn
          (let ((default-directory dir)
                (process-environment (scalpel-agent--git-environment)))
            (call-process "git" nil nil nil "init" "-q"))
          (with-temp-file (expand-file-name ".gitignore" dir)
            (insert "*.log\n"))
          (with-temp-file (expand-file-name "keep.el" dir)
            (insert "(defun keep ())\n"))
          (with-temp-file (expand-file-name "drop.log" dir)
            (insert "noise\n"))
          (should (equal (scalpel-agent--git-ignored-files
                          (list (expand-file-name "drop.log" dir)))
                         (list (expand-file-name "drop.log" dir))))
          (should (null (scalpel-agent--git-ignored-files
                         (list (expand-file-name "keep.el" dir))))))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-context-summary-unifies-inside-and-outside ()
  "Files inside and outside ROOT render in one tree rooted at the filesystem root."
  (let ((scalpel-agent--context-files
         '("/repo/lisp/a.el" "/other/b.el")))
    (should (string= (scalpel-agent-context-summary "/repo")
                     (string-join
                      '("└── /"
                        "    ├── other/"
                        "    │   └── b.el"
                        "    └── repo/lisp/"
                        "        └── a.el")
                      "\n")))))

(ert-deftest scalpel-agent-test-git-toplevel-at-repo-root ()
  "The repository root itself is reported as its own git toplevel."
  (skip-unless (executable-find "git"))
  (let ((dir (file-name-as-directory (make-temp-file "scalpel-test-repo-" t))))
    (unwind-protect
        (progn
          (let ((default-directory dir)
                (process-environment (scalpel-agent--git-environment)))
            (call-process "git" nil nil nil "init" "-q"))
          (dolist (candidate (list dir (directory-file-name dir)))
            (let ((top (scalpel-agent--git-toplevel candidate)))
              (should (equal (and top (file-truename top))
                             (file-truename dir))))))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-expanded-files-respects-gitignore-at-repo-root ()
  "Expanding a repository root excludes gitignored files."
  (skip-unless (executable-find "git"))
  (let ((dir (file-name-as-directory (make-temp-file "scalpel-test-repo-" t))))
    (unwind-protect
        (progn
          (let ((default-directory dir)
                (process-environment (scalpel-agent--git-environment)))
            (call-process "git" nil nil nil "init" "-q"))
          (with-temp-file (expand-file-name ".gitignore" dir)
            (insert "*.log\n"))
          (with-temp-file (expand-file-name "keep.el" dir)
            (insert "(defun keep ())\n"))
          (with-temp-file (expand-file-name "drop.log" dir)
            (insert "noise\n"))
          (let ((files (scalpel-agent--expanded-files
                        (directory-file-name dir))))
            ;; `--expanded-files' canonicalizes its input and every path
            ;; it returns, because the sandbox matches its rules against
            ;; the resolved path; the temp directory is reached through
            ;; the `/var -> /private/var' symlink, so the expected names
            ;; must be resolved too or the comparison can never hold.
            (should (member (file-truename (expand-file-name "keep.el" dir))
                            files))
            (should-not (member (file-truename (expand-file-name "drop.log" dir))
                                files))))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-walk-all-files-skips-dot-git ()
  "Walking skips `.git' whether it is a directory or a gitfile."
  (let ((dir (make-temp-file "scalpel-test-walk-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.el" dir)
            (insert "(defun a ())\n"))
          (with-temp-file (expand-file-name ".git" dir)
            (insert "gitdir: ../nowhere\n"))
          (make-directory (expand-file-name "sub" dir))
          (with-temp-file (expand-file-name "sub/b.el" dir)
            (insert "(defun b ())\n"))
          (should (equal (sort (mapcar #'file-name-nondirectory
                                       (scalpel-agent--walk-all-files dir))
                               #'string<)
                         '("a.el" "b.el"))))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-git-toplevel-nil-outside-repo ()
  "A directory outside any repository has no git toplevel.
Guards against the git subprocess inheriting the caller's
`default-directory' or a leaked GIT_* environment."
  (skip-unless (executable-find "git"))
  (let ((dir (file-name-as-directory (make-temp-file "scalpel-test-norepo-" t))))
    (unwind-protect
        (should-not (scalpel-agent--git-toplevel (directory-file-name dir)))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-context-update-first-render-marks-nothing ()
  "Without a baseline, no line is marked as changed."
  (let ((scalpel-agent--context-files '("/a/one.el")))
    (let ((cells (car (scalpel-agent-context-update 'none-yet nil))))
      (should cells)
      (should (cl-every (lambda (cell)
                          (eq (plist-get cell :status) 'same))
                        cells)))))

(ert-deftest scalpel-agent-test-context-update-marks-changes ()
  "Sibling additions leave existing files unmarked; drops are removed."
  (let* ((scalpel-agent--context-files '("/a/one.el"))
         (baseline 'none-yet)
         cells
         (status-of (lambda (name)
                      (plist-get (cl-find name cells
                                          :key (lambda (cell)
                                                 (plist-get cell :text))
                                          :test #'string-match-p)
                                 :status))))
    (let ((result (scalpel-agent-context-update baseline nil)))
      (setq baseline (cdr result)
            cells (car result))
      (should (cl-every (lambda (cell)
                          (eq (plist-get cell :status) 'same))
                        cells)))
    (setq scalpel-agent--context-files '("/a/one.el" "/a/two.el"))
    (let ((result (scalpel-agent-context-update baseline nil)))
      (setq baseline (cdr result)
            cells (car result))
      (should (eq (funcall status-of "one.el") 'same))
      (should (eq (funcall status-of "two.el") 'added)))
    (setq scalpel-agent--context-files '("/a/one.el"))
    (let ((result (scalpel-agent-context-update baseline nil)))
      (setq cells (car result))
      (should (eq (funcall status-of "two.el") 'removed))
      (should (eq (funcall status-of "one.el") 'same)))))

(ert-deftest scalpel-agent-test-context-update-marks-new-directory ()
  "A directory holding only new files is itself marked added."
  (let ((scalpel-agent--context-files '("/a/one.el")))
    (let ((baseline (cdr (scalpel-agent-context-update 'none-yet nil)))
          cells)
      (setq scalpel-agent--context-files '("/a/one.el" "/b/two.el"))
      (setq cells (car (scalpel-agent-context-update baseline nil)))
      (should (eq (plist-get (cl-find "b/" cells
                                      :key (lambda (cell)
                                             (plist-get cell :text))
                                      :test #'string-match-p)
                             :status)
                  'added))
      (should (eq (plist-get (cl-find "a/" cells
                                      :key (lambda (cell)
                                             (plist-get cell :text))
                                      :test #'string-match-p)
                             :status)
                  'same)))))

(ert-deftest scalpel-agent-test-plan-projects-fields-for-tool ()
  "Plan projects only the fields declared for each tool.
Regression: `let' bound `tool' before `fields' used it, so the
field list was always nil and the projected action lost its keys."
  (cl-letf (((symbol-function 'scalpel-llm-request-async)
             (lambda (_prompt on-success _on-error &optional _system)
               (funcall on-success "[{\"tool\":\"reply\",\"text\":\"hi\"}]"))))
    (let (actions)
      (scalpel-agent-plan
       "say hi" nil
       (lambda (a) (setq actions a))
       (lambda (err) (ert-fail (plist-get err :message))))
      (should (= (length actions) 1))
      (should (equal (plist-get (car actions) :tool) "reply"))
      (should (equal (plist-get (car actions) :text) "hi")))))

(ert-deftest scalpel-agent-test-system-prompt-declares-every-tool ()
  "Every dispatchable tool must be declared to the planner.
Regression: `shell' was dispatchable and implemented but absent
from the system prompt, so the planner could never emit it."
  (dolist (tool scalpel-agent--tool-vocabulary)
    (ert-info ((format "Tool %S is not declared in `scalpel-agent-system-prompt'"
                       tool))
      (should (string-match-p
               (format "\"tool\"[ \t]*:[ \t]*\"%s\"" (regexp-quote tool))
               scalpel-agent-system-prompt)))))

(ert-deftest scalpel-agent-test-system-prompt-hides-the-sandbox ()
  "The planner must not be told that commands run under a sandbox.
Regression: the prompt named the OS sandbox, so the planner could
reason about the boundary and probe or route around it; the user
experience is meant to be an ordinary shell with a smaller
filesystem, not a sandboxed one."
  (dolist (word '("sandbox" "bwrap" "bubblewrap" "sandbox-exec"))
    (ert-info ((format "Prompt mentions %S" word))
      (should-not (string-match-p (regexp-quote word)
                                  (downcase scalpel-agent-system-prompt))))))

(ert-deftest scalpel-agent-test-shell-runs-command-through-a-shell ()
  "Shell actions delegate execution to the sandbox and report its status."
  (let ((scalpel-console--root nil)
        (scalpel-agent--context-files nil)
        (default-directory (file-name-as-directory
                            (expand-file-name temporary-file-directory))))
    (cl-letf (((symbol-function 'scalpel-sandbox-run)
               (lambda (command root files)
                 (should (string= command "echo hello | tr a-z A-Z"))
                 (should root)
                 (should (null files))
                 (cons 0 "HELLO\n"))))
      (let ((piped (scalpel-agent-shell "echo hello | tr a-z A-Z"
                                        "check shell semantics")))
      (ert-info ((format "Report:\n%S" piped))
        (should (string-match-p "HELLO" piped)))))
    (cl-letf (((symbol-function 'scalpel-sandbox-run)
               (lambda (&rest _ignore) (cons 3 ""))))
      (let ((failed (scalpel-agent-shell "exit 3" "check exit status")))
      (ert-info ((format "Report:\n%S" failed))
          (should (string-match-p "Exit: 3" failed)))))))

(ert-deftest scalpel-agent-test-action-summary-prefers-target ()
  "Confirmation prompts describe what will run or change.
Regression: the prompt showed only the reason, so a shell command
was confirmed without ever being displayed."
  (should (string= (scalpel-agent--action-summary
                    '(:tool "shell" :command "make test" :reason "run tests"))
                   "make test"))
  (should (string= (scalpel-agent--action-summary
                    '(:tool "edit" :file "/tmp/a.el" :symbol "foo"))
                   "foo in /tmp/a.el"))
  (should (string= (scalpel-agent--action-summary
                    '(:tool "reply" :text "hi"))
                   "hi"))
  (should (string= (scalpel-agent--action-summary
                    '(:tool "confirm" :reason "need input"))
                   "need input")))

(ert-deftest scalpel-agent-test-shell-confirm-gated-by-flags ()
  "Only a long-running shell action asks; every other one runs unattended.
Regression: confirmation was plain list membership, so every
read-only command needed a prompt, and the planner's own
\"read-only\" claim decided the gate even though nothing verified
it."
  (let ((scalpel-agent-confirm-tools '("shell"))
        (asked 0)
        (ran 0))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (setq asked (1+ asked)) t))
              ((symbol-function 'scalpel-agent-shell)
               (lambda (&rest _) (setq ran (1+ ran)) "report")))
      ;; JSON booleans reach this code as `t' and `:false', so the
      ;; test uses those symbols rather than nil to keep the
      ;; `(eq ... t)' check honest.
      (let ((quick '(:tool "shell" :command "rm -rf build"
                           :reason "clean"
                           :long-running :false))
            (long '(:tool "shell" :command "make test" :reason "run"
                          :long-running t))
            report)
        (scalpel-agent-execute-action
         quick
         (lambda (r) (setq report r))
         (lambda (err) (ert-fail (plist-get err :message))))
        (should (string= report "report"))
        (ert-info ((format "asked=%d after a quick action" asked))
          (should (= asked 0)))
        (scalpel-agent-execute-action
         long
         (lambda (r) (setq report r))
         (lambda (err) (ert-fail (plist-get err :message))))
        (should (string= report "report"))
        (ert-info ((format "asked=%d after a long action" asked))
          (should (= asked 1)))
        (should (= ran 2))))))

(ert-deftest scalpel-agent-test-shell-contract-drops-read-only ()
  "The shell contract carries only fields the code still acts on.
Regression: the planner declared \"read-only\", which gated the
confirmation prompt even though nothing verified it; the prompt is
now driven by \"long-running\" alone, so the field must not be
requested from the planner."
  (should-not (memq :read-only (cdr (assoc "shell" scalpel-agent--tool-fields))))
  (should-not (string-match-p "read-only" scalpel-agent-system-prompt)))

(ert-deftest scalpel-agent-test-shell-report-is-delimited ()
  "Shell reports name the command, state the exit status, and end."
  (let ((scalpel-console--root nil)
        (scalpel-agent--context-files nil)
        (default-directory (file-name-as-directory
                            (expand-file-name temporary-file-directory))))
    (cl-letf (((symbol-function 'scalpel-sandbox-run)
               (lambda (&rest _ignore) (cons 0 "hi\n"))))
      (let ((report (scalpel-agent-shell "echo hi" "check delimiters")))
      (ert-info ((format "Report:\n%S" report))
        (should (string-match-p "\\`Shell: echo hi\n" report))
        (should (string-match-p "\nExit: 0\n" report))
        (should (string-match-p "\n--- output ---\n" report))
        (should (string-match-p "--- end output ---\\'" report)))))))

(ert-deftest scalpel-agent-test-shell-report-drops-control-characters ()
  "Control characters in sandbox output never reach the report."
  (let ((scalpel-console--root nil)
        (scalpel-agent--context-files nil)
        (default-directory (file-name-as-directory
                            (expand-file-name temporary-file-directory))))
    (cl-letf (((symbol-function 'scalpel-sandbox-run)
               (lambda (&rest _ignore) (cons 0 "a\ab\n"))))
      (let ((report (scalpel-agent-shell "printf 'a\\ab\\n'" "check controls")))
      (ert-info ((format "Report:\n%S" report))
        (should (string-match-p "\n--- output ---\nab\n" report))
        (should-not (string-match-p "[\0-\10\13-\37\177-\237]" report)))))))

(ert-deftest scalpel-agent-test-prompt-includes-history ()
  "The prompt carries the conversation before the instruction.
Regression: only the context and the newest instruction were sent,
so a follow-up such as \"the third point is wrong\" had no referent."
  (let ((scalpel-agent--context-files nil))
    (let ((prompt (scalpel-agent--prompt "second"
                                         "User: first\nScalpel: reply\n")))
      (ert-info ((format "Prompt:\n%S" prompt))
        (should (string-match-p "Conversation so far:\nUser: first" prompt))
        (should (string-suffix-p "User instruction:\nsecond" prompt))))
    (let ((prompt (scalpel-agent--prompt "first" nil)))
      (ert-info ((format "Prompt:\n%S" prompt))
        (should-not (string-match-p "Conversation so far:" prompt))))))

(ert-deftest scalpel-agent-test-visible-raw-exposes-invisible-bytes ()
  "A reply that fails to parse must expose the bytes that broke it.
Regression: the error used %S, which prints control bytes and NBSP
literally, so the offending character could not be seen."
  (let ((shown (scalpel-agent--visible-raw "a\tb\u00A0c")))
    (ert-info ((format "Shown: %S" shown))
      ;; Nothing invisible may survive: a literal TAB or NBSP in the
      ;; error message is exactly as unreadable as the original.
      (should-not (string-match-p "[\t\u00A0]" shown))
      (should (string-match-p "\\\\" shown)))))

(ert-deftest scalpel-agent-test-shell-report-states-output-size ()
  "The report always states the true output size."
  (let ((scalpel-console--root nil)
        (scalpel-agent--context-files nil)
        (default-directory (file-name-as-directory
                            (expand-file-name temporary-file-directory))))
    (cl-letf (((symbol-function 'scalpel-sandbox-run)
               (lambda (&rest _ignore) (cons 0 "abc"))))
      (let ((report (scalpel-agent-shell "printf abc" "measure output")))
      (ert-info ((format "Report:\n%S" report))
        (should (string-match-p "\nOutput: 3 bytes\n" report)))))))

(ert-deftest scalpel-agent-test-shell-suppresses-binary-output ()
  "Output holding a NUL byte is reported as binary, contents dropped."
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (let ((scalpel-console--root nil)
        (scalpel-agent--context-files nil)
        (default-directory (file-name-as-directory
                            (expand-file-name temporary-file-directory))))
    (cl-letf (((symbol-function 'scalpel-sandbox-run)
               (lambda (&rest _ignore) (cons 0 "a\0b"))))
      (let ((report (scalpel-agent-shell "printf 'a\\000b'" "check binary")))
      (ert-info ((format "Report:\n%S" report))
        (should (string-match-p
                 "\\[binary output suppressed: 3 bytes\\]" report))
        (should-not (string-match-p "a\0b" report)))))))

(ert-deftest scalpel-agent-test-run-records-shell-output-size ()
  "A round reports the raw size of every shell command it ran.
The report preserves the raw output size for continuation decisions."
  (let ((scalpel-agent--context-files nil)
        (scalpel-console--root nil)
        (default-directory (file-name-as-directory
                            (expand-file-name temporary-file-directory)))
        (orig-llm-request-async (symbol-function 'scalpel-llm-request-async))
        (orig-sandbox-run (symbol-function 'scalpel-sandbox-run))
        (orig-agent-shell (symbol-function 'scalpel-agent-shell)))
    (unwind-protect
        (progn
          (fset 'scalpel-llm-request-async
                (lambda (_prompt on-success _on-error &optional _system)
                  (funcall on-success
                           (concat "[{\"tool\":\"shell\",\"command\":\"printf abc\","
                                   "\"reason\":\"size\","
                                   "\"long-running\":false}]"))))
          (fset 'scalpel-sandbox-run
                (lambda (&rest _ignore) (cons 0 "abc")))
          (fset 'scalpel-agent-shell
                (lambda (_command _reason)
                  (setq scalpel-agent--shell-output
                        '(:bytes 3 :truncated nil :binary nil))
                  "Shell: printf abc\nReason: size\nExit: 0\nOutput: 3 bytes"))
          (let (result)
            (scalpel-agent-run
             "measure" nil
             (lambda (r) (setq result r))
             (lambda (err) (ert-fail (plist-get err :message))))
            (let ((shell (car (plist-get result :shells))))
              (ert-info ((format "Result:\n%S" result))
                (should (equal (plist-get shell :command)
                               "printf abc"))
                (should (= (plist-get shell :bytes)
                           3))
                (should-not (plist-get shell :truncated))
                (should-not (plist-get shell :binary))))))
      (fset 'scalpel-llm-request-async orig-llm-request-async)
      (fset 'scalpel-sandbox-run orig-sandbox-run)
      (fset 'scalpel-agent-shell orig-agent-shell))))

(provide 'scalpel-agent-test)

;;; scalpel-agent-test.el ends here
