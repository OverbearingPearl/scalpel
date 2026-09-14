;;; scalpel-agent-test.el --- Tests for scalpel-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-agent.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-agent)
(require 'scalpel-execute)
(require 'scalpel-llm-dialect)
(require 'scalpel-locate)
(require 'scalpel-locate-elisp)
(require 'scalpel-utils-test)

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

(ert-deftest scalpel-agent-test-execute-action-create-separates-blocks ()
  "A create action lands after its anchor with blank lines around it.
One blank line separates it on each side.  The layout is applied by
`scalpel-execute-insert-after' on disk, not negotiated through the
prompt."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n\n(defun bar ()\n  nil)\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success "(defun baz ()\n  t)"))))
      (let (report)
        (scalpel-agent-execute-action
         (list :tool "create" :file this-file :symbol "baz"
               :instruction "add baz" :after "foo")
         (lambda (r) (setq report r))
         (lambda (err) (ert-fail (plist-get err :message))))
        (ert-info ((format "Report: %S" report))
          (should (string-match "Created baz" report)))
        (let ((on-disk (with-temp-buffer
                         (insert-file-contents this-file)
                         (buffer-string))))
          (ert-info ((format "On disk:\n%S" on-disk))
            (should (string= on-disk
                             (concat "(defun foo (x)\n  (+ x 1))\n\n"
                                     "(defun baz ()\n  t)\n\n"
                                     "(defun bar ()\n  nil)\n")))))))))

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

(ert-deftest scalpel-agent-test-usable-replacement-extracts-suffix-definition ()
  "A definition buried under leading non-code lines is still usable.
Characterization: the model answered a block-replacement request
with the whole file -- file header, Commentary and all -- with the
defun at the end.  The prefix search cannot reach it, so the
replacement search must also try suffixes, or such a reply is
refused outright."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n"))
    (let* ((reply (concat ";;; llm-pick.el --- demo -*- lexical-binding: t; -*-\n"
                          ";; Author: someone\n"
                          ";;; Commentary:\n"
                          ";; Words.\n"
                          ";;; Code:\n"
                          "(require 'json)\n"
                          "(defun foo (x)\n  (+ x 2))\n"))
           (usable (scalpel-agent--usable-replacement this-file reply)))
      (ert-info ((format "Usable:\n%S" usable))
        (should (scalpel-locate-single-definition-p this-file usable))
        (should (string= usable "(defun foo (x)\n  (+ x 2))"))))))

(ert-deftest scalpel-agent-test-usable-replacement-drops-trailing-prose ()
  "A definition followed by prose keeps only the definition.
Characterization: the model answered with the correct replacement
and then explained it.  The prefix search must stop at the end of
the definition; the whole reply fails
`scalpel-locate-single-definition-p' and would otherwise be
refused outright, which is the shape of the observed planner
failure that surfaced as a prose-reply error."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n"))
    (let* ((reply (concat "(defun foo (x)\n  (+ x 2))\n\n"
                          "This increments x by 2 instead of 1. "
                          "Let me know if you want a different step."))
           (usable (scalpel-agent--usable-replacement this-file reply)))
      (ert-info ((format "Usable:\n%S" usable))
        (should (scalpel-locate-single-definition-p this-file usable))
        (should (string= usable "(defun foo (x)\n  (+ x 2))"))))))

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
  (let ((parsed (scalpel-llm-dialect--default-parse
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
   (scalpel-agent--validate-action '(:tool "read" :symbol "foo"))
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
  (let ((scalpel-agent--context-files '("/tmp/notes.unknown")))
    (should (string= (scalpel-agent-context) "FILE: /tmp/notes.unknown"))))

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

(ert-deftest scalpel-agent-test-plan-reports-tool-call-reply-as-its-own-type ()
  "A reply written as a tool call is reported as `tool-call', not `parse'.
Regression: both arrived as `parse', so the console answered a
deterministic model failure with retry advice -- advice the user
followed three times without the reply changing."
  ;; Dispatch reads the session's own backend and model, so a dialect
  ;; registered for them would decide this test's outcome.  None is
  ;; registered here: the subject is the default parser's refusal.
  (let ((scalpel-llm-dialect-providers nil))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success
                          (concat "<tool_call>shell<arg_key>command</arg_key>"
                                  "<arg_value>ls</arg_value></tool_call>")))))
      (let (error)
        (scalpel-agent-plan
         "look around" nil
         (lambda (_actions) (ert-fail "a tool-call reply must not plan"))
         (lambda (err) (setq error err)))
        (ert-info ((format "Error: %S" error))
          (should (eq (plist-get error :type) 'tool-call))
          (should (string-match-p "tool-call syntax"
                                  (plist-get error :message))))))))

(ert-deftest scalpel-agent-test-plan-degrades-a-prose-reply-to-a-reply-action ()
  "A reply written as prose is delivered as a reply action, not refused.
Regression: a prose reply was reported as a planner error and the
whole round was thrown away, so an answer the model had already
written -- such as instructions it could not execute itself --
never reached the user.  The prose now degrades to a reply action
whose text keeps the original answer readable."
  ;; Dispatch reads the session's own backend and model, so a dialect
  ;; registered for them would decide this test's outcome.  None is
  ;; registered here: the subject is the parser's own report.
  (let ((scalpel-llm-dialect-providers nil))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success
                          (concat "The dependency lives in two layers.\n\n"
                                  "**The gateway** is the hard coupling: it\n"
                                  "calls gptel.\n")))))
      (let (actions)
        (scalpel-agent-plan
         "analyse the dependency" nil
         (lambda (a) (setq actions a))
         (lambda (err) (ert-fail (plist-get err :message))))
        (ert-info ((format "Actions: %S" actions))
          (should (= (length actions) 1))
          (should (equal (plist-get (car actions) :tool) "reply"))
          ;; The answer stays readable: it is the whole evidence the
          ;; user has of what the planner wrote instead of an array.
          (should (string-match-p "The gateway"
                                  (plist-get (car actions) :text)))
          (should-not (string-match-p "\\\\n"
                                      (plist-get (car actions) :text))))))))

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

(ert-deftest scalpel-agent-test-system-prompt-bounds-reply-text ()
  "The prompt must bound how long a reply may be.
Nothing in the code can bound what the model writes, so the bound
has to be stated to the model; the failure it prevents, a reply cut
off mid-JSON by the backend's output limit, cannot be reproduced
here because every reply in this suite is mocked.  This guards only
that the live prompt still carries the rule, so a rewrite that drops
it fails here instead of in a session."
  (ert-info ((format "Rule:\n%S" scalpel-agent--reply-brevity-rule))
    (should (string-match-p
             (regexp-quote scalpel-agent--reply-brevity-rule)
             scalpel-agent-system-prompt))))

(ert-deftest scalpel-agent-test-prompt-example-parses ()
  "The example the system prompt shows is one the parser accepts.
Regression: the prompt presented prose before the array as an
outright failure, while `scalpel-llm-dialect--default-parse' digs
the array out of surrounding prose and a test asserts it does, so
the prompt described a system other than this one."
  (let ((parsed (scalpel-llm-dialect--default-parse
                 scalpel-agent--prompt-example)))
    (ert-info ((format "Example: %S" scalpel-agent--prompt-example))
      (should (equal (plist-get (car parsed) :tool) "reply")))))

(ert-deftest scalpel-agent-test-system-prompt-denies-tool-calling ()
  "The prompt must deny that the planner has tools to call.
Regression: the observed planner failure -- twice, on two models --
is a reply written as a tool call instead of an action array, and
the prompt said nothing about a tool-calling prior while spending
its emphasis on greetings.  This check is a proxy: it accepts any
wording that denies the capability, because the property guarded
is the denial, not a phrase."
  (let ((prompt (downcase scalpel-agent-system-prompt)))
    (ert-info ((format "Prompt:\n%S" prompt))
      (should (cl-some (lambda (marker) (string-match-p marker prompt))
                       '("no tools" "no function to call"
                         "not dispatched as a tool call"))))))

(ert-deftest scalpel-agent-test-edit-prompt-asks-in-the-file-language ()
  "The edit prompt names no language of its own.
Regression: it asked for \"plain Emacs Lisp text\" while the
locator layer serves Markdown, YAML and .gitignore too, so an edit
aimed at a heading section was told to answer in a language the
file is not written in; the replacement is validated per language
by `scalpel-locate-single-definition-p'."
  (scalpel-utils-test-with-temp-file ".md"
    (with-temp-file this-file (insert "# Alpha\nbody\n\n# Beta\n"))
    (let ((prompt nil))
      (cl-letf (((symbol-function 'scalpel-llm-request-async)
                 (lambda (p _on-success on-error &optional _system)
                   (setq prompt p)
                   (funcall on-error (list :type 'test :message "stop")))))
        (scalpel-agent-edit this-file "Alpha" "tighten the wording"
                            (lambda (_report) nil)
                            (lambda (_err) nil)))
      (ert-info ((format "Prompt:\n%S" prompt))
        (should prompt)
        (should-not (string-match-p "Emacs Lisp" prompt))))))

(ert-deftest scalpel-agent-test-create-prompt-requests-no-change-sentinel ()
  "The create prompt asks for the sentinel the code compares against.
Regression: `scalpel-agent-create' tested the reply against
`scalpel-agent--no-change-sentinel', but its prompt never asked for
it, so \"nothing should be created\" had no way to be said and the
model could only answer with a definition that should not exist."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let ((prompt nil))
      (cl-letf (((symbol-function 'scalpel-llm-request-async)
                 (lambda (p _on-success on-error &optional _system)
                   (setq prompt p)
                   (funcall on-error (list :type 'test :message "stop")))))
        (scalpel-agent-create this-file "bar" "add bar" "foo"
                              (lambda (_report) nil)
                              (lambda (_err) nil)))
      (ert-info ((format "Prompt:\n%S" prompt))
        (should prompt)
        (should (string-match-p
                 (regexp-quote scalpel-agent--no-change-sentinel)
                 prompt))))))

(ert-deftest scalpel-agent-test-cod-prompt-keeps-the-output-contract ()
  "Enabling the CoD draft leaves the whole output contract in place.
The draft is scoped to precede the array, so the appended text must
not displace the schema, the example or the brevity rule; a draft
that replaced any of them would move the array out of first place
inside the same system message."
  (let ((scalpel-agent-cod-enabled t)
        (system nil))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt _on-success _on-error &optional sys)
                 (setq system sys))))
      (scalpel-agent-plan "hi" nil
                          (lambda (_actions) nil)
                          (lambda (_err) nil)))
    (ert-info ((format "System prompt:\n%S" system))
      (should system)
      (should (string-match-p (regexp-quote scalpel-agent-cod-prompt)
                              system))
      (should (string-match-p (regexp-quote scalpel-agent--prompt-example)
                              system))
      (should (string-match-p
               (regexp-quote scalpel-agent--reply-brevity-rule)
               system)))))

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

(ert-deftest scalpel-agent-test-system-prompt-prefers-perl ()
  "The prompt steers text-transformation commands toward perl.
Nothing in the code can make the planner pick a portable tool, so
the preference has to be stated in the prompt; this guards only
that the live prompt still carries the rule, so a rewrite that
drops it fails here instead of in a session against BSD sed."
  (ert-info ((format "Prompt excerpt:\n%S"
                     (substring scalpel-agent-system-prompt 0 0)))
    (should (string-match-p "command -v perl"
                            scalpel-agent-system-prompt))
    (should (string-match-p "perl -pi -e"
                            scalpel-agent-system-prompt))))

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

(ert-deftest scalpel-agent-test-confirm-returns-text ()
  "A confirm action hands its text back as the confirmation request."
  (should (string= (scalpel-agent-confirm "proceed?") "proceed?")))

(ert-deftest scalpel-agent-test-confirm-malformed ()
  "A confirm action without text signals `user-error'."
  (should-error (scalpel-agent-confirm nil) :type 'user-error))

(ert-deftest scalpel-agent-test-rename-moves-file-and-buffer ()
  "A rename moves the file and the visiting buffer follows."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let* ((to (concat this-file "-renamed.el"))
           (buf (find-file-noselect this-file)))
      (unwind-protect
          (progn
            (with-current-buffer buf (insert "x") (set-buffer-modified-p nil))
            (let ((report (scalpel-agent-rename this-file to)))
              (ert-info ((format "Report: %S" report))
                (should (string-match-p "Renamed" report))))
            (should (file-exists-p to))
            (should-not (file-exists-p this-file))
            (should (string= (buffer-file-name buf) to)))
        (scalpel-utils-test-kill-file-buffer to)
        (scalpel-utils-test-delete-file to)))))

(ert-deftest scalpel-agent-test-rename-refuses-bad-input ()
  "A rename of a missing source or onto an existing target signals."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "x"))
    (should-error (scalpel-agent-rename "/nonexistent/x.el" "/tmp/y.el")
                  :type 'user-error)
    (should-error (scalpel-agent-rename this-file this-file)
                  :type 'user-error)
    (should-error (scalpel-agent-rename nil nil) :type 'user-error)))

(ert-deftest scalpel-agent-test-delete-file-removes-from-disk ()
  "A `delete-file' removes the file and kills its unmodified buffer."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())"))
    (find-file-noselect this-file)
    (let ((report (scalpel-agent-delete-file this-file)))
      (ert-info ((format "Report: %S" report))
        (should (string-match-p "Deleted file" report)))
      (should-not (file-exists-p this-file))
      (should-not (get-file-buffer this-file)))))

(ert-deftest scalpel-agent-test-delete-file-refuses-unsaved-changes ()
  "A `delete-file' of a file with unsaved changes is refused."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())"))
    (let ((buf (find-file-noselect this-file)))
      (with-current-buffer buf (insert "unsaved"))
      (should-error (scalpel-agent-delete-file this-file)
                    :type 'user-error)
      (should (file-exists-p this-file))
      (with-current-buffer buf (set-buffer-modified-p nil)))))

(ert-deftest scalpel-agent-test-delete-file-refuses-missing ()
  "A `delete-file' of an absent file signals; so does a malformed action."
  (should-error (scalpel-agent-delete-file "/nonexistent/x.el")
                :type 'user-error)
  (should-error (scalpel-agent-delete-file nil) :type 'user-error))

(ert-deftest scalpel-agent-test-execute-action-rename-and-delete-file ()
  "Rename and `delete-file' actions settle synchronously through reports."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "x"))
    (let ((scalpel-agent-confirm-tools nil)
          report)
      (scalpel-agent-execute-action
       (list :tool "rename" :file this-file
             :to (concat this-file "-moved.el"))
       (lambda (r) (setq report r))
       (lambda (err) (ert-fail (plist-get err :message))))
      (should (string-match-p "Renamed" report))
      (scalpel-agent-execute-action
       (list :tool "delete-file" :file (concat this-file "-moved.el"))
       (lambda (r) (setq report r))
       (lambda (err) (ert-fail (plist-get err :message))))
      (should (string-match-p "Deleted file" report)))))

(ert-deftest scalpel-agent-test-execute-action-confirm ()
  "A confirm action delivers its text through ON-SUCCESS."
  (let ((report nil))
    (scalpel-agent-execute-action
     (list :tool "confirm" :text "shall I?")
     (lambda (r) (setq report r))
     (lambda (err) (ert-fail (plist-get err :message))))
    (should (string= report "shall I?"))))

(ert-deftest scalpel-agent-test-validate-action-unknown-tool ()
  "An action whose tool has no field contract signals `user-error'."
  (should-error (scalpel-agent--validate-action '(:tool "nope"))
                :type 'user-error))

(ert-deftest scalpel-agent-test-action-summary-falls-back-to-file-and-reason ()
  "The summary falls back through file, then reason, then a placeholder."
  (should (string= (scalpel-agent--action-summary
                    '(:tool "delete-file" :file "/tmp/a.el"))
                   "/tmp/a.el"))
  (should (string= (scalpel-agent--action-summary
                    '(:tool "shell" :reason "look around"))
                   "look around"))
  (should (string= (scalpel-agent--action-summary '(:tool "shell"))
                   "no reason")))

(ert-deftest scalpel-agent-test-edit-missing-symbol-reports-through-on-error ()
  "An edit naming a symbol the file does not hold settles via ON-ERROR.
Regression: the initial locate ran outside the error guard, so the
`user-error' escaped the callback contract and the console reported
a raw error instead of a planner failure the history could read."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo (x)\n  (+ x 1))\n"))
    (let (error)
      (scalpel-agent-edit
       this-file "gone" "do nothing"
       (lambda (_r) (ert-fail "a missing symbol must not edit"))
       (lambda (e) (setq error e)))
      (ert-info ((format "Error: %S" error))
        (should (eq (plist-get error :type) 'locate))
        (should (string-match-p "not found"
                                (plist-get error :message)))))))

(ert-deftest scalpel-agent-test-edit-rename-announces-new-name ()
  "An edit whose replacement renames the definition says so in the report.
Regression: the replacement validator accepted a definition under a
new name, the report still named the old symbol, and the next round
located the old name and failed with `not found'.  The report is the
only channel that tells the planner the old symbol no longer exists."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo (x)\n  (+ x 1))\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success "(defun bar (x)\n  (+ x 2))"))))
      (let (report)
        (scalpel-agent-edit
         this-file "foo" "rename to bar"
         (lambda (r) (setq report r))
         (lambda (err) (ert-fail (plist-get err :message))))
        (ert-info ((format "Report: %S" report))
          (should (string-match-p "Edited foo" report))
          (should (string-match-p "now named bar" report)))))))

(ert-deftest scalpel-agent-test-edit-changed-body-reports-through-on-error ()
  "A region changed in flight settles via ON-ERROR, not a raw signal.
Regression: the second locate, inside `--apply-if-unchanged', also
ran outside the error guard and escaped the callback contract."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo (x)\n  (+ x 1))\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 ;; Change the file after the prompt captured its body,
                 ;; so the re-verified range no longer matches.  Written
                 ;; through the visiting buffer, never `with-temp-file':
                 ;; an outside write desyncs the visiting buffer's
                 ;; modtime and makes the run prompt to reread from
                 ;; disk, blocking unattended tests.  Clearing the
                 ;; modified flag keeps the change "on disk" in Emacs's
                 ;; view without saving.
                 (with-current-buffer (find-file-noselect this-file)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert "(defun foo (x)\n  (+ x 99))\n"))
                   (set-buffer-modified-p nil))
                 (funcall on-success "(defun foo (x)\n  (+ x 2))"))))
      (let (error)
        (scalpel-agent-edit
         this-file "foo" "increment x"
         (lambda (_r) (ert-fail "a changed region must not edit"))
         (lambda (e) (setq error e)))
        (ert-info ((format "Error: %S" error))
          (should (eq (plist-get error :type) 'locate)))))))

(ert-deftest scalpel-agent-test-replacement-name-reads-the-defined-name ()
  "The replacement name reader returns the first defined name, or nil."
  (should (equal (scalpel-agent--replacement-name "(defun foo (x))")
                 "foo"))
  (should (equal (scalpel-agent--replacement-name "(defvar bar 1)")
                 "bar"))
  (should-not (scalpel-agent--replacement-name "(defun foo)"))
  (should-not (scalpel-agent--replacement-name "not lisp (")))

(ert-deftest scalpel-agent-test-path-components-relative-and-absolute ()
  "Absolute paths keep a root component; relative ones do not."
  (should (equal (scalpel-agent--path-components "/a/b.el")
                 '("/" "a" "b.el")))
  (should (equal (scalpel-agent--path-components "~/x/a.el")
                 '("~" "x" "a.el")))
  (should (equal (scalpel-agent--path-components "a/b.el")
                 '("a" "b.el"))))

(provide 'scalpel-agent-test)

;;; scalpel-agent-test.el ends here
