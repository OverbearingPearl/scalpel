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
    (cl-letf (((symbol-function 'scalpel-llm-request)
               (lambda (_prompt &optional _system)
                 "(defun foo (x)\n  (+ x 2))")))
      (let ((action (list :tool "edit"
                          :file this-file
                          :symbol "foo"
                          :instruction "increment x"
                          :text nil)))
        (let ((report (scalpel-agent-execute-action action)))
          (should (string-match "Edited foo" report))
          (with-current-buffer (find-file-noselect this-file)
            (should (string= (buffer-string) "(defun foo (x)\n  (+ x 2))\n"))))))))

(ert-deftest scalpel-agent-test-execute-action-reply ()
  "Execute a reply action and return its text."
  (let ((action (list :tool "reply"
                      :file nil
                      :symbol nil
                      :instruction nil
                      :text "Hello, world!")))
    (should (string= (scalpel-agent-execute-action action) "Hello, world!"))))

(ert-deftest scalpel-agent-test-execute-action-unknown ()
  "Unknown action tool returns a descriptive message."
  (let ((action (list :tool "unknown"
                      :file nil
                      :symbol nil
                      :instruction nil
                      :text nil)))
    (should (string-match "Unknown action" (scalpel-agent-execute-action action)))))

(ert-deftest scalpel-agent-test-edit-malformed ()
  "Malformed edit action (missing fields) signals user-error."
  (should-error
   (scalpel-agent-edit nil nil nil)
   :type 'error))

(ert-deftest scalpel-agent-test-edit-rejects-prose-response ()
  "When LLM returns prose instead of code, no edit is applied."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request)
               (lambda (&rest _ignore)
                 "There are no occurrences of `(+ x 1)` in the body.")))
      (should-error
       (scalpel-agent-edit this-file "foo" "replace x with y")
       :type 'user-error)
      (with-current-buffer (find-file-noselect this-file)
        (should (string= (buffer-string)
                         "(defun foo (x)\n  (+ x 1))\n"))))))

(ert-deftest scalpel-agent-test-parse-json-single-object ()
  "A single JSON action object should be accepted and wrapped."
  (let ((actions (scalpel-agent--parse-json
                  "{\"tool\":\"reply\",\"text\":\"hi\"}")))
    (should (= (length actions) 1))
    (should (equal (plist-get (car actions) :text) "hi"))))

(ert-deftest scalpel-agent-test-context-add-readonly-single-file ()
  "Adding a single file as read-only places it in the readonly list only."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil))
    (scalpel-utils-test-with-temp-file ".el"
      (with-temp-file this-file (insert "(defun foo ())"))
      (scalpel-agent-context-add-readonly this-file)
      (should (equal scalpel-agent--context-readonly-files
                     (list (expand-file-name this-file))))
      (should (null scalpel-agent--context-files)))))

(ert-deftest scalpel-agent-test-context-renders-readonly-content ()
  "Read-only files render with their full contents in the context."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil))
    (scalpel-utils-test-with-temp-file ".el"
      (with-temp-file this-file (insert "hello"))
      (setq scalpel-agent--context-readonly-files
            (list (expand-file-name this-file)))
      (let ((ctx (scalpel-agent-context)))
        (should (string-match-p "FILE (READONLY): " ctx))
        (should (string-match-p "CONTENT:\nhello" ctx))))))

(ert-deftest scalpel-agent-test-context-omits-symbols-without-provider ()
  "Files without a locator provider render without a SYMBOLS line."
  (let ((scalpel-agent--context-files '("/tmp/notes.md"))
        (scalpel-agent--context-readonly-files nil))
    (should (string= (scalpel-agent-context) "FILE: /tmp/notes.md"))))

(ert-deftest scalpel-agent-test-context-add-moves-between-lists ()
  "Adding a file as writable removes it from the readonly list and vice versa."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil))
    (scalpel-utils-test-with-temp-file ".el"
      (with-temp-file this-file (insert "(defun foo ())"))
      (scalpel-agent-context-add-readonly this-file)
      (should (member (expand-file-name this-file)
                      scalpel-agent--context-readonly-files))
      (scalpel-agent-context-add this-file)
      (should (null scalpel-agent--context-readonly-files))
      (should (member (expand-file-name this-file)
                      scalpel-agent--context-files))
      (scalpel-agent-context-add-readonly this-file)
      (should (null scalpel-agent--context-files))
      (should (member (expand-file-name this-file)
                      scalpel-agent--context-readonly-files)))))

(ert-deftest scalpel-agent-test-context-remove-by-directory-prefix ()
  "Removing a directory removes all files beneath it from both lists."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil)
        (dir (make-temp-file "scalpel-test-dir-" t)))
    (unwind-protect
        (let ((f1 (expand-file-name "a.el" dir))
              (f2 (expand-file-name "b.el" dir)))
          (with-temp-file f1 (insert "(defun a ())"))
          (with-temp-file f2 (insert "(defun b ())"))
          (scalpel-agent-context-add f1)
          (scalpel-agent-context-add-readonly f2)
          (should (= (length scalpel-agent--context-files) 1))
          (should (= (length scalpel-agent--context-readonly-files) 1))
          (scalpel-agent-context-remove dir)
          (should (null scalpel-agent--context-files))
          (should (null scalpel-agent--context-readonly-files)))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-edit-refuses-readonly ()
  "Editing a read-only context file signals user-error."
  (let ((scalpel-agent--context-files nil)
        (scalpel-agent--context-readonly-files nil))
    (scalpel-utils-test-with-temp-file ".el"
      (with-temp-file this-file (insert "(defun foo (x)\n  (+ x 1))\n"))
      (setq scalpel-agent--context-readonly-files
            (list (expand-file-name this-file)))
      (cl-letf (((symbol-function 'scalpel-llm-request)
                 (lambda (_prompt &optional _system)
                   "(defun foo (x)\n  (+ x 2))")))
        (should-error
         (scalpel-agent-edit this-file "foo" "increment x")
         :type 'user-error)))))

(ert-deftest scalpel-agent-test-context-add-remove ()
  "Add dedupes and normalizes; remove of absent file does not error."
  (let ((scalpel-agent--context-files nil)
        (file (make-temp-file "scalpel-test-" nil ".el")))
    (unwind-protect
        (progn
          (scalpel-agent-context-add file)
          (scalpel-agent-context-add file)  ; dedupe
          (should (equal scalpel-agent--context-files
                         (list (expand-file-name file))))
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
                           (sort (list file
                                       (expand-file-name "notes.md" dir))
                                 #'string<)))))
      (delete-directory dir t)
      (scalpel-utils-test-delete-file other))))

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
  (let ((scalpel-agent--context-files '("/a/b/c.el"))
        (scalpel-agent--context-readonly-files nil))
    (should (string= (scalpel-agent-context-summary)
                     "└── /a/b/\n    └── c.el"))))

(ert-deftest scalpel-agent-test-context-summary-tree ()
  "Summary nests files, orders directories first, marks attributes."
  (let ((scalpel-agent--context-files
         '("/repo/lisp/a.el" "/repo/lisp/c.el" "/repo/z.el"))
        (scalpel-agent--context-readonly-files '("/repo/lisp/notes.txt")))
    (should (string=
             (scalpel-agent-context-summary "/repo" '("/repo/lisp/a.el"))
             (string-join
              '("└── /repo/"
                "    ├── lisp/"
                "    │   ├── a.el (gitignored)"
                "    │   ├── c.el"
                "    │   └── notes.txt (read-only)"
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
         '("/repo/lisp/a.el" "/other/b.el"))
        (scalpel-agent--context-readonly-files nil))
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
            (should (member (expand-file-name "keep.el" dir) files))
            (should-not (member (expand-file-name "drop.log" dir) files))))
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
  (let ((scalpel-agent--context-files '("/a/one.el"))
        (scalpel-agent--context-readonly-files nil))
    (let ((cells (car (scalpel-agent-context-update 'none-yet nil))))
      (should cells)
      (should (cl-every (lambda (cell)
                          (eq (plist-get cell :status) 'same))
                        cells)))))

(ert-deftest scalpel-agent-test-context-update-marks-changes ()
  "Sibling additions leave existing files unmarked; drops are removed."
  (let* ((scalpel-agent--context-files '("/a/one.el"))
         (scalpel-agent--context-readonly-files nil)
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
  (let ((scalpel-agent--context-files '("/a/one.el"))
        (scalpel-agent--context-readonly-files nil))
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

(provide 'scalpel-agent-test)

;;; scalpel-agent-test.el ends here
