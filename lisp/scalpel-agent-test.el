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
      (let ((action (list :tool "block-edit"
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
         (list :tool "block-insert" :file this-file :symbol "baz"
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

(ert-deftest scalpel-agent-test-create-reports-a-full-context ()
  "A create the context limit cannot take is reported, not silent.
The file exists on disk either way, so a refusal nobody mentions would
leave the planner unable to read a file the user can see."
  (let ((dir (make-temp-file "scalpel-test-new-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((scalpel-agent--context-files '("/tmp/scalpel-test-held.el"))
                 (scalpel-agent-context-max-files 1)
                 (target (expand-file-name "new.el" dir))
                 (report (scalpel-agent-file-create target "(defun a ())\n")))
            (ert-info ((format "Report: %S Context: %S"
                               report scalpel-agent--context-files))
              (should (file-exists-p target))
              (should (equal scalpel-agent--context-files
                             '("/tmp/scalpel-test-held.el")))
              (should (string-match-p "not added to the context" report)))))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-create-file-joins-the-context ()
  "A created file joins the session context.
Regression: `file-create' wrote the file and left the context alone, so
the file existed on disk while every later round -- which reads and
edits through the context alone -- could not name it."
  (let ((dir (make-temp-file "scalpel-test-new-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((scalpel-agent--context-files nil)
                 (target (expand-file-name "sub/new.el" dir))
                 (report (scalpel-agent-file-create target "(defun a ())\n")))
            (ert-info ((format "Report: %S Context: %S"
                               report scalpel-agent--context-files))
              (should (string-match-p "Created file" report))
              (should (member (file-truename target)
                              scalpel-agent--context-files)))))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-create-reports-the-name-that-landed ()
  "The create report names the definition the file really holds.
Regression: it repeated the symbol the planner asked for, so a planner
whose definition landed under another name was told the create produced
it; the next round then located that name and failed with \"not found\",
with nothing in the record saying why."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun anchor ())\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success "(defun other ()\n  t)"))))
      (let (report)
        (scalpel-agent-execute-action
         (list :tool "block-insert" :file this-file :symbol "wanted"
               :instruction "add wanted" :after "anchor")
         (lambda (r) (setq report r))
         (lambda (err) (ert-fail (plist-get err :message))))
        (ert-info ((format "Report: %S On disk: %S" report
                           (with-temp-buffer
                             (insert-file-contents this-file)
                             (buffer-string))))
          (should (string-match-p "Created wanted" report))
          (should (string-match-p "named other" report)))))))

(ert-deftest scalpel-agent-test-execute-action-delete ()
  "A delete action drops the block, its blank line, and the buffer's state.
The action settles synchronously -- no LLM request is involved -- and
the file on disk must already hold the result."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n\n(defun bar ()\n  nil)\n"))
    (let ((action (list :tool "block-delete" :file this-file :symbol "foo"))
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
    (scalpel-agent-block-edit
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
        (scalpel-agent-block-edit
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

(ert-deftest scalpel-agent-test-edit-names-unbalanced-brackets ()
  "A refused replacement whose brackets do not balance says so.
Regression: the planner's reply was one closing bracket short, and the
refusal said only that no usable replacement had been returned -- so
nothing told the planner what to change, and the same broken reply came
back on the next round.  The cause is decidable here, unlike the intent
behind a bad pattern, so the refusal states it."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 ;; The defun is never closed: no reader gets past it.  A
                 ;; suffix of it passes the bracket walk on its own, and
                 ;; is still refused, because `let' defines nothing.
                 (funcall on-success
                          "(defun foo (x)\n  (let ((y x))\n    (+ y 1))"))))
      (let (error)
        (scalpel-agent-block-edit
         this-file "foo" "bump x"
         (lambda (_report) (ert-fail "an unbalanced reply must not edit"))
         (lambda (err) (setq error err)))
        (ert-info ((format "Error: %S" error))
          (should (eq (plist-get error :type) 'no-replacement))
          (should (string-match-p "brackets do not balance"
                                  (plist-get error :message))))))))

(ert-deftest scalpel-agent-test-edit-names-a-reply-with-no-definition ()
  "A refused replacement that reads as no definition says so.
A bare \"no usable replacement\" merges causes that are not alike: a
reply whose brackets do not balance, one holding no definition, and one
holding several.  Each is decidable, so each is named, and the planner
is not left to guess which one it wrote."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo (x)\n  (+ x 1))\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 ;; Balanced, and defines nothing: `require' is not one
                 ;; of `scalpel-locate-elisp--defining-forms'.
                 (funcall on-success "(require 'json)"))))
      (let (error)
        (scalpel-agent-block-edit
         this-file "foo" "do nothing"
         (lambda (_report) (ert-fail "a non-definition must not edit"))
         (lambda (err) (setq error err)))
        (ert-info ((format "Error: %S" error))
          (should (eq (plist-get error :type) 'no-replacement))
          (should (string-match-p "No definition could be read"
                                  (plist-get error :message))))))))

(ert-deftest scalpel-agent-test-refusal-explains-a-reply-with-nothing-to-read ()
  "A reply holding no definition is explained by that, not by brackets.
Regression: the bracket walk was consulted before the definition count,
so a prose reply -- which holds no definition to have brackets about --
was answered with a sentence about structure, and the walk was asked
about text no step of it could move over, which hung the run.  The
listing below reads no definition in the reply, so the refusal must say
that and never reach the walk."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo (x)\n  (+ x 1))\n"))
    (let* ((reply "There are no occurrences of `(+ x 1)` in the body.")
           (reason (scalpel-agent--unusable-replacement-reason
                    this-file reply)))
      (ert-info ((format "Reason: %S" reason))
        (should (string-match-p "No definition could be read" reason))
        (should-not (string-match-p "brackets" reason))))))

(ert-deftest scalpel-agent-test-insert-names-unbalanced-brackets ()
  "A refused insertion states the same reason a refused edit does.
The two refusals are one judgement -- the reply is not a single usable
definition -- so the planner must not read a different account of it
depending on which action it reached for."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun anchor ())\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success
                          "(defun baz ()\n  (let ((y 1))\n    y)"))))
      (let (error)
        (scalpel-agent-block-insert
         this-file "baz" "add baz" "anchor"
         (lambda (_report) (ert-fail "an unbalanced reply must not land"))
         (lambda (err) (setq error err)))
        (ert-info ((format "Error: %S" error))
          (should (eq (plist-get error :type) 'no-replacement))
          (should (string-match-p "brackets do not balance"
                                  (plist-get error :message))))))))

(ert-deftest scalpel-agent-test-insert-accepts-a-top-level-registration-call ()
  "A reply that is one registration call, not a definition, is inserted.
Regression: the insert path admitted only a reply the locator reads a
definition in, so a file's registration idiom -- a call such as
`llm-pick-source-register' -- was refused whole, and every round that
reached for it died the same way.  The call defines no name, so it
cannot be located afterwards; the report says so rather than leaving
the planner to discover it."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun anchor ())\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success
                          (concat "(llm-pick-source-register "
                                  "'artificial-analysis\n"
                                  "  :kind 'capability\n"
                                  "  :fetcher #'loader)")))))
      (let (report)
        (scalpel-agent-block-insert
         this-file "register-artificial-analysis" "register it" "anchor"
         (lambda (r) (setq report r))
         (lambda (err) (ert-fail (plist-get err :message))))
        (ert-info ((format "Report: %S On disk: %S" report
                           (with-temp-buffer
                             (insert-file-contents this-file)
                             (buffer-string))))
          (should (string-match-p "Created register-artificial-analysis"
                                  report))
          (should (string-match-p "cannot be located" report)))
        (let ((on-disk (with-temp-buffer
                         (insert-file-contents this-file)
                         (buffer-string))))
          (ert-info ((format "On disk:\n%S" on-disk))
            (should (string-match-p "llm-pick-source-register" on-disk))
            (should (string-match-p "artificial-analysis" on-disk))))))))

(ert-deftest scalpel-agent-test-insert-refusal-speaks-of-forms-not-definitions ()
  "A refused insertion says the reply is not one top-level form.
Regression: the refusal judged the reply as a definition the locator
had to read, so a file's own registration idiom was answered with
\"no definition could be read out of it\" -- a sentence about a
definition the reply was never meant to hold, which sent the next
round after the wrong artifact.  The reply below is balanced and
defines nothing, so it is refused for its form count, not as code the
reader cannot finish."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun anchor ())\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success "(require 'json)\n(provide 'x)"))))
      (let (error)
        (scalpel-agent-block-insert
         this-file "register-x" "register x" "anchor"
         (lambda (_r) (ert-fail "a two-form reply must not land"))
         (lambda (e) (setq error e)))
        (ert-info ((format "Error: %S" error))
          (should (eq (plist-get error :type) 'no-replacement))
          (should (string-match-p "exactly one complete top-level form"
                                  (plist-get error :message)))
          (should-not (string-match-p "No definition could be read"
                                      (plist-get error :message))))))))

(ert-deftest scalpel-agent-test-edit-accepts-escaped-quotes-in-a-docstring ()
  "A docstring's escaped quotes are source syntax, not a defect.
Regression risk: the refusal that started this work arrived with
backslash-escaped quotes in its docstring, and the tempting fix --
stripping them -- would close the string early and leave the reply
unreadable.  The reader needs the backslash to keep the quote inside the
string, so such a reply must be applied as written."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ()\n  \"Say hi.\")\n"))
    (cl-letf (((symbol-function 'scalpel-llm-request-async)
               (lambda (_prompt on-success _on-error &optional _system)
                 (funcall on-success "(defun foo ()\n  \"Say \\\"hi\\\".\")"))))
      (let (report)
        (scalpel-agent-block-edit
         this-file "foo" "quote the word"
         (lambda (r) (setq report r))
         (lambda (err) (ert-fail (plist-get err :message))))
        (ert-info ((format "Report: %S" report))
          (should (string-match-p "Edited foo" report)))
        (with-current-buffer (find-file-noselect this-file)
          (should (string= (buffer-string)
                           "(defun foo ()\n  \"Say \\\"hi\\\".\")\n")))))))

(ert-deftest scalpel-agent-test-read-whole-file ()
  "A read without a symbol returns the file inside the output fence."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file)))))
      (let ((report (scalpel-agent-file-read this-file nil)))
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
      (let ((report (scalpel-agent-file-read this-file "foo")))
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
      (should-error (scalpel-agent-file-read this-file nil) :type 'user-error)
      (should-error (scalpel-agent-file-read this-file "foo")
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
          (scalpel-agent-file-read-max-bytes 10))
      (let ((err (condition-case e
                     (progn (scalpel-agent-file-read this-file "foo") nil)
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
           (scalpel-agent-file-read-max-bytes 20)
           (size (with-temp-buffer
                   (insert-file-contents this-file)
                   (string-bytes (buffer-string))))
           (report (scalpel-agent-file-read this-file nil)))
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
               (list :tool "file-peek" :file "/tmp/a.el" :symbol "foo"))))

(ert-deftest scalpel-agent-test-execute-action-read ()
  "A read action settles synchronously through ON-SUCCESS."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file))))
          report)
     (scalpel-agent-execute-action
      (list :tool "file-peek" :file this-file :symbol nil)
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
                 (concat "[{\"tool\":\"file-peek\",\"file\":\"/tmp/a.el\"},"
                         "{\"tool\":\"file-peek\",\"file\":\"/tmp/a.el\","
                         "\"symbol\":\"foo\"}]"))))
    (should (= (length parsed) 2))
    (should-not (plist-get (car parsed) :symbol))
    (should (equal (plist-get (cadr parsed) :symbol) "foo")))
  (let ((projected (scalpel-agent--project-actions
                    (list (list :tool "file-peek" :file "/tmp/a.el")
                          (list :tool "file-peek" :file "/tmp/a.el" :symbol "foo")))))
    (should-not (plist-get (car projected) :symbol))
    (should (equal (plist-get (cadr projected) :symbol) "foo"))))

(ert-deftest scalpel-agent-test-read-requires-file ()
  "A read action without :file is rejected at validation."
  (should-error
   (scalpel-agent--validate-action '(:tool "file-peek" :symbol "foo"))
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
                           (concat "[{\"tool\":\"file-peek\",\"file\":\"/tmp/a.el\","
                                   "\"symbol\":\"foo\"}]"))))
          (let (result)
            (cl-letf (((symbol-function 'scalpel-agent-file-read)
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
                                      (plist-get (car actions) :text)))
          ;; The answer is the prose itself, not the error narrative:
          ;; shipping "nothing was executed" as the reply is what made
          ;; the planner read it back and repeat it as content.
          (should-not (string-match-p "nothing was executed"
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
        (scalpel-agent-block-edit this-file "Alpha" "tighten the wording"
                            (lambda (_report) nil)
                            (lambda (_err) nil)))
      (ert-info ((format "Prompt:\n%S" prompt))
        (should prompt)
        (should-not (string-match-p "Emacs Lisp" prompt))))))

(ert-deftest scalpel-agent-test-create-prompt-requests-no-change-sentinel ()
  "The create prompt asks for the sentinel the code compares against.
Regression: `scalpel-agent-block-insert' tested the reply against
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
        (scalpel-agent-block-insert this-file "bar" "add bar" "foo"
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
                    '(:tool "block-edit" :file "/tmp/a.el" :symbol "foo"))
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

(ert-deftest scalpel-agent-test-shell-decline-is-successful-outcome ()
  "A user decline of a confirmed action is a successful outcome.
ON-SUCCESS is called with a report naming the decline, so the round continues
instead of ending.  The shell function itself must never run."
  (let* ((asks 0)
         (ran-shell nil)
         (success-report nil)
         (error-called nil))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) (setq asks (1+ asks)) nil))
              ((symbol-function 'scalpel-agent-shell)
               (lambda (_command _callback) (setq ran-shell t) "report")))
      (scalpel-agent-execute-action
       '(:tool "shell" :command "make test" :reason "run" :long-running t)
       (lambda (report) (setq success-report report))
       (lambda (err) (setq error-called t)
         (ert-fail (plist-get err :message))))
      (should (= 1 asks))
      (should-not ran-shell)
      (should-not error-called)
      (should (stringp success-report))
      (should (string-match-p "declined by the user" success-report)))))

(ert-deftest scalpel-agent-test-system-prompt-states-the-pattern-dialect ()
  "The prompt states the regular-expression dialect a pattern is read in.
Regression: the prompt asked for \"a regular expression\" and named
no dialect, so a planner wrote \"\\(emacs ...\\)\" meaning a literal
bracket -- a group in Emacs syntax -- and the brackets it aimed at
were absent from the pattern, which then matched nothing in a file
that held them.  Nothing in the code can read that intent back out
of a pattern without guessing at it, so the rule has to be stated to
the model; this guards only that the live prompt still carries it,
the way the brevity rule and the perl preference are guarded."
  (ert-info ((format "Rule:\n%S" scalpel-agent--substitute-pattern-rule))
    (should (string-match-p
             (regexp-quote scalpel-agent--substitute-pattern-rule)
             scalpel-agent-system-prompt))
    (should (string-match-p "Emacs regular expression"
                            scalpel-agent--substitute-pattern-rule))
    ;; The rule is carried with a worked example: the prose statement
    ;; alone did not stop the escaped spelling, which came back three
    ;; rounds running, each time refused for matching nothing.  The
    ;; example is the part a model can copy.
    (should (string-match-p
             (regexp-quote "((emacs \"28\\.1\") (transient \"0\\.3\\.0\"))")
             scalpel-agent--substitute-pattern-rule))
    ;; The rule covers the other borrowed convention too: a replacement's
    ;; whole-match placeholder is not a pattern construct, and a run of
    ;; text that repeats inside one pattern is named by a group and a
    ;; backreference.
    (should (string-match-p "replacement convention"
                            scalpel-agent--substitute-pattern-rule))
    (should (string-match-p (regexp-quote "\\1")
                            scalpel-agent--substitute-pattern-rule))))

(ert-deftest scalpel-agent-test-system-prompt-takes-names-literally ()
  "The prompt says a symbol name is copied, never re-spelled.
Regression: the planner asked for `llm-pick-view--cache-dir' while the
file held `llm-pick-view-cache-dir', and for `llm-pick-view--main'
while it held `llm-pick-view-main'; each round died before anything was
edited, although the SYMBOLS list in the prompt stated the right
spelling.  Nothing in the code can prevent the spelling, so the rule
has to be stated to the model; this guards only that the live prompt
still carries it, the way the dialect and brevity rules are guarded."
  (ert-info ((format "Rule:\n%S" scalpel-agent--symbol-name-rule))
    (should (string-match-p
             (regexp-quote scalpel-agent--symbol-name-rule)
             scalpel-agent-system-prompt))
    (should (string-match-p "taken literally"
                            scalpel-agent--symbol-name-rule))))

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
            (let ((report (scalpel-agent-file-rename this-file to)))
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
    (should-error (scalpel-agent-file-rename "/nonexistent/x.el" "/tmp/y.el")
                  :type 'user-error)
    (should-error (scalpel-agent-file-rename this-file this-file)
                  :type 'user-error)
    (should-error (scalpel-agent-file-rename nil nil) :type 'user-error)))

(ert-deftest scalpel-agent-test-delete-file-removes-from-disk ()
  "A `delete-file' removes the file and kills its unmodified buffer."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())"))
    (find-file-noselect this-file)
    (let ((report (scalpel-agent-file-delete this-file)))
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
      (should-error (scalpel-agent-file-delete this-file)
                    :type 'user-error)
      (should (file-exists-p this-file))
      (with-current-buffer buf (set-buffer-modified-p nil)))))

(ert-deftest scalpel-agent-test-delete-file-refuses-missing ()
  "A `delete-file' of an absent file signals; so does a malformed action."
  (should-error (scalpel-agent-file-delete "/nonexistent/x.el")
                :type 'user-error)
  (should-error (scalpel-agent-file-delete nil) :type 'user-error))

(ert-deftest scalpel-agent-test-delete-file-drops-the-context-entry ()
  "A deleted file leaves the session context with the disk.
Regression: the entry stayed, so every later prompt listed a file that
no longer exists and the sandbox policy carried a read-only bind for a
path nothing can open."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let ((entry (file-truename (expand-file-name this-file))))
      (with-temp-buffer
        (let* ((scalpel-agent--context-files (list entry))
               (report (scalpel-agent-file-delete this-file)))
          (ert-info ((format "Report: %S Context: %S"
                             report scalpel-agent--context-files))
            (should (string-match-p "Deleted file" report))
            (should-not (file-exists-p this-file))
            (should (null scalpel-agent--context-files))))))))

(ert-deftest scalpel-agent-test-rename-moves-the-context-entry ()
  "A renamed file keeps its place in the context under its new path.
Regression: the context kept the old path, which no longer exists, and
did not hold the new one, which does: the next round read a file that is
gone and could not name the file that replaced it."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let* ((to (concat this-file "-renamed.el"))
           (entry (file-truename (expand-file-name this-file))))
      (unwind-protect
          (with-temp-buffer
            (let* ((scalpel-agent--context-files (list entry))
                   (report (scalpel-agent-file-rename this-file to)))
              (ert-info ((format "Report: %S Context: %S"
                                 report scalpel-agent--context-files))
                (should (string-match-p "Renamed" report))
                (should-not (member entry scalpel-agent--context-files))
                (should (member (file-truename (expand-file-name to))
                                scalpel-agent--context-files)))))
        (scalpel-utils-test-kill-file-buffer to)
        (scalpel-utils-test-delete-file to)))))

(ert-deftest scalpel-agent-test-rename-adds-no-file-to-the-context ()
  "A rename of a file the session never held leaves the context alone.
The context is the user's list of readable files; a rename moves what is
already in it and is not a way into it."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let ((to (concat this-file "-moved.el")))
      (unwind-protect
          (with-temp-buffer
            (let ((scalpel-agent--context-files nil))
              (scalpel-agent-file-rename this-file to)
              (should (null scalpel-agent--context-files))))
        (scalpel-utils-test-kill-file-buffer to)
        (scalpel-utils-test-delete-file to)))))

(ert-deftest scalpel-agent-test-execute-action-rename-and-delete-file ()
  "Rename and `delete-file' actions settle synchronously through reports."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "x"))
    (let ((scalpel-agent-confirm-tools nil)
          report)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (scalpel-agent-execute-action
       (list :tool "file-rename" :file this-file
             :to (concat this-file "-moved.el"))
       (lambda (r) (setq report r))
       (lambda (err) (ert-fail (plist-get err :message))))
      (should (string-match-p "Renamed" report))
      (scalpel-agent-execute-action
       (list :tool "file-delete" :file (concat this-file "-moved.el"))
       (lambda (r) (setq report r))
       (lambda (err) (ert-fail (plist-get err :message)))))
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
                    '(:tool "file-delete" :file "/tmp/a.el"))
                   "/tmp/a.el"))
  (should (string= (scalpel-agent--action-summary
                    '(:tool "shell" :reason "look around"))
                   "look around"))
  (should (string= (scalpel-agent--action-summary '(:tool "shell"))
                   "no reason")))

(ert-deftest scalpel-agent-test-delete-corrects-a-misnamed-file ()
  "A symbol named with a hallucinated path is corrected, not refused.
Regression: the planner repeatedly asked to edit symbols under a
sibling file's path and the round died with \"symbol not found\";
location is deterministic, so the context search fixes the path
with zero LLM calls.  The correction is stated in the report, so
the substitution is never silent."
  ;; The asked file holds no `target'; the sibling one does, which is
  ;; the shape the planner's hallucinated path really takes.
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun other ())\n"))
    (let* ((sibling (concat this-file "-sibling.el"))
           (scalpel-agent--context-files
            (list (file-truename (expand-file-name this-file))
                  (file-truename (expand-file-name sibling)))))
      (with-temp-file sibling (insert "(defun target ())\n"))
      (unwind-protect
          (let (report)
            (scalpel-agent-execute-action
             (list :tool "block-delete"
                   :file (file-truename (expand-file-name this-file))
                   :symbol "target")
             (lambda (r) (setq report r))
             (lambda (err) (ert-fail (plist-get err :message))))
            (ert-info ((format "Report: %S" report))
              (should (string-match-p "Deleted target" report))
              (should (string-match-p "the definition lives in" report))
              (should (string-match-p
                       (regexp-quote
                        (file-truename (expand-file-name sibling)))
                       report)))
            (let ((on-disk (with-temp-buffer
                             (insert-file-contents sibling)
                             (buffer-string))))
              (ert-info ((format "On disk:\n%S" on-disk))
                (should (string= on-disk "")))))
        (scalpel-utils-test-kill-file-buffer sibling)
        (scalpel-utils-test-delete-file sibling)))))

(ert-deftest scalpel-agent-test-ambiguous-symbol-names-every-file ()
  "A symbol defined in several context files is refused with the list.
Auto-correcting on ambiguity would pick a file the planner never
named, so the refusal hands back the facts instead."
  ;; The asked file holds no `dup' -- a direct hit there is a plain
  ;; success, not an ambiguity -- while two real context files define
  ;; it.  A sibling whose file is missing would be skipped by the scan
  ;; and read as a unique hit instead of an ambiguity.
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun other ())\n"))
    (let ((sibling (concat this-file "-sibling.el"))
          (second (concat this-file "-second.el")))
      (with-temp-file sibling (insert "(defun dup ())\n"))
      (with-temp-file second (insert "(defun dup ())\n"))
      (unwind-protect
          (let ((scalpel-agent--context-files
                 (list (file-truename (expand-file-name this-file))
                       (file-truename (expand-file-name sibling))
                       (file-truename (expand-file-name second)))))
            (let ((err (condition-case e
                           (progn (scalpel-agent--resolve-symbol
                                   (car scalpel-agent--context-files) "dup")
                                  nil)
                         (user-error e))))
              (ert-info ((format "Error: %S" err))
                (should err)
                (should (string-match-p "several context files"
                                        (error-message-string err)))
                (should (string-match-p
                         (regexp-quote (cadr scalpel-agent--context-files))
                         (error-message-string err)))
                (should (string-match-p
                         (regexp-quote (cl-caddr scalpel-agent--context-files))
                         (error-message-string err))))))
        (dolist (file (list sibling second))
          (scalpel-utils-test-kill-file-buffer file)
          (scalpel-utils-test-delete-file file))))))

(ert-deftest scalpel-agent-test-absent-symbol-names-the-context ()
  "A symbol defined nowhere lists the context files in the error.
The message is the planner's only feedback for picking the right
file next round, so \"not found\" alone is a dead end."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun other ())\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file)))))
      (let ((err (condition-case e
                     (progn (scalpel-agent--resolve-symbol
                             (car scalpel-agent--context-files) "gone")
                            nil)
                   (user-error e))))
        (ert-info ((format "Error: %S" err))
          (should err)
          (should (string-match-p "nor anywhere in the context"
                                  (error-message-string err)))
          (should (string-match-p
                   (regexp-quote (car scalpel-agent--context-files))
                   (error-message-string err))))))))

(ert-deftest scalpel-agent-test-absent-symbol-reports-what-the-file-defines ()
  "A symbol missing from a context file reports what that file defines.
Regression: the refusal told the planner to add the file to the
context, which was already true -- the user hit it twice with a file
the context plainly held -- so the cause it could not name, a
definition the locator does not read, stayed invisible and the round
was spent on a remedy that changed nothing.  The definitions below
are the observed shape: the file holds a name that begins like the
one asked for, and the requested spelling is not among them."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun llm-pick-view--main-groups (records)\n  nil)\n"
              "(defun llm-pick-view-main ())\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file)))))
      (let ((err (condition-case e
                     (progn (scalpel-agent--resolve-symbol
                             (car scalpel-agent--context-files)
                             "llm-pick-view--main")
                            nil)
                   (user-error e))))
        (ert-info ((format "Error: %S" err))
          (should err)
          (let ((message (error-message-string err)))
            (ert-info ((format "Message:\n%S" message))
              (should (string-match-p "defines 2 definition" message))
              (should (string-match-p
                       "llm-pick-view--main is not one of them" message))
              ;; The name that is nearly the one asked for gets its own
              ;; line, so the gap is visible without reading the list.
              ;; The line reads "like <asked for>: <near name>", so the
              ;; assertion quotes exactly that: a literal joining the
              ;; two names without the symbol between them matches no
              ;; line here, and a shorter one matching only the near
              ;; name would also match the "It defines" line below it,
              ;; passing even if this line were gone.
              (should (string-match-p
                       (regexp-quote
                        (concat "Definitions spelled like "
                                "llm-pick-view--main: "
                                "llm-pick-view--main-groups"))
                       message)))))))))

(ert-deftest scalpel-agent-test-symbol-skeleton-blinds-only-separators ()
  "Separators vanish from the comparison; letters stay.
Regression: the names this failure turns on differ only in hyphens --
`llm-pick-view--cache-dir' was asked for while the file held
`llm-pick-view-cache-dir' -- so the comparison has to see through
them; one that did not could only offer the shared prefix, which is
the part every name of that file shares."
  (should (equal (scalpel-agent--symbol-skeleton "llm-pick-view--cache-dir")
                 (scalpel-agent--symbol-skeleton "llm-pick-view-cache-dir")))
  (should-not (equal (scalpel-agent--symbol-skeleton "llm-pick-view--cache-dir")
                     (scalpel-agent--symbol-skeleton "llm-pick-view--cache-file"))))

(ert-deftest scalpel-agent-test-absent-symbol-names-a-separator-mismatch ()
  "A name one hyphen away is reported as the file spells it.
Regression: the near-name line compared prefixes, so the name the
planner was aiming at was invisible exactly when it was a separator
away -- `llm-pick-view--cache-dir' was asked for while the file held
`llm-pick-view-cache-dir', and the refusal offered only
`llm-pick-view', the prefix every name of that file shares."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defcustom llm-pick-view-cache-dir nil\n  \"Where.\")\n"
              "(defun llm-pick-view--cache-file ()\n  nil)\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file)))))
      (let ((err (condition-case e
                     (progn (scalpel-agent--resolve-symbol
                             (car scalpel-agent--context-files)
                             "llm-pick-view--cache-dir")
                            nil)
                   (user-error e))))
        (ert-info ((format "Error: %S" err))
          (should err)
          (let ((message (error-message-string err)))
            (ert-info ((format "Message:\n%S" message))
              (should (string-match-p
                       (regexp-quote
                        (concat "The file spells llm-pick-view-cache-dir "
                                "where llm-pick-view--cache-dir was asked "
                                "for"))
                       message)))))))))

(ert-deftest scalpel-agent-test-absent-symbol-keeps-letters-apart ()
  "A name differing in a letter gets no separator sentence.
The comparison drops `-' and `_', so a name that differs by a letter
must not be offered as the same name spelled differently: that
sentence would send the planner after a definition it never named."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun llm-pick-view--cache-file ())\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file)))))
      (let ((err (condition-case e
                     (progn (scalpel-agent--resolve-symbol
                             (car scalpel-agent--context-files)
                             "llm-pick-view--cache-dir")
                            nil)
                   (user-error e))))
        (should err)
        (should-not (string-match-p "only the separators differ"
                                    (error-message-string err)))))))

(ert-deftest scalpel-agent-test-absent-symbol-when-the-locator-reads-nothing ()
  "A file holding no locatable definition says so, not \"add its file\".
A definition written with a form the locator does not know, and a
file whose text is not on disk, both read as an empty definition
list; that is a different cause from a file nobody added to the
context, and the two must not share a message."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(message \"no definitions here\")\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file)))))
      (let ((err (condition-case e
                     (progn (scalpel-agent--resolve-symbol
                             (car scalpel-agent--context-files) "gone")
                            nil)
                   (user-error e))))
        (ert-info ((format "Error: %S" err))
          (should err)
          (should (string-match-p "reads no definition in it"
                                  (error-message-string err))))))))

(ert-deftest scalpel-agent-test-absent-symbol-outside-the-context-names-the-remedy ()
  "A file outside the context is reported as something to ask the user for.
The planner cannot add a file itself -- the prompt says so -- so a
refusal that told it to do so asked for something it cannot carry
out, and cost a round."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun elsewhere ())\n"))
    (let ((scalpel-agent--context-files nil))
      (let ((err (condition-case e
                     (progn (scalpel-agent--resolve-symbol this-file "gone")
                            nil)
                   (user-error e))))
        (ert-info ((format "Error: %S" err))
          (should err)
          (should (string-match-p "ask the user to add it"
                                  (error-message-string err))))))))

(ert-deftest scalpel-agent-test-edit-missing-symbol-reports-through-on-error ()
  "An edit naming a symbol the file does not hold settles via ON-ERROR.
Regression: the initial locate ran outside the error guard, so the
`user-error' escaped the callback contract and the console reported
a raw error instead of a planner failure the history could read."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo (x)\n  (+ x 1))\n"))
    (let (error)
      (scalpel-agent-block-edit
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
        (scalpel-agent-block-edit
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
        (scalpel-agent-block-edit
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

(ert-deftest scalpel-agent-test-create-file-writes-whole-content ()
  "A file-create lands the whole content and missing parents.
Regression: no tool could create a file, so a planner holding a
finished new-file draft could only degrade to handing the user a
shell command through confirm."
  (let ((dir (make-temp-file "scalpel-test-new-" t))
        report)
    (unwind-protect
        (let ((target (expand-file-name "sub/new.el" dir)))
          (scalpel-agent-execute-action
           (list :tool "file-create" :file target
                 :text "(defun a ())\n(defun b ())\n")
           (lambda (r) (setq report r))
           (lambda (err) (ert-fail (plist-get err :message))))
          (ert-info ((format "Report: %S" report))
            (should (string-match-p "Created file" report)))
          (let ((on-disk (with-temp-buffer
                           (insert-file-contents target)
                           (buffer-string))))
            (ert-info ((format "On disk:\n%S" on-disk))
              (should (string= on-disk "(defun a ())\n(defun b ())\n"))))))
      (delete-directory dir t)))

(ert-deftest scalpel-agent-test-create-file-refuses-existing ()
  "A file-create onto an existing file is refused, never overwritten.
Changing an existing file is `block-edit' and `block-insert' work;
the refusal must leave the file exactly as it was."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "keep\n"))
    (progn
      (should-error
       (scalpel-agent-file-create this-file "(defun a ())")
       :type 'user-error)
      (let ((on-disk (with-temp-buffer
                       (insert-file-contents this-file)
                       (buffer-string))))
        (should (string= on-disk "keep\n"))))))

(ert-deftest scalpel-agent-test-create-file-runs-without-confirmation ()
  "A file-create runs without asking, whatever the confirm list says.
Regression: it was a file-level tool and always prompted, so
every new-file creation cost a keystroke although the report
names the created path in full."
  (let ((scalpel-agent-confirm-tools '("file-create"))
        (asked 0)
        (dir (make-temp-file "scalpel-test-new-" t)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (setq asked (1+ asked)) t)))
            (scalpel-agent-execute-action
             (list :tool "file-create"
                   :file (expand-file-name "new.el" dir)
                   :text "x")
             (lambda (_r) nil)
             (lambda (err) (ert-fail (plist-get err :message)))))
          (should (file-exists-p (expand-file-name "new.el" dir)))
          (ert-info ((format "asked=%d" asked))
            (should (= asked 0))))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-create-file-refuses-malformed ()
  "A file-create without a file or content signals `user-error'."
  (should-error (scalpel-agent-file-create nil "x") :type 'user-error)
  (should-error (scalpel-agent-file-create "/tmp/a.el" nil)
                :type 'user-error))

(ert-deftest scalpel-agent-test-path-components-relative-and-absolute ()
  "Absolute paths keep a root component; relative ones do not."
  (should (equal (scalpel-agent--path-components "/a/b.el")
                 '("/" "a" "b.el")))
  (should (equal (scalpel-agent--path-components "~/x/a.el")
                 '("~" "x" "a.el")))
  (should (equal (scalpel-agent--path-components "a/b.el")
                 '("a" "b.el"))))

(defun scalpel-agent-test--stage-two-files ()
  "Create two context files under one temp directory; return (DIR F1 F2)."
  (let ((dir (make-temp-file "scalpel-test-rewrite-" t)))
    (let ((f1 (expand-file-name "a.el" dir))
          (f2 (expand-file-name "b.el" dir)))
      (with-temp-file f1 (insert "(defun old-a ())\n(defun keep ())\n"))
      (with-temp-file f2 (insert "(defun old-b ())\n"))
      (list dir f1 f2))))

(ert-deftest scalpel-agent-test-rewrite-applies-across-context-files ()
  "A rewrite changes every matching occurrence in every named file.
The effect is enumerable: each file and its occurrence count is in
the report, and the one confirmation is not waivable."
  (cl-destructuring-bind (dir f1 f2) (scalpel-agent-test--stage-two-files)
    (unwind-protect
        (let* ((files (mapcar #'file-truename (list f1 f2)))
               (scalpel-agent--context-files (copy-sequence files))
               (asked 0)
               report)
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (setq asked (1+ asked)) t)))
            (scalpel-agent-execute-action
             (list :tool "file-substitute" :files files
                   :pattern "old-" :replacement "new-"
                   :reason "bulk rename")
             (lambda (r) (setq report r))
             (lambda (e) (ert-fail (plist-get e :message)))))
          (ert-info ((format "Report: %S asked=%d" report asked))
            (should (string-match-p "Rewrote 1 occurrence(s) in .*a\\.el"
                                    report))
            (should (string-match-p "Rewrote 1 occurrence(s) in .*b\\.el"
                                    report)))
          (ert-info ((format "asked=%d" asked))
            (should (= asked 1)))
          (with-temp-buffer (insert-file-contents f1)
            (should (string-match-p "(defun new-a ())" (buffer-string)))
            (should (string-match-p "(defun keep ())" (buffer-string)))))
      (dolist (f (list f1 f2))
        (scalpel-utils-test-kill-file-buffer f))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-rewrite-refuses-file-outside-context ()
  "A rewrite never touches a file outside the session context."
  (let ((scalpel-agent--context-files nil))
    (should-error (scalpel-agent-file-substitute
                   (list "/tmp/scalpel-not-in-context.el") "x" "y")
                  :type 'user-error)))

(ert-deftest scalpel-agent-test-substitute-refusal-names-the-action-and-the-remedy ()
  "A refused file-substitute names the action, the disk state, and the fix.
Regression: the refusal called the action \"a rewrite\" -- a name no
entry of `scalpel-agent--tool-vocabulary' answers to -- and stopped
there, so neither the user nor the planner could tell what to do
next."
  (let ((absent (make-temp-name
                 (expand-file-name "scalpel-absent-"
                                   temporary-file-directory))))
    (scalpel-utils-test-with-temp-file ".el"
      (with-temp-file this-file (insert "(defun foo ())\n"))
      (let ((scalpel-agent--context-files nil))
        (dolist (probe (list (cons this-file "exists on disk")
                             (cons absent "No such file")))
          (let* ((file (car probe))
                 (err (condition-case e
                          (progn (scalpel-agent-file-substitute
                                  (list file) "foo" "bar")
                                 nil)
                        (user-error e))))
            (ert-info ((format "File %S; error: %S" file err))
              (should err)
              (let ((message (error-message-string err)))
                (should (string-match-p "file-substitute" message))
                (should-not (string-match-p "rewrite" message))
                ;; The remedy is one the user can carry out...
                (should (string-match-p "C-c C-a" message))
                ;; ...and the disk state is stated, so a path that simply
                ;; does not exist is not read as a missing context entry.
                (should (string-match-p (regexp-quote (cdr probe))
                                        message))))))))))

(ert-deftest scalpel-agent-test-rewrite-refuses-zero-matches ()
  "A rewrite matching nothing is refused, not reported as success.
The refusal names the pattern and the files it scanned: the message
is the planner's only feedback for correcting its own next rewrite,
and a refusal without the pattern is a dead end for the user too."
  (cl-destructuring-bind (dir f1 _f2) (scalpel-agent-test--stage-two-files)
    (unwind-protect
        (let ((scalpel-agent--context-files (list (file-truename f1))))
          (let ((err (condition-case e
                         (progn (scalpel-agent-file-substitute
                                 (list (file-truename f1))
                                 "no-such-token" "x")
                                nil)
                       (user-error e))))
            (ert-info ((format "Error: %S" err))
              (should err)
              (let ((message (error-message-string err)))
                (should (string-match-p "no-such-token" message))
                (should (string-match-p "replacement \"x\"" message))
                (should (string-match-p
                         (regexp-quote (file-truename f1)) message))))))
      (dolist (f (directory-files dir t "^[^.]"))
        (scalpel-utils-test-kill-file-buffer f))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-rewrite-refuses-unbalanced-result-whole ()
  "One file whose rewrite breaks balance refuses every file's change.
Regression risk: applying file by file would leave the earlier files
rewritten when a later one failed -- a half-applied batch whose
extent nothing downstream could know."
  (cl-destructuring-bind (dir f1 f2) (scalpel-agent-test--stage-two-files)
    (unwind-protect
        (let* ((files (mapcar #'file-truename (list f1 f2)))
               (scalpel-agent--context-files (copy-sequence files)))
          ;; A harmless change in a.el is paired with an unbalancing
          ;; one in b.el; the refusal must cover a.el too.
          (let ((err (condition-case e
                         (progn (scalpel-agent-file-substitute
                                 files "(defun keep" "(defun keep (")
                                nil)
                       (user-error e))))
            (ert-info ((format "Error: %S" err))
              (should err))
            ;; The refused action names the pair it was built from, so
            ;; the next attempt can correct it whole.
            (should (string-match-p
                     (regexp-quote
                      (scalpel-agent--substitute-invocation
                       "(defun keep" "(defun keep ("))
                     (error-message-string err))))
          (with-temp-buffer (insert-file-contents f1)
            (should (string-match-p "old-a" (buffer-string)))))
      (dolist (f (list f1 f2))
        (scalpel-utils-test-kill-file-buffer f))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-rewrite-reports-definitions-it-changed ()
  "A rewrite's report names the definitions it dropped and added.
Regression: the report counted occurrences only, so a bulk rename
said nothing about the name it removed; the next round located the
old name and failed with \"not found\", with nothing in the record
explaining where it had gone."
  (cl-destructuring-bind (dir f1 _f2) (scalpel-agent-test--stage-two-files)
    (unwind-protect
        (let ((scalpel-agent--context-files (list (file-truename f1))))
          (let ((report (scalpel-agent-file-substitute
                         (list (file-truename f1)) "old-a" "new-a")))
            (ert-info ((format "Report:\n%S" report))
              (should (string-match-p "Rewrote 1 occurrence" report))
              (should (string-match-p "no longer defined: old-a" report))
              (should (string-match-p "now defined: new-a" report)))))
      (dolist (f (directory-files dir t "^[^.]"))
        (scalpel-utils-test-kill-file-buffer f))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-rewrite-without-definition-change-has-no-note ()
  "A rewrite that leaves the definitions alone reports no definition change.
The note is a signal, not boilerplate: one printed on every rewrite
would be read as noise and stop carrying the renames it exists for.

The pattern is a bare word on purpose.  A \"pattern\" is an Emacs
regular expression, and one holding brackets does not match its own
spelling: \"(+ 1 1)\" reads as one-or-more literal open parens
followed by \" 1 1)\", which the text \"(+ 1 1)\" never contains.
The refusal that follows is the tool working -- zero matches is
refused -- so a test written that way proves nothing about the note."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ()\n  nil)\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (report (scalpel-agent-file-substitute
                    (list resolved) "nil" "t")))
      (ert-info ((format "Report:\n%S" report))
        (should (string-match-p "Rewrote 1 occurrence" report))
        (should-not (string-match-p "definitions changed" report))))))

(ert-deftest scalpel-agent-test-rewrite-counts-what-it-replaced ()
  "The occurrence count reads the pattern the replacement used.
Regression: the count ran with the buffer's case folding while the
replacement ran case-sensitively, so a rewrite of \"old\" changed one
occurrence in the file and reported two -- a number about matches the
rewrite never made, which is worse than no number at all, because the
planner trusts it to decide whether the rewrite is complete."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun foo ()\n  (list \"old\" \"OLD\"))\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (report (scalpel-agent-file-substitute
                    (list resolved) "old" "new")))
      (ert-info ((format "Report: %S On disk: %S"
                         report
                         (with-temp-buffer
                           (insert-file-contents resolved)
                           (buffer-string))))
        (should (string-match-p "Rewrote 1 occurrence" report))
        (with-temp-buffer
          (insert-file-contents resolved)
          (should (string-match-p "\"new\" \"OLD\"" (buffer-string))))))))

(ert-deftest scalpel-agent-test-rewrite-zero-match-shows-the-closest-lines ()
  "A zero-match refusal quotes the lines the pattern was written for.
Regression: the refusal named the pattern and the files and nothing
else, so a planner whose pattern missed a header by one entry had no
way to correct it except by spending another round reading the file
it had already misremembered; the observed next move was to send the
same pattern again."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert ";; Package-Requires: ((emacs \"28.1\") (transient \"0.3.0\") "
              "(s \"1.13.0\"))\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved)
                       (concat ";; Package-Requires: ((emacs \"28\\.1\") "
                               "(transient \"0\\.3\\.0\"))")
                       ";; Package-Requires: ((emacs \"28.1\"))")
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (let ((message (error-message-string err)))
          (ert-info ((format "Message:\n%S" message))
            (should (string-match-p "matched nothing" message))
            ;; The file's own line, quoted: the entry the pattern was
            ;; written without is what the planner has to see.
            (should (string-match-p (regexp-quote "(s \"1.13.0\"))")
                                    message))
            ;; Nothing was written: the refusal still refuses.
            (should (string-match-p "transient \"0.3.0\") (s \"1.13.0\"))"
                                    (with-temp-buffer
                                      (insert-file-contents resolved)
                                      (buffer-string))))))))))

(ert-deftest scalpel-agent-test-rewrite-zero-match-without-near-miss-is-bare ()
  "A pattern sharing nothing with the file adds no lines to the refusal.
The note is evidence, not boilerplate: an unrelated stub must not put
arbitrary lines under \"closest\", or the planner would read
coincidences as the text it was aiming at."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved) "zzz-absent-identifier" "x")
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (should-not (string-match-p "closest lines"
                                    (error-message-string err)))))))

(ert-deftest scalpel-agent-test-rewrite-hint-prefers-the-longest-run ()
  "The hint quotes the line sharing the longest matching run.
A shorter prefix is reached only when every longer one found
nothing, so a line matching the pattern's opening characters by
coincidence cannot displace the near miss."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "alpha: one\nalpha: one two three four\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (text (with-temp-buffer
                   (insert-file-contents resolved)
                   (buffer-string)))
           (hint (scalpel-agent--substitute-hint-lines
                  text "alpha: one two three four five")))
      (ert-info ((format "Hint: %S" hint))
        ;; The line holding only the shorter run is not offered: the
        ;; longer prefix already placed the text.
        (should (equal (car hint) '("alpha: one two three four")))
        ;; The pattern is searched as written, escapes and all, so no
        ;; bracket sentence is due.
        (should-not (cdr hint))))))

(ert-deftest scalpel-agent-test-rewrite-anchored-pattern-still-shows-the-closest-lines ()
  "A refused anchored pattern still quotes the file's own line.
Regression: `scalpel-agent--substitute-prefix-lines' walked its
prefixes off the right of the pattern, so a leading anchor --
zero-width, and held by no file line -- survived on every prefix and
no line could match it.  The refusal for an anchored pattern quoted
nothing, which is the shape the observed failure took: a
registration call refused for matching nothing, with no line for the
planner to compare against."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(llm-pick-source-register 'artificial-analysis "
              ":kind 'capability)\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved)
                       (concat "^\\(llm-pick-source-register "
                               "'artificial-analysis.*\\)")
                       "x")
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (let ((message (error-message-string err)))
          (ert-info ((format "Message:\n%S" message))
            (should (string-match-p "matched nothing" message))
            (should (string-match-p "closest lines" message))
            (should (string-match-p
                     (regexp-quote
                      "(llm-pick-source-register 'artificial-analysis")
                     message))))))))

(ert-deftest scalpel-agent-test-rewrite-hint-sees-through-bracket-escapes ()
  "A bracket-group pattern's refusal quotes the line it was aimed at.
Regression: a planner wrote \" \\(transient \"0\\.3\\.0\"\\)\" for a
requirement line holding \" (transient \"0.3.0\")\", which in Emacs
regexp syntax is a group and so matches no brackets, and the refusal
quoted nothing: the pattern as written searches for a backslash the
file never holds, and the walk to shorter prefixes halved 23
characters straight past the lengths that would have landed.  The
sentence naming the reading is added only when that reading is what
placed the lines; the refusal itself is unchanged and nothing is
rewritten."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert ";; Package-Requires: ((emacs \"28.1\") (transient \"0.3.0\"))\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved)
                       " \\(transient \"0\\.3\\.0\"\\)"
                       " ")
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (let ((message (error-message-string err)))
          (ert-info ((format "Message:\n%S" message))
            (should (string-match-p "matched nothing" message))
            ;; The file's own line, quoted: the brackets the pattern
            ;; lost are on it.
            (should (string-match-p
                     (regexp-quote "(transient \"0.3.0\"))") message))
            ;; And the reading those lines came from is named, so the
            ;; next attempt can be written differently.
            (should (string-match-p
                     (regexp-quote
                      scalpel-agent--substitute-bracket-reading-note)
                     message))
            ;; Nothing was written: the refusal still refuses.
            (should (string-match-p
                     "transient \"0.3.0\""
                     (with-temp-buffer
                       (insert-file-contents resolved)
                       (buffer-string))))))))))

(ert-deftest scalpel-agent-test-rewrite-bracket-pattern-without-near-miss-is-bare ()
  "A bracket-group pattern the file holds nowhere adds no sentence.
The sentence about the bracket reading is evidence, not a reflex: a
pattern whose brackets are groups on purpose, and which the file
holds under neither reading, must still refuse with just the pattern
and the files -- a sentence naming a reading that placed nothing
would tell the planner less than nothing."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved) " \\(zzz-absent-identifier\\)" " ")
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (let ((message (error-message-string err)))
          (should (string-match-p "matched nothing" message))
          (should-not (string-match-p "closest lines" message))
          (should-not (string-match-p
                       (regexp-quote
                        scalpel-agent--substitute-bracket-reading-note)
                       message)))))))

(ert-deftest scalpel-agent-test-rewrite-hint-names-bracket-escapes ()
  "A pattern escaping a bracket gets the bracket sentence.
Regression, twice over.  The sentence was first appended only when
the pattern as written placed nothing at all, but its opening
characters land whenever the file shares them -- here the dependency
header matches up to the digits -- so the lines came back with no
reason attached.  It was then made to depend on the bracket-literal
reading placing more of the pattern, which is not decidable either:
the \"\\.\" earlier in this very pattern is searched for as a
backslash the file does not hold, so both readings stop at the same
33 characters and the extra length never appears.

What is decidable is that the bracket-literal reading places a run,
and that is what the sentence now reports.  The pattern itself is
never rewritten: only the refusal's account of the file changes."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert ";; Package-Requires: ((emacs \"28.1\") "
              "(transient \"0.3.0\"))\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved)
                       (concat ";; Package-Requires: ((emacs \"28\\.1\") "
                               "\\(transient \"0\\.3\\.0\"\\))")
                       ";; Package-Requires: ((emacs \"28.1\"))")
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (let ((message (error-message-string err)))
          (ert-info ((format "Message:\n%S" message))
            (should (string-match-p "matched nothing" message))
            ;; The file's own line, quoted, so the entry the pattern
            ;; insists on matching can be compared with what is there.
            (should (string-match-p
                     (regexp-quote
                      (concat ";; Package-Requires: ((emacs \"28.1\") "
                              "(transient \"0.3.0\"))"))
                     message))
            ;; And the reading those lines came from is named, so the
            ;; next attempt can drop the escapes that hid them.
            (should (string-match-p
                     (regexp-quote
                      scalpel-agent--substitute-bracket-reading-note)
                     message))
            ;; The corrected spelling is handed over verbatim, so the
            ;; next attempt can be copied instead of translated.  Its
            ;; dots stay escaped -- a dot is special to Emacs regexp
            ;; syntax -- while its brackets do not.
            (should (string-match-p
                     (regexp-quote
                      (concat "((emacs \"28\\.1\") "
                              "(transient \"0\\.3\\.0\"))"))
                     message))
            (should-not (string-match-p "invalid JSON" message))))))))

(ert-deftest scalpel-agent-test-rewrite-names-an-escaped-literal-no-file-holds ()
  "A zero-match refusal names an escaped literal the files never hold.
Regression: the pattern wrote an ampersand meaning \"whatever text is
already there\" -- the whole-match placeholder of a replacement --
and the refusal quoted the file's lines without saying that no file
holds an ampersand at all, so a pattern that cannot match looked like
a near miss, and the next attempt was spent on it."
  (scalpel-utils-test-with-temp-file ".md"
    (with-temp-file this-file
      (insert "| `some-report-marginal-threshold-ofmarginal-threshold`"
              " | `2.0` |\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved)
                       "some-report-\\(&-of\\&-of\\&\\)"
                       "x")
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (let ((message (error-message-string err)))
          (ert-info ((format "Message:\n%S" message))
            (should (string-match-p "matched nothing" message))
            (should (string-match-p "literal character \"&\"" message))
            (should (string-match-p "no file in the list holds \"&\""
                                    message))))))))

(ert-deftest scalpel-agent-test-rewrite-keeps-quiet-about-a-literal-the-file-holds ()
  "No escape note when the files really hold the escaped character.
The note is evidence, not a reflex: a file that holds an ampersand
must not be reported as lacking one merely because the pattern
escaped it, or the refusal would send the planner after a difference
that is not there."
  (scalpel-utils-test-with-temp-file ".txt"
    (with-temp-file this-file (insert "keep & safe\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved) "zzz\\&zzz" "x")
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (let ((message (error-message-string err)))
          (ert-info ((format "Message:\n%S" message))
            (should (string-match-p "matched nothing" message))
            (should-not (string-match-p "literal character" message))))))))

(ert-deftest scalpel-agent-test-rewrite-malformed-signals ()
  "A rewrite without files, pattern or replacement signals."
  (should-error (scalpel-agent-file-substitute nil "x" "y") :type 'user-error)
  (should-error (scalpel-agent-file-substitute '("/tmp/a.el") nil "y")
                :type 'user-error))

(ert-deftest scalpel-agent-test-rewrite-malformed-replacement-names-the-pair ()
  "A refused replacement names the pattern and the replacement itself.
Regression: the refusal said only that the replacement was malformed,
so the other half of the action had to be restored from memory before
another one could be written -- and memory wrote the malformed half.
`scalpel-agent--substitute-invocation' is what every file-substitute
refusal now carries for that."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file (insert "(defun foo ())\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (pattern "(defun foo")
           ;; A trailing backslash is not a valid replacement text; the
           ;; pattern matches, so the replacement is really read.
           (replacement "\\")
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved) pattern replacement)
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (let ((message (error-message-string err)))
          (ert-info ((format "Message:\n%S" message))
            (should (string-match-p "replacement is malformed" message))
            (should (string-match-p (regexp-quote resolved) message))
            (should (string-match-p
                     (regexp-quote
                      (scalpel-agent--substitute-invocation
                       pattern replacement))
                     message))))))))

(ert-deftest scalpel-agent-test-rewrite-bracket-correction-is-verified ()
  "A corrected spelling is offered only when it really matches the file.
Regression risk: the sentence that hands over the bracket-literal
reading was derived from the pattern alone, so a refusal could offer a
\"corrected\" pattern that fails for the same reason the original did,
and the next attempt would copy it.  Here the brackets are not the
whole difference -- the version asked for is absent as well -- so the
refusal must say the correction matches nothing instead of presenting
it as the pattern to use."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert ";; Package-Requires: ((emacs \"28.1\") "
              "(transient \"0.3.0\"))\n"))
    (let* ((resolved (file-truename (expand-file-name this-file)))
           (scalpel-agent--context-files (list resolved))
           (pattern (concat ";; Package-Requires: ((emacs \"28\\.1\") "
                            "\\(transient \"9\\.9\\.9\"\\))"))
           (err (condition-case e
                    (progn
                      (scalpel-agent-file-substitute
                       (list resolved) pattern "x")
                      nil)
                  (user-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (let ((message (error-message-string err)))
          (ert-info ((format "Message:\n%S" message))
            (should (string-match-p "matched nothing" message))
            ;; The syntax rule and the file's own line are still the
            ;; evidence, and both are true whether or not the reading
            ;; lands.
            (should (string-match-p
                     (regexp-quote
                      scalpel-agent--substitute-bracket-reading-note)
                     message))
            (should (string-match-p
                     (regexp-quote "(transient \"0.3.0\"))") message))
            ;; The spelling that matches nothing is not handed over as
            ;; the pattern to use next; the refusal says so instead.
            (should (string-match-p
                     (regexp-quote
                      scalpel-agent--substitute-bracket-correction-fails)
                     message))
            (should-not
             (string-match-p
              (regexp-quote
               (format scalpel-agent--substitute-bracket-correction-matches
                       (scalpel-agent--substitute-literal-brackets pattern)))
              message))))))))

(ert-deftest scalpel-agent-test-rewrite-summary-shows-pattern-and-count ()
  "The confirmation prompt names the pattern and the file count.
Regression: the summary fell through to :reason, so the user
confirmed a rewrite without seeing what it would match."
  (should (string=
           (scalpel-agent--action-summary
            '(:tool "file-substitute" :pattern "old-" :replacement "new-"
                    :files ("/a.el" "/b.el") :reason "bulk"))
           "old- over 2 file(s)")))

(ert-deftest scalpel-agent-test-run-records-rewrites-for-continuation ()
  "A rewrite round carries its report in :changes, so the console continues.
Regression: only `block-edit' and `block-insert' entered the round's
change list, so a rewrite-only round ended the loop and the planner
never read its own occurrence counts."
  (cl-destructuring-bind (dir f1 _f2) (scalpel-agent-test--stage-two-files)
    (unwind-protect
        (let ((scalpel-agent--context-files (list (file-truename f1)))
              (scalpel-console--root nil)
              (orig-llm (symbol-function 'scalpel-llm-request-async))
              (orig-yes (symbol-function 'yes-or-no-p))
              result)
          (unwind-protect
              (progn
                (fset 'yes-or-no-p (lambda (&rest _) t))
                (fset 'scalpel-llm-request-async
                      (lambda (_p on-success _on-error &optional _s)
                        (funcall on-success
                                 (concat "[{\"tool\":\"file-substitute\","
                                         "\"files\":[\"" f1 "\"],"
                                         "\"pattern\":\"old-\","
                                         "\"replacement\":\"new-\"}]"))))
                (scalpel-agent-run "bulk rename" nil
                                   (lambda (r) (setq result r))
                                   (lambda (e) (ert-fail (plist-get e :message))))
                (ert-info ((format "Result: %S" result))
                  (should (= (length (plist-get result :changes)) 1))
                  (should (string-match-p "Rewrote 1 occurrence"
                                          (plist-get result :report)))))
            (fset 'scalpel-llm-request-async orig-llm)
            (fset 'yes-or-no-p orig-yes)))
      (dolist (f (directory-files dir t "^[^.]"))
        (scalpel-utils-test-kill-file-buffer f))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-run-records-a-created-file-as-a-change ()
  "A round that created a file reports it as a change.
Regression: the round's change list carried edits, inserts and
rewrites only, so a round whose only action was a file-level change
-- a create, a rename, a delete -- reported nothing, and the console
loop stopped after it: the planner never received the continuation
instruction, and the file it had just made was never read back.
The action is a real one, not a mock, because `file-create' is the
writing tool a planner reaches for most often."
  (let ((dir (make-temp-file "scalpel-test-new-" t))
        (scalpel-agent--context-files nil)
        (orig-llm (symbol-function 'scalpel-llm-request-async))
        result)
    (unwind-protect
        (with-temp-buffer
          (unwind-protect
              (progn
                (fset 'scalpel-llm-request-async
                      (lambda (_p on-success _on-error &optional _s)
                        (funcall on-success
                                 (format (concat "[{\"tool\":\"file-create\","
                                                 "\"file\":%S,"
                                                 "\"text\":\"(defun a ())\"}]")
                                         (expand-file-name "new.el" dir)))))
                (scalpel-agent-run
                 "create it" nil
                 (lambda (r) (setq result r))
                 (lambda (e) (ert-fail (plist-get e :message))))
                (ert-info ((format "Result: %S" result))
                  (should (= (length (plist-get result :changes)) 1))
                  (should (string-match-p
                           "Created file"
                           (car (plist-get result :changes))))))
            (fset 'scalpel-llm-request-async orig-llm)))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-absent-symbol-describes-its-namesake-file ()
  "A symbol named after a context file is answered with that file's contents.
Regression: the zero-hit refusal described the file the planner asked
for and nothing else, so the file the name was actually about --
`llm-pick-fetch-get' for `llm-pick-fetch-get.el' -- was invisible: the
scan that found nothing had walked every context file, and only the
asked one's answer was kept.  A name one separator away from what the
file holds is then the one thing the message never shows."
  (let ((dir (make-temp-file "scalpel-test-namesake-" t)))
    (unwind-protect
        (let ((asked (expand-file-name "asked.el" dir))
              (namesake (expand-file-name "target-spec.el" dir)))
          (with-temp-file asked (insert "(defun elsewhere ())\n"))
          ;; The namesake holds the name one separator away, which is what
          ;; the planner's own spelling of a module name usually is.
          (with-temp-file namesake (insert "(defun target--spec ())\n"))
          (let ((scalpel-agent--context-files
                 (mapcar #'file-truename (list asked namesake))))
            (let ((err (condition-case e
                           (progn (scalpel-agent--resolve-symbol
                                   (car scalpel-agent--context-files)
                                   "target-spec")
                                  nil)
                         (user-error e))))
              (ert-info ((format "Error: %S" err))
                (should err)
                (let ((message (error-message-string err)))
                  (ert-info ((format "Message:\n%S" message))
                    ;; The asked file is still described...
                    (should (string-match-p "asked\\.el" message))
                    ;; ...and so is the file named after the symbol, with
                    ;; the spelling it really holds.
                    (should (string-match-p "target-spec\\.el" message))
                    (should (string-match-p "defines 1 definition" message))
                    (should (string-match-p
                             (regexp-quote
                              (concat "spells target--spec where target-spec "
                                      "was asked for"))
                             message))))))))
      (dolist (file (directory-files dir t "^[^.]"))
        (scalpel-utils-test-kill-file-buffer file))
      (delete-directory dir t))))

(ert-deftest scalpel-agent-test-absent-symbol-omits-a-near-line-that-repeats-the-list ()
  "A near-name line naming the whole list is left out.
Regression: the two lines carried the same names whenever every name
a file holds is spelled like the one asked for -- which is what a file
named after the symbol looks like -- and the refusal printed them
twice.  The message joins the conversation, so the duplication was
re-sent on every later round; `llm-pick-fetch-get.el' printed eight
names on each of the two lines.  The names are still stated, once, in
the definition list below the header."
  (scalpel-utils-test-with-temp-file ".el"
    (with-temp-file this-file
      (insert "(defun thing-one ())\n(defun thing-two ())\n"))
    (let ((scalpel-agent--context-files
           (list (file-truename (expand-file-name this-file)))))
      (let ((err (condition-case e
                     (progn (scalpel-agent--resolve-symbol
                             (car scalpel-agent--context-files) "thing")
                            nil)
                   (user-error e))))
        (ert-info ((format "Error: %S" err))
          (should err)
          (let ((message (error-message-string err)))
            (ert-info ((format "Message:\n%S" message))
              (should (string-match-p "defines 2 definition" message))
              (should-not (string-match-p "Definitions spelled like"
                                          message))
              (should (string-match-p
                       "It defines: thing-one, thing-two" message)))))))))

(provide 'scalpel-agent-test)

;;; scalpel-agent-test.el ends here
