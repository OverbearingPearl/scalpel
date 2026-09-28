;;; scalpel-llm-dialect-test.el --- Tests for scalpel-llm-dialect -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the reply-dialect registry and the default parser.
;; No LLM is contacted: the parsers are pure functions of their input.
;; The plan contract is TOML with single-quoted literal strings, so the
;; default-parse tests speak TOML.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-llm-dialect)
(require 'scalpel-utils-test)

(ert-deftest scalpel-llm-dialect-test-default-parse-action-table-array ()
  "An [[action]] table array parses to one plist per table."
  (ert-info ("Input: two [[action]] tables; expect two actions")
    (let ((result (scalpel-llm-dialect--default-parse
                   "[[action]]\ntool = 'reply'\ntext = 'hi'\n\n[[action]]\ntool = 'reply'\ntext = 'there'\n")))
      (should (equal (length result) 2))
      (should (equal (plist-get (car result) :tool) "reply"))
      (should (equal (plist-get (car result) :text) "hi"))
      (should (equal (plist-get (cadr result) :text) "there")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-single-table ()
  "A single top-level table with a tool key wraps into a one-element list."
  (ert-info ("Input: one top-level table; expect a one-element list")
    (let ((result (scalpel-llm-dialect--default-parse
                   "tool = 'reply'\ntext = 'x'\n")))
      (should (equal (length result) 1))
      (should (equal (plist-get (car result) :tool) "reply")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-multiline-literal ()
  "A ''' multiline literal keeps its newlines verbatim."
  (ert-info ("Input: heredoc text; expect newlines preserved")
    (let ((result (scalpel-llm-dialect--default-parse
                   "[[action]]\ntool = 'reply'\ntext = '''line one\nline two'''\n")))
      (should (equal (plist-get (car result) :text) "line one\nline two")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-backslashes-verbatim ()
  "A regex inside a ''' literal arrives with its backslashes whole."
  (ert-info ("Input: \\s in a pattern; expect the same text back")
    (let ((result (scalpel-llm-dialect--default-parse
                   "[[action]]\ntool = 'file-substitute'\nfiles = ['/tmp/a.el']\npattern = '''\\s+$'''\nreplacement = ''\n")))
      (should (equal (plist-get (car result) :pattern) "\\s+$")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-multiple-fields ()
  "Every field of an action table survives parsing."
  (let ((raw "[[action]]\ntool = 'block-edit'\nfile = '/tmp/foo.el'\nsymbol = 'bar'\ninstruction = 'do something'\n"))
    (let ((actions (scalpel-llm-dialect--default-parse raw)))
      (should (= (length actions) 1))
      (should (equal (plist-get (car actions) :tool) "block-edit"))
      (should (equal (plist-get (car actions) :file) "/tmp/foo.el"))
      (should (equal (plist-get (car actions) :symbol) "bar"))
      (should (equal (plist-get (car actions) :instruction) "do something")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-empty-reply-signals ()
  "An empty reply is reported as a backend failure, not bad TOML.
Regression: an empty reply fell into the generic invalid-reply error,
whose message gave no hint that the cause was the backend, not the
reply's syntax."
  (ert-info ("Input: empty and whitespace-only replies; expect user-error naming the empty reply")
    (should-error (scalpel-llm-dialect--default-parse "")
                  :type 'user-error)
    (let ((err (condition-case e
                   (progn (scalpel-llm-dialect--default-parse "  \n") nil)
                 (user-error e))))
      (should err)
      (should (string-match-p "empty reply"
                              (error-message-string err))))))

(ert-deftest scalpel-llm-dialect-test-default-parse-double-quotes-outside-blocks ()
  "Double quotes outside ''' blocks violate the single-quote contract."
  (ert-info ("Input: double-quoted reply; expect user-error")
    (should-error
     (scalpel-llm-dialect--default-parse
      "[{\"tool\":\"reply\",\"text\":\"hi\"}]")
     :type 'user-error)))

(ert-deftest scalpel-llm-dialect-test-default-parse-prose-signals ()
  "A plain prose reply signals `user-error'."
  (ert-info ("Input: plain prose; expect user-error")
    (should-error (scalpel-llm-dialect--default-parse "no toml here")
                  :type 'user-error)))

(ert-deftest scalpel-llm-dialect-test-default-parse-unterminated-block-signals ()
  "An odd number of ''' fences means a truncated heredoc and signals."
  (ert-info ("Input: heredoc opened but never closed; expect user-error")
    (should-error
     (scalpel-llm-dialect--default-parse
      "[[action]]\ntool = 'reply'\ntext = '''unterminated\n")
     :type 'user-error)))

(ert-deftest scalpel-llm-dialect-test-default-parse-tool-call-syntax-signals ()
  "XML tool-call markup is named in the error, not reported as bad TOML.
The detector runs before the TOML
parse, so a reply in another calling convention is classified, not
merely rejected."
  (let ((raw (concat "I'll look around.\n\n"
                     "<invoke name=\"shell\">\n"
                     "<parameter name=\"command\">ls</parameter>\n"
                     "</invoke>")))
    (let ((err (condition-case e
                   (progn (scalpel-llm-dialect--default-parse raw) nil)
                 (user-error e))))
      (ert-info ((format "Raw:\n%S" raw))
        (should err)
        (should (string-match-p "tool-call syntax"
                                (error-message-string err)))))))

(ert-deftest scalpel-llm-dialect-test-parse-error-names-literal-string-causes ()
  "Pin the literal-string diagnostics of `scalpel-llm-dialect--parse-error'."
  (let ((triple-run-end
         "[[tool]]\nname = 'grep'\ntext = '''\nabc\n'''\n'''")
        (truncated
         "[[tool]]\nname = 'grep'\ntext = '''\nabc")
        (backslash-before-quote
         "[[tool]]\nname = 'grep'\ncommand = 'ls\\' 'file'")
        (plain-invalid
         "[[tool]]\nname = 'grep'\nthis line is not valid toml"))
    (ert-info ((format "Reply with an odd fence count ending on a triple-quote run signals three consecutive quotes; input: %S" triple-run-end))
      (let ((msg (condition-case err
                     (progn (scalpel-llm-dialect--parse-error triple-run-end) nil)
                   (user-error (error-message-string err)))))
        (should msg)
        (ert-info ((format "Expected 'three consecutive' and no 'cut off'; actual: %S" msg))
          (should (string-match-p "three consecutive" msg))
          (should-not (string-match-p "cut off" msg)))))
    (ert-info ((format "Reply truncated inside an open literal value signals cut off; input: %S" truncated))
      (let ((msg (condition-case err
                     (progn (scalpel-llm-dialect--parse-error truncated) nil)
                   (user-error (error-message-string err)))))
        (should msg)
        (ert-info ((format "Expected 'cut off'; actual: %S" msg))
          (should (string-match-p "cut off" msg)))))
    (ert-info ((format "Reply with a backslash before a single quote in a command value signals backslash; input: %S" backslash-before-quote))
      (let ((msg (condition-case err
                     (progn (scalpel-llm-dialect--parse-error backslash-before-quote) nil)
                   (user-error (error-message-string err)))))
        (should msg)
        (ert-info ((format "Expected 'backslash'; actual: %S" msg))
          (should (string-match-p "backslash" msg)))))
    (ert-info ((format "Reply invalid for an unrelated reason falls back to the generic invalid TOML message; input: %S" plain-invalid))
      (let ((msg (condition-case err
                     (progn (scalpel-llm-dialect--parse-error plain-invalid) nil)
                   (user-error (error-message-string err)))))
        (should msg)
        (ert-info ((format "Expected 'invalid TOML' and none of 'backslash', 'cut off', 'three consecutive'; actual: %S" msg))
          (should (string-match-p "invalid TOML" msg))
          (should-not (string-match-p "backslash" msg))
          (should-not (string-match-p "cut off" msg))
          (should-not (string-match-p "three consecutive" msg)))))))

(ert-deftest scalpel-llm-dialect-test-tool-call-detection-covers-every-tag ()
  "Every registered tool-call tag makes the reply fail as tool-call syntax.
The list is the contract: a dialect added to it is detected with no
further change, so detection cannot silently cover only the
dialects some test happened to spell out."
  (dolist (tag scalpel-llm-dialect--tool-call-tags)
    (let ((raw (format "thinking...<%s>x</%s>" tag tag)))
      (ert-info ((format "Tag: %S" tag))
        (let ((err (condition-case e
                       (progn (scalpel-llm-dialect--parse-error raw) nil)
                     (user-error e))))
          (should err)
          (should (string-match-p "tool-call syntax"
                                  (error-message-string err))))))))

(ert-deftest scalpel-llm-dialect-test-register-replaces-same-regexp ()
  "Registering the same regexp replaces the previous provider."
  (ert-info ("Register twice under one regexp; expect one entry, the newest")
    (let ((scalpel-llm-dialect-providers nil))
      (scalpel-llm-dialect-register "foo" (list :parse-reply #'identity))
      (scalpel-llm-dialect-register "foo" (list :parse-reply #'list))
      (should (= (length scalpel-llm-dialect-providers) 1))
      (should (eq (plist-get (cdr (car scalpel-llm-dialect-providers))
                             :parse-reply)
                  #'list)))))

(ert-deftest scalpel-llm-dialect-test-parse-dispatches-to-provider ()
  "A provider registered for the active backend handles the reply."
  (ert-info ("Bind a fake provider and backend name; expect it called")
    (let* ((seen nil)
           (stub (lambda (raw)
                   (setq seen raw)
                   (list (list :tool "reply"))))
           (scalpel-llm-dialect-providers
            (list (cons "stub-backend" (list :parse-reply stub))))
           (saved-backend gptel-backend))
      (unwind-protect
          (progn
            (setq gptel-backend
                  (gptel-make-openai "stub-backend" :key "test-key"))
            (should (equal (scalpel-llm-dialect-parse "anything")
                           (list (list :tool "reply"))))
            (should (equal seen "anything")))
        (setq gptel-backend saved-backend)
        (scalpel-utils-test-delete-backend "stub-backend")))))

(ert-deftest scalpel-llm-dialect-test-parse-dispatches-on-the-model-name ()
  "A dialect registered for the model is found through a provider backend."
  (let* ((seen nil)
         (stub (lambda (raw)
                 (setq seen raw)
                 (list (list :tool "reply"))))
         (scalpel-llm-dialect-providers
          (list (cons "laguna" (list :parse-reply stub))))
         (saved-backend gptel-backend))
    (unwind-protect
        (progn
          (setq gptel-backend
                (gptel-make-openai "OpenRouter" :key "test-key"
                                   :models '("poolside/laguna-s-2.1")))
          (with-temp-buffer
            (set (make-local-variable 'gptel-model)
                 "poolside/laguna-s-2.1")
            (should (equal (scalpel-llm-dialect-parse "anything")
                           (list (list :tool "reply"))))
            (should (equal seen "anything"))))
      (setq gptel-backend saved-backend)
      (scalpel-utils-test-delete-backend "OpenRouter"))))

(ert-deftest scalpel-llm-dialect-test-parse-error-does-not-echo-broken-reply-as-retry-template ()
  "Regression test for the echo-disinfection in `scalpel-llm-dialect--parse-error'.
Guards against the feedback loop where the error message echoes the
broken raw reply verbatim: on retry the model then treats the echoed
broken reply as a template and copies its malformation instead of
following the format prescribed by the system prompt.  The message must
say that the shown reply is the broken sample and that a retry must
follow the system prompt's format, not imitate the sample."
  (let* ((raw "[last-table]\nkey = \"value\"\n]]\n")
         (err (should-error
               (scalpel-llm-dialect--parse-error raw)
               :type 'user-error))
         (msg (error-message-string err)))
    (ert-info ("Expect the error message to label the shown reply as a broken sample shown for diagnosis only.")
      (should (string-match-p
               (regexp-quote
                "was itself the broken attempt and must not be copied")
               msg)))
    (ert-info ("Expect the error message to require that a retry follow the format given in the system prompt, not imitate the broken sample.")
      (should (string-match-p
               (regexp-quote
                "the retry must follow the format shown in the system prompt instead")
               msg)))
    (ert-info ("Expect the error message to contain the raw broken reply text verbatim.")
      (should (string-match-p (regexp-quote raw) msg)))))

(provide 'scalpel-llm-dialect-test)

;;; scalpel-llm-dialect-test.el ends here
