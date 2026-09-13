;;; scalpel-llm-dialect-test.el --- Tests for scalpel-llm-dialect -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the reply-dialect registry and the default parser.
;; No LLM is contacted: the parsers are pure functions of their input.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-llm-dialect)

(ert-deftest scalpel-llm-dialect-test-default-parse-plain-array ()
  "A bare JSON action array parses to one plist per object."
  (ert-info ("Input: a bare array; expect one action with :tool reply")
    (let ((result (scalpel-llm-dialect--default-parse
                   "[{\"tool\":\"reply\",\"text\":\"hi\"}]")))
      (should (equal (length result) 1))
      (should (equal (plist-get (car result) :tool) "reply"))
      (should (equal (plist-get (car result) :text) "hi")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-single-object ()
  "A single JSON object is accepted and wrapped into a one-element list."
  (ert-info ("Input: one object; expect a one-element list")
    (let ((result (scalpel-llm-dialect--default-parse
                   "{\"tool\":\"reply\",\"text\":\"x\"}")))
      (should (equal (length result) 1))
      (should (equal (plist-get (car result) :tool) "reply")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-prose-wrapped ()
  "Prose and markdown fences around the payload are ignored."
  (ert-info ("Input: fenced array with prose; expect the array parsed")
    (let ((result (scalpel-llm-dialect--default-parse
                   "Here is the plan:\n```json\n[{\"tool\":\"reply\",\"text\":\"ok\"}]\n```")))
      (should (equal (plist-get (car result) :tool) "reply")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-bracket-in-string ()
  "A bracket inside a JSON string must not end the payload early."
  (ert-info ("Input: array whose string holds ']'; expect full parse")
    (let ((result (scalpel-llm-dialect--default-parse
                   "[{\"tool\":\"reply\",\"text\":\"echo ']'\"}]")))
      (should (equal (plist-get (car result) :text) "echo ']'")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-raw-control-escaped ()
  "Literal newlines inside JSON strings are escaped before parsing."
  (ert-info ("Input: string value holding a raw newline; expect it parsed")
    (let ((result (scalpel-llm-dialect--default-parse
                   "[{\"tool\":\"reply\",\"text\":\"a\nb\"}]")))
      (should (equal (plist-get (car result) :text) "a\nb")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-empty-reply-signals ()
  "An empty reply is reported as a backend failure, not bad JSON.
Regression: an empty reply fell into the generic invalid-JSON error,
whose message showed `%s' as \"\" and gave no hint that the cause
was the backend, not the reply's syntax."
  (ert-info ("Input: empty and whitespace-only replies; expect user-error naming the empty reply")
    (should-error (scalpel-llm-dialect--default-parse "")
                  :type 'user-error)
    (let ((err (condition-case e
                   (progn (scalpel-llm-dialect--default-parse "  \n") nil)
                 (user-error e))))
      (should err)
      (should (string-match-p "empty reply"
                              (error-message-string err))))))

(ert-deftest scalpel-llm-dialect-test-default-parse-no-json-signals ()
  "A reply with no JSON payload signals `user-error'."
  (ert-info ("Input: plain prose; expect user-error naming the prose reply")
    (should-error (scalpel-llm-dialect--default-parse "no json here")
                  :type 'user-error)))

(ert-deftest scalpel-llm-dialect-test-default-parse-unexpected-structure ()
  "A JSON array of non-action objects signals `user-error'."
  (ert-info ("Input: array of strings; expect user-error")
    (should-error (scalpel-llm-dialect--default-parse "[\"a\",\"b\"]")
                  :type 'user-error)))

(ert-deftest scalpel-llm-dialect-test-default-parse-multiple-fields ()
  "Every field of an action object survives parsing.
Moved from `scalpel-agent-test-parse-json' when parsing became this
module's job: the agent no longer owns a JSON parser."
  (let ((raw "[{\"tool\":\"edit\",\"file\":\"/tmp/foo.el\",\"symbol\":\"bar\",\"instruction\":\"do something\"}]"))
    (let ((actions (scalpel-llm-dialect--default-parse raw)))
      (should (= (length actions) 1))
      (should (equal (plist-get (car actions) :tool) "edit"))
      (should (equal (plist-get (car actions) :file) "/tmp/foo.el"))
      (should (equal (plist-get (car actions) :symbol) "bar"))
      (should (equal (plist-get (car actions) :instruction) "do something")))))

(ert-deftest scalpel-llm-dialect-test-default-parse-object-without-tool-signals ()
  "A single JSON object that is not an action signals `user-error'.
Moved from `scalpel-agent-test-parse-json-object-not-array'."
  (ert-info ("Input: an object wrapping an action array; expect user-error")
    (should-error
     (scalpel-llm-dialect--default-parse
      "{\"actions\":[{\"tool\":\"reply\",\"text\":\"hi\"}]}")
     :type 'user-error)))

(ert-deftest scalpel-llm-dialect-test-parse-error-names-truncated-json ()
  "A reply whose JSON array never closes is reported as truncated.
Regression: an unterminated array fell into the generic invalid-JSON
error, sending the user hunting for a syntax error when the real
cause was the backend cutting the reply off."
  (let ((raw "[{\"tool\":\"reply\",\"text\":\"\\u4f60\\u597d"))
    (let ((err (condition-case e
                   (progn (scalpel-llm-dialect--default-parse raw) nil)
                 (user-error e))))
      (ert-info ((format "Raw:\n%S" raw))
        (should err)
        (should (string-match-p "cut off" (error-message-string err)))))))

(ert-deftest scalpel-llm-dialect-test-json-payload-without-json ()
  "A reply holding no complete JSON value has no payload."
  (ert-info ("Input: prose and an unterminated array; expect nil both times")
    (should-not (scalpel-llm-dialect--json-payload
                 "There is nothing to change."))
    (should-not (scalpel-llm-dialect--json-payload "[unterminated"))))

(ert-deftest scalpel-llm-dialect-test-parse-error-names-tool-call-syntax ()
  "XML tool-call markup is named in the error, not reported as bad JSON.
Moved from `scalpel-agent-test-parse-json-rejects-tool-call-syntax'."
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

(ert-deftest scalpel-llm-dialect-test-parse-error-names-tool-call-tag-syntax ()
  "A reply in the `<tool_call>` dialect is reported as tool-call syntax.
Regression: laguna and GLM leak `<tool_call>` with `<arg_key>` and
`<arg_value>` children, while the detector knew only `<invoke>', so
the reply fell through to the generic invalid-JSON error and sent
the user hunting for a JSON syntax error that did not exist."
  (let ((raw (concat "I'll investigate the full scope of gptel usage "
                     "before giving advice. Let me read the key files."
                     "<tool_call>read<arg_key>file</arg_key>"
                     "<arg_value>/tmp/a.el</arg_value></tool_call>")))
    (let ((err (condition-case e
                   (progn (scalpel-llm-dialect--default-parse raw) nil)
                 (user-error e))))
      (ert-info ((format "Raw:\n%S" raw))
        (should err)
        (should (string-match-p "tool-call syntax"
                                (error-message-string err)))
        (should-not (string-match-p "invalid JSON"
                                    (error-message-string err)))))))

(ert-deftest scalpel-llm-dialect-test-tool-call-error-keeps-the-reply-visible ()
  "The tool-call error carries its own type and still quotes the reply.
Regression: splitting the type out of the plain `user-error' is what
lets the console change its advice, and the message must survive
that change -- the reply is the only evidence the user has of what
the model did instead of planning.  The message is read through
`scalpel-llm-dialect-error-message', because `error-message-string'
renders a `define-error' condition as \"class: data\", doubling the
sentence and re-escaping the reply; the literal reply compared
below is what fails if that renderer is used again."
  (let ((raw (concat "<tool_call>shell<arg_key>command</arg_key>"
                     "<arg_value>ls</arg_value></tool_call>")))
    (let ((err (condition-case e
                   (progn (scalpel-llm-dialect--default-parse raw) nil)
                 (scalpel-llm-dialect-tool-call-error e))))
      (ert-info ((format "Error: %S" err))
        (should err)
        (should (eq (car err) 'scalpel-llm-dialect-tool-call-error))
        (let ((message (scalpel-llm-dialect-error-message err)))
          (ert-info ((format "Message: %S" message))
            (should (string-match-p "tool-call syntax" message))
            (should (string-match-p
                     (regexp-quote (scalpel-llm-dialect--visible-raw raw))
                     message))))))))

(ert-deftest scalpel-llm-dialect-test-tool-call-type-is-a-user-error ()
  "The tool-call condition stays a `user-error'.
Regression: the console handler and its tests catch a parse failure
as `user-error'; a type that stopped deriving from it would slip
past the console's error handler and surface as a backtrace."
  (should (memq 'user-error
                (get 'scalpel-llm-dialect-tool-call-error
                     'error-conditions)))
  (should-error (scalpel-llm-dialect--parse-error "<tool_call>x</tool_call>")
                :type 'user-error))

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

(ert-deftest scalpel-llm-dialect-test-tool-call-markup-inside-json-still-parses ()
  "A JSON action array quoting tool-call markup is still a valid reply.
Detection runs only when no payload was found, so widening the tag
list cannot turn a well-formed array into a parse error."
  (let ((result (scalpel-llm-dialect--default-parse
                 "[{\"tool\":\"reply\",\"text\":\"use <tool_call> instead\"}]")))
    (ert-info ((format "Result: %S" result))
      (should (equal (plist-get (car result) :text)
                     "use <tool_call> instead")))))

(ert-deftest scalpel-llm-dialect-test-visible-raw-exposes-invisible-bytes ()
  "A reply that fails to parse must expose the bytes that broke it.
Regression: the error used %S, which prints control bytes and NBSP
literally, so the offending character could not be seen.  Moved from
`scalpel-agent-test-visible-raw-exposes-invisible-bytes'."
  (let ((shown (scalpel-llm-dialect--visible-raw "a\tb\u00A0c")))
    (ert-info ((format "Shown: %S" shown))
      ;; Nothing invisible may survive: a literal TAB or NBSP in the
      ;; error message is exactly as unreadable as the original.
      (should-not (string-match-p "[\t\u00A0]" shown))
      (should (string-match-p "\\\\" shown)))))

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
      ;; `gptel-make-openai' may setq `gptel-backend' as a side effect
      ;; when it is nil; save and restore the global so the test never
      ;; leaks a stub backend into the user session.
      (unwind-protect
          (progn
            (setq gptel-backend
                  ;; A real backend object is built so
                  ;; `gptel-backend-name' works; no request is ever sent.
                  (gptel-make-openai "stub-backend" :key "test-key"))
            (should (equal (scalpel-llm-dialect-parse "anything")
                           (list (list :tool "reply"))))
            (should (equal seen "anything")))
        (setq gptel-backend saved-backend)))))

(ert-deftest scalpel-llm-dialect-test-parse-dispatches-on-the-model-name ()
  "A dialect registered for the model is found through a provider backend.
Regression: dispatch keyed off `gptel-backend-name' alone, so a
Laguna model reached through a backend named \"OpenRouter\" -- the
shape the README configures -- fell through to the default parser,
which refused the reply as tool-call syntax even though
`scalpel-llm-laguna' was loaded and had registered its parser."
  (let* ((seen nil)
         (stub (lambda (raw)
                 (setq seen raw)
                 (list (list :tool "reply"))))
         (scalpel-llm-dialect-providers
          (list (cons "laguna" (list :parse-reply stub))))
         (saved-backend gptel-backend))
    ;; `gptel-make-openai' may setq `gptel-backend' as a side effect
    ;; when it is nil; save and restore the global so the test never
    ;; leaks a stub backend into the user session.
    (unwind-protect
        (progn
          (setq gptel-backend
                (gptel-make-openai "OpenRouter" :key "test-key"
                                   :models '("poolside/laguna-s-2.1")))
          ;; The model lives in a buffer-local, matching gptel's own
          ;; storage; a temp buffer keeps the binding out of the
          ;; session the moment this test ends.
          (with-temp-buffer
            (set (make-local-variable 'gptel-model)
                 "poolside/laguna-s-2.1")
            (should (equal (scalpel-llm-dialect-parse "anything")
                           (list (list :tool "reply"))))
            (should (equal seen "anything"))))
      (setq gptel-backend saved-backend))))

(ert-deftest scalpel-llm-dialect-test-parse-leaves-unregistered-models-alone ()
  "A backend and model no provider matches keep the default parser.
Regression risk: matching every name, or the model name of a
backend serving another model, would hand a reply this module was
not written for to a provider registered for a different one."
  (let ((scalpel-llm-dialect-providers
         (list (cons "laguna" (list :parse-reply (lambda (_raw) :wrong)))))
        (saved-backend gptel-backend))
    (unwind-protect
        (progn
          (setq gptel-backend
                (gptel-make-openai "OpenRouter" :key "test-key"
                                   :models '("minimax/minimax-m2.7")))
          (with-temp-buffer
            (set (make-local-variable 'gptel-model)
                 "minimax/minimax-m2.7")
            (let ((parsed (scalpel-llm-dialect-parse
                           "[{\"tool\":\"reply\",\"text\":\"hi\"}]")))
              (ert-info ((format "Parsed: %S" parsed))
                (should (equal (plist-get (car parsed) :tool) "reply"))))))
      (setq gptel-backend saved-backend))))

(ert-deftest scalpel-llm-dialect-test-parse-ignores-a-model-the-backend-does-not-serve ()
  "A model name the active backend does not declare selects no dialect.
Regression: dispatch consulted `gptel-model' on its own, so a name
left over from another backend -- the value survives creating or
selecting a backend -- made a dialect answer for a backend that does
not serve that model, and the reply was read in a convention the
session was never configured for."
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
                                   :models '("minimax/minimax-m2.7")))
          (with-temp-buffer
            (set (make-local-variable 'gptel-model)
                 "poolside/laguna-s-2.1")
            (let ((parsed (scalpel-llm-dialect-parse
                           "[{\"tool\":\"reply\",\"text\":\"hi\"}]")))
              (ert-info ((format "Parsed: %S" parsed))
                (should (equal (plist-get (car parsed) :tool) "reply"))
                (should-not seen)))))
      (setq gptel-backend saved-backend))))

(ert-deftest scalpel-llm-dialect-test-parse-error-names-a-prose-reply ()
  "A reply holding no JSON at all is reported as prose, not bad JSON.
Regression: the message said \"invalid JSON\" about a reply that
contained no JSON value, which is the category error the empty-reply
branch of `scalpel-llm-dialect--default-parse' exists to avoid: it
sent the user hunting for a syntax error that was not there.  The
reply below is the shape a planner really produced when asked to
analyse a dependency -- prose, code spans, and no array anywhere."
  (let* ((raw (concat "The gptel dependency lives in two layers.\n\n"
                      "**The gateway** is the hard coupling: it calls\n"
                      "`gptel-request` and reads `gptel-backend`.\n"))
         (err (condition-case e
                  (progn (scalpel-llm-dialect--default-parse raw) nil)
                (user-error e))))
    (ert-info ((format "Raw:\n%S" raw))
      (should err)
      ;; Caught as the `user-error' it derives from, so the console's
      ;; own error handler still catches it, but carrying a type of its
      ;; own: the console advises differently on a prose answer than on
      ;; a malformed array, and both used to arrive as `parse'.
      (should (eq (car err) 'scalpel-llm-dialect-prose-reply-error))
      (should (string-match-p "prose" (error-message-string err)))
      (should-not (string-match-p "invalid JSON"
                                  (error-message-string err)))
      ;; The reply stays visible, as it was written: it is the only
      ;; evidence the user has of what the planner wrote instead of an
      ;; action array, and it is the answer, so its own line breaks are
      ;; kept rather than escaped into one unreadable line.
      (should (string-match-p (regexp-quote raw)
                              (error-message-string err)))
      (should-not (string-match-p "\\\\n"
                                  (error-message-string err)))
      ;; The remedy belongs to the console's advice, not here: the
      ;; message is printed next to that advice, and it also joins the
      ;; conversation, so a remedy written in both places is read twice
      ;; and re-sent on every later round.
      (should-not (string-match-p "rephrase" (error-message-string err))))))

(ert-deftest scalpel-llm-dialect-test-prose-reply-drops-unprintable-bytes ()
  "A prose reply is shown without bytes the console cannot render.
A bell byte would make the console ring, and a C1 byte would hide
the text after it.  Unlike the branches that report a failed parse
-- where an invisible byte is the evidence, which is why
`scalpel-llm-dialect--visible-raw' escapes it -- there is no syntax
here for the byte to explain."
  (let ((err (condition-case e
                 (progn (scalpel-llm-dialect--default-parse
                         "answer\a then\nnext line")
                        nil)
               (user-error e))))
    (let ((message (error-message-string err)))
      (ert-info ((format "Message:\n%S" message))
        (should (string-match-p "answer then\nnext line" message))
        (should-not (string-match-p "\a" message))))))

(ert-deftest scalpel-llm-dialect-test-prose-type-is-a-user-error ()
  "The prose condition stays a `user-error' and carries its own type.
Regression: the console catches a planner-output failure as
`user-error', so a type that stopped deriving from it would slip
past the console's handler and surface as a backtrace; and without
a type of its own the console could not tell a prose answer from a
malformed reply, two failures whose remedy is not the same."
  (should (memq 'user-error
                (get 'scalpel-llm-dialect-prose-reply-error
                     'error-conditions)))
  ;; The fixture is bound rather than passed as a literal: checkdoc reads
  ;; a literal argument of a call whose name ends in "error" as a message
  ;; and asks for a capital letter, while a prose reply is lowercase on
  ;; purpose -- that is the shape the parser has to recognise.
  (let ((prose "plain prose, no JSON"))
    (should-error (scalpel-llm-dialect--parse-error prose)
                  :type 'scalpel-llm-dialect-prose-reply-error)))

(provide 'scalpel-llm-dialect-test)

;;; scalpel-llm-dialect-test.el ends here
