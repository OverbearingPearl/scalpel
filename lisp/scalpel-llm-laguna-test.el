;;; scalpel-llm-laguna-test.el --- Tests for scalpel-llm-laguna -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Laguna reply dialect.  No LLM is contacted: the
;; parser is a pure function of the text it is given, and every input
;; below is a reply shape observed from the model.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'scalpel-agent)
(require 'scalpel-llm-dialect)
(require 'scalpel-llm-laguna)

(defconst scalpel-llm-laguna-test--read-call
  (concat "I'll check the file.\n"
          "<tool_call>file-read<arg_key>file</arg_key>"
          "<arg_value>/tmp/a.el</arg_value></tool_call>")
  "A reply shape as observed: prose, then one read call.
The tool name and the field name inside the call belong to Scalpel;
only the envelope around them is the dialect.")

(ert-deftest scalpel-llm-laguna-test-parse-converts-a-read-call ()
  "A read call written as text becomes the read action it describes.
Regression: the reply used to reach the refusal path, so the whole
round was thrown away although every name in it belonged to Scalpel."
  (let ((actions (scalpel-llm-laguna-parse-reply
                  scalpel-llm-laguna-test--read-call)))
    (ert-info ((format "Actions: %S" actions))
      (should (= (length actions) 1))
      (should (equal (plist-get (car actions) :tool) "file-read"))
      (should (equal (plist-get (car actions) :file) "/tmp/a.el")))))

(ert-deftest scalpel-llm-laguna-test-parse-converts-every-call-in-order ()
  "Each call in the reply becomes one action, in the order written."
  (let ((actions
         (scalpel-llm-laguna-parse-reply
          (concat "<tool_call>file-read<arg_key>file</arg_key>"
                  "<arg_value>/tmp/a.el</arg_value></tool_call>"
                  "<tool_call>reply<arg_key>text</arg_key>"
                  "<arg_value>done</arg_value></tool_call>"))))
    (ert-info ((format "Actions: %S" actions))
      (should (= (length actions) 2))
      (should (equal (plist-get (car actions) :tool) "file-read"))
      (should (equal (plist-get (cadr actions) :tool) "reply"))
      (should (equal (plist-get (cadr actions) :text) "done")))))

(ert-deftest scalpel-llm-laguna-test-value-keeps-newlines ()
  "An argument value is taken verbatim, newlines included.
A scanner that could not match across a newline would stop at the
first line of a multi-line body and hand back a truncated value
without saying so."
  (let ((actions
         (scalpel-llm-laguna-parse-reply
          (concat "<tool_call>reply<arg_key>text</arg_key>"
                  "<arg_value>first line\nsecond line</arg_value>"
                  "</tool_call>"))))
    (ert-info ((format "Actions: %S" actions))
      (should (equal (plist-get (car actions) :text)
                     "first line\nsecond line")))))

(ert-deftest scalpel-llm-laguna-test-shell-omission-is-filled-and-reported ()
  "A shell call that states no `long-running' is filled in and reported.
Every observed shell call gave command and reason and no
`long-running'; the value filled in is t so the round asks before
the command runs, and each fill is announced rather than applied
quietly."
  (let ((notices nil)
        actions)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) notices))))
      (setq actions
            (scalpel-llm-laguna-parse-reply
             (concat "<tool_call>shell<arg_key>command</arg_key>"
                     "<arg_value>grep -rn gptel .</arg_value>"
                     "<arg_key>reason</arg_key>"
                     "<arg_value>find references</arg_value>"
                     "</tool_call>"))))
    (let ((action (car actions)))
      (ert-info ((format "Action: %S Notices: %S" action notices))
        (should (equal (plist-get action :command) "grep -rn gptel ."))
        (should (equal (plist-get action :reason) "find references"))
        (should (eq (plist-get action :long-running) t))
        (should (cl-some (lambda (notice)
                           (string-match-p "long-running" notice))
                         notices))))))

(ert-deftest scalpel-llm-laguna-test-assumed-field-makes-the-round-ask-first ()
  "The value filled in for a missing field is what stops a quiet run.
`scalpel-agent--confirm-needed-p' asks for a shell action whose
`long-running' is true, so t is the value that brings the user into
the decision; false would run the command without one."
  (let ((scalpel-agent-confirm-tools '("shell"))
        (action
         (car (scalpel-llm-laguna-parse-reply
               (concat "<tool_call>shell<arg_key>command</arg_key>"
                       "<arg_value>rm -rf build</arg_value>"
                       "<arg_key>reason</arg_key>"
                       "<arg_value>clean</arg_value></tool_call>")))))
    (ert-info ((format "Action: %S" action))
      (should (eq (plist-get action :long-running) t))
      (should (scalpel-agent--confirm-needed-p action)))))

(ert-deftest scalpel-llm-laguna-test-parse-leaves-a-json-reply-whole ()
  "A JSON action array keeps its JSON reading.
The markers can also appear inside a quoted string, and a reply that
names one without completing a call must still parse as JSON."
  (let ((actions
         (scalpel-llm-laguna-parse-reply
          (concat "Sure.\n[{\"tool\":\"reply\",\"text\":\""
                  "the marker <tool_call> is not a call\"}]"))))
    (ert-info ((format "Actions: %S" actions))
      (should (= (length actions) 1))
      (should (equal (plist-get (car actions) :tool) "reply"))
      (should (string-match-p "<tool_call>"
                              (plist-get (car actions) :text))))))

(ert-deftest scalpel-llm-laguna-test-brackets-in-a-command-stay-a-call ()
  "A bracket in an argument does not divert the reply to the JSON parser.
A reading that looks for a JSON payload first would find the bracket
in the grep pattern, fail to parse it, and refuse a call that was
well formed; a complete call is therefore taken first."
  (let ((actions
         (scalpel-llm-laguna-parse-reply
          (concat "<tool_call>shell<arg_key>command</arg_key>"
                  "<arg_value>grep -E '[0-9]+' notes.txt</arg_value>"
                  "<arg_key>reason</arg_key>"
                  "<arg_value>count digits</arg_value></tool_call>"))))
    (ert-info ((format "Actions: %S" actions))
      (should (= (length actions) 1))
      (should (equal (plist-get (car actions) :command)
                     "grep -E '[0-9]+' notes.txt")))))

(ert-deftest scalpel-llm-laguna-test-parse-refuses-a-truncated-call ()
  "A reply cut off mid-call is refused whole, never partly executed.
The backend output budget can end the reply before the closing
marker; executing the calls that did arrive would carry out half of
a plan the model never finished stating, so the refusal covers the
reply rather than the call.  The reported cause names tool-call
syntax, which is what such a reply is; the truncation is not
distinguished from it."
  (let ((raw
         (concat "<tool_call>read<arg_key>file</arg_key>"
                 "<arg_value>/tmp/a.el</arg_value></tool_call>"
                 "<tool_call>shell<arg_key>command</arg_key>"
                 "<arg_value>grep -rn gptel .</arg_value>")))
    (ert-info ((format "Raw:\n%S" raw))
      (should-error (scalpel-llm-laguna-parse-reply raw)
                    :type 'scalpel-llm-dialect-tool-call-error))))

(ert-deftest scalpel-llm-laguna-test-parse-refuses-an-incomplete-argument ()
  "A call whose argument is incomplete is refused, not partly read.
Skipping the unreadable pair and keeping the rest would silently
lose a field, and the field this dialect is known to omit is the one
that decides whether a command needs an answer from the user."
  (let ((missing-value
         (concat "<tool_call>shell<arg_key>command</arg_key>"
                 "<arg_value>ls</arg_value>"
                 "<arg_key>long-running</arg_key><arg_value>true"
                 "</tool_call>"))
        (missing-key-end
         "<tool_call>read<arg_key>file</tool_call>"))
    (ert-info ((format "Raw:\n%S" missing-value))
      (should-error (scalpel-llm-laguna-parse-reply missing-value)
                    :type 'scalpel-llm-dialect-tool-call-error))
    (ert-info ((format "Raw:\n%S" missing-key-end))
      (should-error (scalpel-llm-laguna-parse-reply missing-key-end)
                    :type 'scalpel-llm-dialect-tool-call-error))))

(ert-deftest scalpel-llm-laguna-test-parse-does-not-own-the-tool-contract ()
  "The parser converts the envelope and leaves field checking alone.
The per-tool contract lives in `scalpel-agent--tool-fields', and
checking it here too would put the same rule in two places, with this
copy -- not the one dispatch uses -- the copy that drifts."
  (let ((actions
         (scalpel-llm-laguna-parse-reply
          (concat "<tool_call>nope<arg_key>odd</arg_key>"
                  "<arg_value>1</arg_value></tool_call>"))))
    (ert-info ((format "Actions: %S" actions))
      (should (equal (plist-get (car actions) :tool) "nope"))
      (should (equal (plist-get (car actions) :odd) "1")))))

(ert-deftest scalpel-llm-laguna-test-registration-matches-the-laguna-name ()
  "Backends named after the model use this parser, and others do not.
Dispatch keys off the gptel backend name and the model that backend
declares, so the registration is what makes the dialect reachable,
and a backend this module was not written for still gets the loud
refusal: the backend below declares no model, so no model name can be
read for it.  The
backend is a real one, built by `gptel-make-openai' and put back
afterwards: `gptel-backend-name' is a structure accessor that
type-checks its argument inside its own body, so a stand-in symbol is
rejected before dispatch is ever reached."
  (let ((saved gptel-backend))
    (unwind-protect
        (progn
          (setq gptel-backend (gptel-make-openai "Laguna" :key "test-key"))
          (let ((actions (scalpel-llm-dialect-parse
                          scalpel-llm-laguna-test--read-call)))
            (ert-info ((format "Actions: %S" actions))
              (should (= (length actions) 1))
              (should (equal (plist-get (car actions) :tool) "file-read"))))
          (setq gptel-backend
                (gptel-make-openai "Somewhere-Else" :key "test-key"))
          (should-error (scalpel-llm-dialect-parse
                         scalpel-llm-laguna-test--read-call)
                        :type 'scalpel-llm-dialect-tool-call-error))
      (setq gptel-backend saved)
      (scalpel-utils-test-delete-backend "Laguna")
      (scalpel-utils-test-delete-backend "Somewhere-Else"))))

(ert-deftest scalpel-llm-laguna-test-plan-accepts-a-text-call ()
  "A text call reaches dispatch as a planned action, not a refusal.
Regression: before the dialect was converted, the same reply made the
planner report a tool-call failure and the round ran nothing."
  (let ((saved gptel-backend)
        (scalpel-agent--context-files nil))
    (unwind-protect
        (progn
          (setq gptel-backend (gptel-make-openai "Laguna" :key "test-key"))
          (cl-letf (((symbol-function 'scalpel-llm-request-async)
                     (lambda (_prompt on-success _on-error &optional _system)
                       (funcall on-success scalpel-llm-laguna-test--read-call))))
            (let (actions error)
              (scalpel-agent-plan
               "read the file" nil
               (lambda (result) (setq actions result))
               (lambda (err) (setq error err)))
              (ert-info ((format "Actions: %S Error: %S" actions error))
                (should-not error)
                (should (= (length actions) 1))
                (should (equal (plist-get (car actions) :tool) "file-read"))
                (should (equal (plist-get (car actions) :file)
                               "/tmp/a.el"))))))
      (setq gptel-backend saved)
      (scalpel-utils-test-delete-backend "Laguna"))))

(provide 'scalpel-llm-laguna-test)

;;; scalpel-llm-laguna-test.el ends here
