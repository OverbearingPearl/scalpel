;;; scalpel-prompt-test.el --- Tests for scalpel-prompt -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the prompt text moved into `scalpel-prompt'.  Only the
;; tests that read the prompt text itself live here; tests that drive
;; the agent's planning or editing stay in `scalpel-agent-test'.

;;; Code:

(require 'ert)
(require 'scalpel-prompt)
(require 'scalpel-llm-dialect)

(ert-deftest scalpel-prompt-test-system-prompt-hides-the-sandbox ()
  "The planner must not be told that commands run under a sandbox.
Regression: the prompt named the OS sandbox, so the planner could
reason about the boundary and probe or route around it; the user
experience is meant to be an ordinary shell with a smaller
filesystem, not a sandboxed one."
  (dolist (word '("sandbox" "bwrap" "bubblewrap" "sandbox-exec"))
    (ert-info ((format "Prompt mentions %S" word))
      (should-not (string-match-p (regexp-quote word)
                                  (downcase scalpel-agent-system-prompt))))))

(ert-deftest scalpel-prompt-test-system-prompt-bounds-reply-text ()
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

(ert-deftest scalpel-prompt-test-prompt-example-parses ()
  "The example the system prompt shows is one the parser accepts.
Regression: the prompt presented prose before the array as an
outright failure, while `scalpel-llm-dialect--default-parse' digs
the array out of surrounding prose and a test asserts it does, so
the prompt described a system other than this one."
  (let ((parsed (scalpel-llm-dialect--default-parse
                 scalpel-agent--prompt-example)))
    (ert-info ((format "Example: %S" scalpel-agent--prompt-example))
      (should (equal (plist-get (car parsed) :tool) "reply")))))

(ert-deftest scalpel-prompt-test-system-prompt-denies-tool-calling ()
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

(ert-deftest scalpel-prompt-test-system-prompt-states-the-pattern-dialect ()
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

(ert-deftest scalpel-prompt-test-system-prompt-takes-names-literally ()
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

(ert-deftest scalpel-prompt-test-system-prompt-prefers-perl ()
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

(provide 'scalpel-prompt-test)
;;; scalpel-prompt-test.el ends here
