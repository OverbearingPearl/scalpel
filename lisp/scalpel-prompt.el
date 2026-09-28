;;; scalpel-prompt.el --- Prompt text and assembly for the Scalpel agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; The prompt text sent to the planner LLM, and nothing else: the
;; constants and the assembly of the system prompt.  No dispatch, no
;; context, no execution logic lives here.

;;; Code:

(defcustom scalpel-prompt-cod-prompt
  "You are a coding agent operating inside Emacs via the Scalpel package.

Respond with concise, actionable answers focused on the user's
Elisp/programming request. When modifying code, provide complete,
self-contained forms rather than fragments. Prefer standard Emacs
Lisp idioms, respect lexical binding, and avoid deprecated APIs.
Do not explain unrelated topics; stay on task."
  "System prompt sent to the Codex backend agent.

This variable is referenced by `scalpel-agent.el' (line 773) and
must be defined before `scalpel-prompt-system-prompt' so that the
reference is resolvable at load time."
  :type 'string
  :group 'scalpel)

(require 'scalpel-prompt-rule)

(defcustom scalpel-prompt-reply-language nil
  "Language the planner's reply actions are written in, or nil.
Nil lets the model choose freely; a non-nil value is stated to the
model as a constraint on reply text only, leaving reasoning,
planning and code untouched."
  :type '(choice (string :tag "Language")
                 (const :tag "Let the model choose" nil))
  :group 'scalpel)

(defconst scalpel-prompt--reply-language-rule-header
  "Write the \"text\" of every reply action in %s.  This bounds the
user-facing reply only: reasoning, planning, code, comments and
prompt wording stay in the language best suited to them.\n")

(defun scalpel-prompt--reply-language-rule ()
  "Return the natural-language reply-language rule for the system prompt.
The result is appended by `scalpel-prompt-system-prompt'.  Returns
nil when `scalpel-prompt-reply-language' is unset, so the model
chooses freely; otherwise returns a paragraph stating the chosen
language as a constraint on reply text only.  The rule text is
formatted through the constant
`scalpel-prompt-rule--reply-language-format' from
scalpel-prompt-rule.el."
  (when scalpel-prompt-reply-language
    (format scalpel-prompt-rule--reply-language-format
            scalpel-prompt-reply-language)))

(defconst scalpel-prompt--decide-for-me
  "The choice is yours: if you've reached a conclusion and judge
the fix safe, implement it now; if a choice remains open, pick
the best option and act."
  "Fixed prompt sent by `scalpel-console-send-decide-for-me' (C-c C-y).
Structural contract shared by that command and
`scalpel-console-mode-map', which binds it.")

(defconst scalpel-prompt--replan
  "Your previous approach is rejected.  If you already applied
changes, revert them cleanly first.  Abandon that line of
thinking entirely, rethink from scratch, and give me a new plan
(or carry it out directly if action was expected)."
  "Fixed prompt sent by `scalpel-console-send-replan' (C-c C-n).
Covers two cases: the previous round may have been a mere
proposal, or it may already have been implemented -- in the latter
case the model must first revert the changes.  Structural contract
shared by that command and `scalpel-console-mode-map', which binds
it.")

(defconst scalpel-prompt--why
  "Explain why this happened and the root cause.  Strictly read-only:
do not modify, create, or delete anything."
  "Fixed prompt sent by `scalpel-console-send-why' (C-c C-w).
Structural contract shared by that command and
`scalpel-console-mode-map', which binds it.")

(defconst scalpel-prompt--summarize
  "Too verbose -- give me a brief summary of the essence.
Strictly read-only -- do not modify, create or delete anything."
  "Fixed prompt sent by `scalpel-console-send-summarize' (C-c C-s).
Structural contract shared by that command and
`scalpel-console-mode-map', which binds it.")

(defconst scalpel-prompt--retry
  "The last change failed.  Do not change direction and do not replan:
keep the original design.  Analyze more deeply why the last change
failed, then continue with a more careful new attempt along the same
line."
  "Prompt instructing the agent to retry a failed change.
The last change failed, and the agent must not change direction or
replan; it should keep the original design, analyze more deeply why
the last change failed, and continue with a more careful new attempt
along the same line.

Sent by the retry command bound in `scalpel-console-mode-map', via
`scalpel-console-send-retry'.  Unlike `scalpel-prompt--replan', which
abandons the current approach and reverts it, this prompt keeps the
original design: the agent must stay on the same line and make a more
careful attempt.  Like `scalpel-prompt--why' and
`scalpel-prompt--summarize', the response must not include any
unrelated modification beyond the new attempt.")

(defconst scalpel-prompt--resume
  "The previous round was cut off by an interruption outside
your control -- an unstable network, a server error, or the
like.  Your last action may never have run, and its outcome is
unknown.  Read the conversation above, judge from it how far
the work actually got, and continue from there rather than
starting over.  Issue at most one concrete next action."
  "Prompt sent in the interrupted-connection case.
It covers a previous round that was cut off before its outcome
was known, unlike `scalpel-prompt--retry', which covers a change
that ran and failed.  Sent by the resume command in the console.")

(defconst scalpel-prompt--continuation-instruction
  "The action from the previous round already ran; its output is in the
conversation above.  Read that output and decide now: if it already
answers the user's request, reply with the conclusion; otherwise issue
at most one concrete next action.  Do not repeat that action."
  "Instruction sent on a continued round, after a report was produced.
The planner is asked to continue.

The user's original instruction is already in the conversation at
that point, so re-sending it would only make the planner re-issue the
same action.  This wording instead points the planner at the previous
round's output and asks it to either conclude or issue at most one
next action.

It deliberately names no action kind: a round that only read a
definition is continued the same way as one that ran a shell command,
and `scalpel-console--run-rounds' tests :reads alongside :shells using
this same text.

This is a structural contract shared with `scalpel-console--run-rounds';
changing the wording here requires checking that caller.")

(defvar scalpel-prompt-programming-language-rules nil
  "Store language-specific prompt rules as filename regexp entries.

Each entry maps a filename regexp to a prompt string.  Later registration
replaces an entry having the same regexp.")

(defun scalpel-prompt-register-programming-language-rule (filename-regexp rule)
  "Register RULE as the prompt rule for FILENAME-REGEXP.
FILENAME-REGEXP is a regular expression matched against a file name.
RULE is the language-specific prompt rule used for matching files."
  (setq scalpel-prompt-programming-language-rules
        (cons (cons filename-regexp rule)
              (assoc-delete-all filename-regexp
                                scalpel-prompt-programming-language-rules))))

(defun scalpel-prompt-programming-language-rule-for-file (file)
  "Return the rule string registered for FILE's language, or nil.
The registry contains (FILENAME-REGEXP . RULE) pairs.  A rule
matches when FILE, the full absolute name, matches the entry's
regexp."
  (assoc-default file scalpel-prompt-programming-language-rules
                 (lambda (regexp key)
                   (string-match-p regexp key))))

(defconst scalpel-prompt--block-edit-prompt
  (concat "Signature: %s\n\nCurrent block:\n%s\n\n"
          "Instruction: %s\n\n"
          "Return only the full replacement block, written in "
          "the same language as the block above, as plain "
          "text. Do not include markdown fences or "
          "explanations. The instruction describes only what "
          "should change and may be ignored wherever it "
          "contains code, diffs, or a restatement of the "
          "block, since the replacement is carried by the "
          "reply itself. If the requested change is impossible or "
          "unnecessary for this block, return exactly: NO_CHANGE")
  "Replacement-round prompt sent by `scalpel-agent-block-edit'.
It is formatted with the block's signature line, current body
and the edit instruction.  The trailing NO_CHANGE literal is the
structural contract shared with `scalpel-agent--no-change-sentinel',
which the reply is compared against.")

(defconst scalpel-prompt--block-insert-prompt
  (concat "Anchor signature: %s\n\n"
          "Instruction: %s\n\n"
          "Return only the full new definition to insert "
          "immediately after the anchor, written in the "
          "same language as the anchor, as plain text. Do "
          "not include markdown fences or explanations. "
          "The instruction describes only what to create; "
          "ignore any code, diffs, or restatement of the "
          "definition it may contain, since the new text "
          "is carried by this reply itself. "
          "If nothing should be created, return exactly: "
          "NO_CHANGE")
  "Creation-round prompt sent by `scalpel-agent-block-insert'.
Formatted with two arguments: the anchor's signature line and
the insertion instruction.  The trailing NO_CHANGE literal is
the structural contract shared with
`scalpel-agent--no-change-sentinel': an LLM response consisting
exactly of that token signals that nothing should be created.")

(defcustom scalpel-prompt-system-prompt
  (concat
   scalpel-prompt-rule--document "\n"
   scalpel-prompt-rule--example "\n"
   scalpel-prompt-rule--actions "\n"
   scalpel-prompt-rule--string-syntax "\n"
   scalpel-prompt-rule--format "\n"
   scalpel-prompt-rule--symbol-name "\n"
   scalpel-prompt-rule--shell "\n"
   scalpel-prompt-rule--reading "\n"
   scalpel-prompt-rule--feedback-fence "\n"
   scalpel-prompt-rule--shell-hygiene "\n"
   scalpel-prompt-rule--file-actions "\n"
   scalpel-prompt-rule--substitute "\n"
   scalpel-prompt-rule--substitute-pattern "\n"
   scalpel-prompt-rule--redaction "\n"
   scalpel-prompt-rule--scope "\n"
   scalpel-prompt-rule--whole-block "\n"
   scalpel-prompt-rule--instruction "\n"
   scalpel-prompt-rule--reply-brevity "\n"
   scalpel-prompt-rule--no-tail "\n"
   scalpel-prompt-rule--action-budget
   (if scalpel-prompt-reply-language
       (concat "\n" (scalpel-prompt--reply-language-rule))
     ""))
  "System prompt for the Scalpel agent planner.
This controls only the wording sent to the LLM; the action schema
is fixed by `scalpel-agent--tool-fields' and
`scalpel-agent--tool-vocabulary' and must not be overridden here.
Whether a reply language is imposed is controlled by
`scalpel-prompt-reply-language'; nil there leaves the prompt
unchanged."
  :type 'string
  :group 'scalpel)

(provide 'scalpel-prompt)

;;; scalpel-prompt.el ends here
