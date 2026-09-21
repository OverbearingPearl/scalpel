;;; scalpel-prompt-commit.el --- Prompt text for commit messages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Commit-message prompt rules, kept here so every prompt lives in a
;; scalpel-prompt* module.  Pure text builders: no state, no buffers.

;;; Code:

(defun scalpel-prompt-commit-style-prompt (style)
  "Return the style rules for STYLE, one of `scalpel-commit--styles'."
  (pcase style
    ('angular
     (concat "Style: conventional (angular) commits.  Subject line is "
             "type(scope): imperative summary -- types include feat, "
             "fix, docs, refactor, test, chore.  No trailing period.  "
             "Body paragraphs, each wrapped to 72 columns, separated "
             "by blank lines.  Footer lines only when a breaking change "
             "or an issue needs naming."))
    ('linux
     (concat "Style: Linux kernel commits.  Subject line is a plain "
             "imperative summary, no type prefix, no trailing period.  "
             "Blank line, then prose paragraphs wrapped to 72 columns "
             "explaining WHY the change is right, not just what it "
             "does.  No type tags, no scope, no bullet lists unless the "
             "change itself enumerates things."))
    (_ "")))

(defun scalpel-prompt-commit-build-prompt (diff style language extra
                                                 subject-max body-width)
  "Build the commit message request from DIFF, STYLE, LANGUAGE and EXTRA.
EXTRA carries the regeneration instruction, such as a request for
more detail or a style or language switch, so the model changes its
answer instead of repeating it.
SUBJECT-MAX and BODY-WIDTH are the column limits the rules state."
  (concat
   "Write a git commit message for the diff below.\n\n"
   "House rules, always in force:\n"
   "- The subject is an imperative sentence: it must complete \"this "
   "commit will\".\n"
   "- No more than " (number-to-string subject-max)
   " characters, no period, capitalized where the language does so.\n"
   "- Body wrapped to " (number-to-string body-width)
   " columns, blank line between paragraphs.\n"
   "- Describe what and why; name real functions and files from the "
   "diff.  Never invent a name the diff does not show.\n"
   "- Reply with ONLY the commit message: subject, blank line, body. "
   "No fences, no commentary.\n\n"
   (scalpel-prompt-commit-style-prompt style)
   "\nWrite the message in " language ".\n"
   (when extra
     (concat "The previous attempt was rejected because: " extra
             ".  Produce a different message.\n"))
   "\nDIFF:\n" diff))

(provide 'scalpel-prompt-commit)

;;; scalpel-prompt-commit.el ends here
