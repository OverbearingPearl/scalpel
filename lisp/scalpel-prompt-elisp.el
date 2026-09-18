;;; scalpel-prompt-elisp.el --- Emacs Lisp prompt rules for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Language-specific formatting guidance for generated Emacs Lisp.

;;; Code:

(require 'scalpel-prompt)

(defconst scalpel-prompt-elisp--format-rule
  (concat
   "For Emacs Lisp, strictly adhere to the regexp dialect guidelines:\n"
   "avoid the parenthesis escaping trap by explicitly distinguishing between\n"
   "grouping constructs and literal parens, always escaping the latter;\n"
   "handle newlines explicitly, noting that the dot metacharacter excludes\n"
   "them by default; and account for the complete lack of non-greedy\n"
   "matching by relying on negated character classes, anchoring, or\n"
   "backtracking constraints to prevent unintended match expansion.")
  "Formatting guidance for generated Emacs Lisp text.")

(scalpel-prompt-register-prompt-language-rule
 "\\.el\\'"
 scalpel-prompt-elisp--format-rule)

(provide 'scalpel-prompt-elisp)

;;; scalpel-prompt-elisp.el ends here
