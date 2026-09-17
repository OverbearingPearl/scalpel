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
  "For Emacs Lisp, split long prose strings into a `concat' form whose
segments stay short.  Format docstrings according to Emacs conventions:
use a complete summary sentence on the first line, then a blank line
before additional paragraphs, and keep two spaces between sentences."
  "Formatting guidance for generated Emacs Lisp text.")

(scalpel-prompt-register-prompt-language-rule
 "\\.el\\'"
 scalpel-prompt-elisp--format-rule)

(provide 'scalpel-prompt-elisp)

;;; scalpel-prompt-elisp.el ends here
