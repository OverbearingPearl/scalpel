;;; scalpel-tool.el --- Command-line tool selection for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; One behaviour category maps to exactly one command-line tool.
;; Each entry is a plist:
;;
;;   :tool        the tool's name symbol.
;;   :argv        the argv prefix the executor builds commands from.
;;   :prompt-rule symbol naming a defconst with the prompt wording,
;;                injected only when that tool's executable is
;;                present; otherwise the planner picks its own tool.
;;
;; There is no candidate list, no priority order and no probe: the
;; table names one tool per category, and `scalpel-tool--argv'
;; reads the entry directly.

;;; Code:

(require 'cl-lib)

(defconst scalpel-tool--perl-substitute-rule
  "Substitute patterns are compiled by Perl 5, which is installed
locally, so write them as Perl-compatible regexps rather than
Emacs-only constructs.  When a pattern's semantics are uncertain,
query the perlre manual through filtered perldoc.")

(defconst scalpel-tool--rg-search-rule
  "Search with ripgrep.  Patterns are Rust regex syntax:
character classes, non-capturing groups, repetition and anchors
all work; lookaround does not.  Prefer -l to list matching files
and -m to bound per-file match counts.  A pattern is a plain
regexp string, no quotes, no delimiters, no prose."
  "Prompt wording for rg as the search tool.")

(defvar scalpel-tool--preferences nil
  "Per-category tool preferences.")

(defun scalpel-tool--entry (category)
  "Return CATEGORY's plist entry from `scalpel-tool--preferences'."
  (plist-get scalpel-tool--preferences category))

(defun scalpel-tool--argv (category)
  "Return the CATEGORY tool's argv prefix straight from its table entry.
No selection state is consulted and no per-OS dispatch is applied.
Nil when the category has no table entry."
  (plist-get (scalpel-tool--entry category) :argv))

(defun scalpel-tool--prompt-rule (category)
  "Return the prompt wording for CATEGORY's preferred tool, or nil.
Look up CATEGORY in `scalpel-tool--preferences'; return nil when there
is no entry, the entry has no :prompt-rule, or the entry's :argv
executable is not installed, leaving the choice to the planner."
  (let* ((entry (plist-get scalpel-tool--preferences category))
         (argv (and entry (plist-get entry :argv)))
         (exec (and argv (car argv)))
         (rule (and exec
                    (executable-find exec)
                    (plist-get entry :prompt-rule))))
    (and rule (symbol-value rule))))

(defun scalpel-tool--prompt-rules ()
  "Return the concatenated prompt wording of the selected tools.
Rules appear in table order, one per selected tool that carries a
:prompt-rule, separated by blank lines.  A tool with no rule
contributes nothing."
  (let ((rules))
    (dolist (category scalpel-tool--preferences)
      (let ((rule (scalpel-tool--prompt-rule (car category))))
        (when rule
          (push rule rules))))
    (mapconcat #'identity (nreverse rules) "\n\n")))

(provide 'scalpel-tool)

;;; scalpel-tool.el ends here
