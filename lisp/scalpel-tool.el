;;; scalpel-tool.el --- Command-line tool selection for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; One category of shell behaviour maps to a preference table of
;; command-line tools.  Each entry is a plist:
;;
;;   :argv        the argv prefix the executor builds commands from.
;;   :prompt-rule symbol naming a defconst with the prompt wording
;;                injected when that tool is the selected one.
;;
;; A console selects exactly one tool per category, once, via
;; `scalpel-tool-probe'.  The probe walks the candidates in order and
;; takes the first installed tool; when none is installed the first
;; candidate is taken, so the executor keeps a fixed contract either
;; way.  The prompt never mentions candidates, priority or fallback:
;; for the planner each category names exactly one tool.  Platform
;; differences (macOS BSD vs GNU spellings) are resolved by
;; `scalpel-tool--argv' at read time.

;;; Code:

(require 'cl-lib)

(defcustom scalpel-tool-preferences
  '((substitute
     (perl
      :argv ("perl")
      :prompt-rule scalpel-tool--perl-substitute-rule)
     (sed
      :argv ("sed" "-i")
      :prompt-rule scalpel-tool--sed-substitute-rule)
     (awk
      :argv ("awk")))
    (search
     (rg
      :argv ("rg")
      :prompt-rule scalpel-tool--rg-search-rule)
     (grep
      :argv ("grep")
      :prompt-rule scalpel-tool--grep-search-rule)))
  "Preference table from behaviour category to candidate tools.
Order inside a category is the priority order; the first tool whose
executable is found wins.  Each entry is a plist with keys :argv
\(list of strings, the executor's command prefix) and :prompt-rule
\(symbol naming a defconst holding the prompt wording, optional).
No other reader enumerates the keys; a new category is just a new
key.  The wording of every :prompt-rule describes only the selected
tool; candidates and priorities never reach the prompt."
  :type 'sexp
  :group 'scalpel)

(defconst scalpel-tool--perl-substitute-rule
  "The target engine for a file-substitute pattern is Perl 5.x:
write a Perl-compatible regular expression and do not use
Emacs-only constructs.  Before writing the pattern, compile-test
it in Perl itself with qr// or m//; if compilation fails, take
the concrete error message Perl prints and fix the pattern next
round rather than guessing.
Write every literal as the exact characters to match.  The dot
metacharacter excludes newlines by default, so handle newlines
explicitly with a whitespace class.  There is no non-greedy
matching guarantee across engines: constrain matches with negated
character classes, anchors, or backtracking constraints instead
of lazy quantifiers.  The replacement is exact replacement text
where Perl's dollar-one style numbering is what the engine uses,
and the ampersand form means the whole match.
The common constructs are literals, character classes,
non-capturing groups, capture groups, alternation, repetition,
anchors, and word, whitespace and digit classes.
A pattern is a plain regexp string, nothing else: output only the
regexp text itself, with no quotes, no delimiters, no flags, no
qr// or m// wrapper, and no prose."
  "Prompt wording for perl as the substitute tool.")

(defconst scalpel-tool--sed-substitute-rule
  "The target engine for a file-substitute is sed.  Write a
POSIX regular expression; the platform decides the dialect, macOS
ships BSD sed and GNU/Linux ships GNU sed, so write to the common
subset and prefer character classes like [[:alnum:]] over \\w,
which many seds do not accept.  Avoid non-greedy and lookahead
constructions entirely: sed has none.  State replacements as
literal text; \\1 refers to the first capture group in the
replacement.  The pattern is a plain regexp string, no quotes, no
delimiters, no prose."
  "Prompt wording for sed as the substitute tool.")

(defconst scalpel-tool--awk-substitute-rule
  "The target engine for a file-substitute is awk.  Write a
POSIX extended regular expression as gawk and BSD awk both accept
it: character classes, groups, repetition and anchors; no
lookaround and no non-greedy matching.  State replacements as
literal text inside sub() or gsub(); \\\\1 in the replacement refers
to the first capture group.  The pattern is a plain regexp
string, no quotes, no delimiters, no prose."
  "Prompt wording for awk as the substitute tool.")

(defconst scalpel-tool--rg-search-rule
  "Search with ripgrep.  Patterns are Rust regex syntax:
character classes, non-capturing groups, repetition and anchors
all work; lookaround does not.  Prefer -l to list matching files
and -m to bound per-file match counts.  A pattern is a plain
regexp string, no quotes, no delimiters, no prose."
  "Prompt wording for rg as the search tool.")

(defconst scalpel-tool--grep-search-rule
  "Search with grep.  Use POSIX regular expressions: character
classes, groups, repetition and anchors; prefer [[:alnum:]] over
\\w.  On macOS grep is BSD grep, so prefer -E over -P and never
rely on GNU-only flags.  Pass -m to bound per-file match counts.
A pattern is a plain regexp string, no quotes, no delimiters, no
prose."
  "Prompt wording for grep as the search tool.")

(defvar-local scalpel-tool--selected nil
  "Buffer-local plist of the tools chosen for this console.
Keys are behaviour categories, values are tool name symbols, for
example (:substitute . perl) as an alist or a plist (:substitute
perl :search rg).  Set once by `scalpel-tool-probe' when the
console is created; nil until probed.")

(defvar scalpel-tool--os nil
  "Cached operating system tag for the probe.
One of the symbols `darwin' or `linux', or nil before the first
probe.")

(defun scalpel-tool--os-tag ()
  "Return the cached OS tag targeted by the probe."
  (or scalpel-tool--os
      (setq scalpel-tool--os
            (if (memq system-type '(darwin macos))
                'darwin
              'linux))))

(defun scalpel-tool--candidates (category)
  "Return the candidate list for CATEGORY, or nil."
  (cdr (assq category scalpel-tool-preferences)))

(defun scalpel-tool--candidate-plist (category tool)
  "Return the candidate plist for TOOL under CATEGORY, or nil if absent."
  (let ((entry (cl-assoc tool (scalpel-tool--candidates category))))
    (and entry (cdr entry))))

(defun scalpel-tool--arg-for-os (tool arg)
  "Return the ARG spelling that fits the platform for TOOL.
Today only sed differs per platform: BSD sed (macOS) takes a bare
-i, GNU sed wants -i with an explicit empty suffix.  Anything else
passes through unchanged."
  (if (and (eq tool 'sed) (string= arg "-i"))
      (if (eq (scalpel-tool--os-tag) 'darwin)
          "-i"
        "-i''")
    arg))

(defun scalpel-tool--argv (category)
  "Return the selected CATEGORY tool's argv prefix, resolved per OS.
Each flag is passed through `scalpel-tool--arg-for-os'.  Nil when
the category has no selection."
  (let* ((tool (plist-get scalpel-tool--selected category))
         (entry (scalpel-tool--candidate-plist category tool)))
    (and entry
         (mapcar (lambda (arg) (scalpel-tool--arg-for-os tool arg))
                 (plist-get entry :argv)))))

(defun scalpel-tool--prompt-rule (category)
  "Return the selected CATEGORY tool's prompt wording, or nil."
  (let* ((tool (plist-get scalpel-tool--selected category))
         (rule (and tool
                    (plist-get (scalpel-tool--candidate-plist
                                category tool)
                               :prompt-rule))))
    (and rule (symbol-value rule))))

(defun scalpel-tool--prompt-rules ()
  "Return the concatenated prompt wording of the selected tools.
Rules appear in table order, one per selected tool that carries a
:prompt-rule, separated by blank lines.  A tool with no rule
contributes nothing."
  (let ((rules))
    (dolist (category scalpel-tool-preferences)
      (let ((rule (scalpel-tool--prompt-rule (car category))))
        (when rule
          (push rule rules))))
    (mapconcat #'identity (nreverse rules) "\n\n")))

(defun scalpel-tool-probe ()
  "Select one tool per category and store it in this buffer.
Walk `scalpel-tool-preferences' in order; the first candidate
whose executable is found wins, and when nothing is installed the
first candidate is taken so the executor keeps a fixed contract.
Return the selected plist."
  (setq scalpel-tool--selected
        (let ((selected))
          (dolist (category scalpel-tool-preferences selected)
            (let* ((candidates (cdr category))
                   (winner
                    (or (cl-find-if
                         (lambda (candidate)
                           (executable-find (symbol-name (car candidate))))
                         candidates)
                        (car candidates))))
              (setq selected
                    (plist-put selected (car category) (car winner))))))))

(provide 'scalpel-tool)

;;; scalpel-tool.el ends here
