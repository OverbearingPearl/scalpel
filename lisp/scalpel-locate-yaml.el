;;; scalpel-locate-yaml.el --- YAML locator provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Structural locator for YAML buffers.  A "definition" is one top-level
;; key's block: the key line plus everything up to the next line that is
;; neither indented nor blank.  The symbol is the key text.

;;; Code:

(require 'cl-lib)

(defconst scalpel-locate-yaml--key-regexp
  "^\\([^ \t\n#-][^:\n]*?\\):[ \t]*\\($\\|[^\n]*\\)"
  "Regexp matching a top-level key line, capturing the key text.
Both the key's first character and its body exclude newline, so a
match can never start on a blank line and swallow the following
line's key into the capture.  A `-` is excluded as the key's first
character so that a list item line (e.g. `- name:` in a workflow
file) is not treated as a top-level key, and a `#` is excluded so
that comment lines are not treated as top-level keys.")

(defun scalpel-locate-yaml--key-opens-block-scalar-p ()
  "Return non-nil when the key line just matched opens a block scalar.
The value after the colon is `|' or `>' optionally followed by
chomping/indentation indicators (`-', `+', digits) and nothing else.
Reads the match data left by the caller's search against
`scalpel-locate-yaml--key-regexp'.

A top-level key whose value is a block scalar makes every following
line scalar text, not structure; without this check, shell scripts and
markup embedded in a workflow's run block surfaced as symbols of their
own."
  (save-excursion
    (goto-char (match-beginning 0))
    (looking-at-p ".*:[ \t]*[|>][-+0-9]*[ \t]*$")))

(defun scalpel-locate-yaml--top-definition-range (symbol)
  "Return (BEG . END) of top-level key SYMBOL's block.
The block runs from the key line to just before the next top-level
key or other non-indented line, or to `point-max'.  Trailing blank
lines are excluded.  Return nil when SYMBOL is absent."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward scalpel-locate-yaml--key-regexp nil t))
        (when (string= (match-string-no-properties 1) symbol)
          (setq found t)))
      (when found
        (let ((beg (match-beginning 0)))
          ;; The block ends at the next line that starts without
          ;; indentation: a top-level key or a document marker.
          (forward-line 1)
          (while (and (< (point) (point-max))
                      (or (looking-at-p "[ \t]")
                          (looking-at-p "[ \t]*$")))
            (forward-line 1))
          ;; Do not hand back blank lines that belong to the gap
          ;; before the next block.
          (goto-char (point))
          (skip-chars-backward " \t\n\r")
          (if (< (point) beg)
              (cons beg (1+ beg))
            (cons beg (point))))))))

(defun scalpel-locate-yaml-range (_file symbol)
  "Return the range of SYMBOL's block as (BEG . END).
Current buffer is the YAML file referenced by FILE."
  (scalpel-locate-yaml--top-definition-range symbol))

(defun scalpel-locate-yaml--single-definition-p (text)
  "Return non-nil when TEXT is exactly one top-level key block.
TEXT must start with a top-level key and hold no later top-level
key, so a replacement can only ever land on one block."
  (let ((lines (split-string text "\n")))
    (when (and lines (string-match-p scalpel-locate-yaml--key-regexp
                                     (car lines)))
      (not (cl-some (lambda (line)
                      (string-match-p scalpel-locate-yaml--key-regexp line))
                    (cdr lines))))))

(defun scalpel-locate-yaml-list-symbols (_file)
  "Return the list of top-level key texts in the current buffer.
When a matched key line opens a block scalar, the key is collected and the
scan jumps past the scalar body: a block scalar's content is scalar text,
not structure, so its lines must never be matched as keys.  List-symbols
must agree with `scalpel-locate-yaml--top-definition-range' about where a
key's block ends."
  (let (keys)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward scalpel-locate-yaml--key-regexp nil t)
        (push (match-string-no-properties 1) keys)
        (when (scalpel-locate-yaml--key-opens-block-scalar-p)
          (let ((indent (current-indentation)))
            (forward-line 1)
            (while (and (not (eobp))
                        (or (looking-at-p "[ \t]*$")
                            (> (current-indentation) indent)))
              (forward-line 1))))))
    (nreverse keys)))

(defun scalpel-locate-yaml-flow-brackets-balanced-p (file)
  "Check that flow brackets in YAML text of FILE are balanced.
Scans line by line, skipping comment lines and block scalar
bodies, ignoring bracket characters inside quotes.  Tracks
nesting depth of square and curly brackets; depth must never go
negative and must return to zero at end of buffer.  Quotes are
handled as simple single/double toggles; `#' starts a comment
only at indentation position or after whitespace."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((depth 0)
              (in-single nil)
              (in-double nil)
              (block-indent nil)
              (bad nil))
          (while (and (not bad) (not (eobp)))
            (let* ((line-start (line-beginning-position))
                   (line-end (line-end-position))
                   (text (buffer-substring-no-properties line-start line-end)))
              (let ((indent (if (string-match "\\`[ \t]*" text)
                                (match-end 0) 0)))
                (cond
                 ;; Inside a block scalar body: skip non-indented-terminating lines.
                 (block-indent
                  (if (and (> indent block-indent)
                           (not (string-blank-p text)))
                      nil ; still inside block scalar body, skip.
                    (setq block-indent nil)))
                 ;; Empty or comment line: skip entirely.
                 ((string-blank-p text) nil)
                 ((string-match "\\`[ \t]*\\(#[^']*\\)?\\'" text)
                  (when (string-match "#" text)
                    (let ((hash-pos (string-match "#" text))
                          (prev (when (> (string-match "#" text) 0)
                                  (aref text (1- (string-match "#" text))))))
                      (ignore (not (= hash-pos indent))
                              (not (memq prev '(?  ?\t)))))))
                 (t
                  ;; Detect block scalar indicators on the line before scanning.
                  (when (string-match "\\([|>][-+]?\\([0-9]*\\)\\)\\([ \t]*#.*\\)?\\'" text)
                    (setq block-indent indent))
                  ;; Scan characters.
                  (let ((i 0) (n (length text)) (first-nonspace t))
                    (while (and (not bad) (< i n))
                      (let ((ch (aref text i)))
                        (cond
                         (in-single
                          (setq in-single nil))
                         (in-double
                          (setq in-double nil))
                         ((eq ch ?#)
                          (when (or first-nonspace
                                    (and (> i 0)
                                         (memq (aref text (1- i)) '(?  ?\t))))
                            (setq i (1- n)))) ; skip rest of line as comment
                         ((eq ch ?\') (setq in-single t))
                         ((eq ch ?\") (setq in-double t))
                         ((eq ch ?\[) (setq depth (1+ depth)))
                         ((eq ch ?\])
                          (setq depth (1- depth))
                          (when (< depth 0)
                            (setq bad (point))))
                         ((eq ch ?\{) (setq depth (1+ depth)))
                         ((eq ch ?\})
                          (setq depth (1- depth))
                          (when (< depth 0)
                            (setq bad (point))))))
                      (setq i (1+ i))
                      (when (and (> i 0) (< i n)
                                 (not (memq (aref text i) '(?  ?\t))))
                        (setq first-nonspace nil)))))))
              (when in-single (setq in-single nil))
              (when in-double (setq in-double nil))
              (forward-line 1)))
          (and (not bad) (= depth 0)))))))

(defun scalpel-locate-yaml--balanced-p (text)
  "Structural validation for text produced by a file-substitute rewrite.
YAML is a bracket-pairing-free language, so indentation and flow
brackets are only an approximation of a :balanced-p answer; this
function returns t if TEXT's YAML structure is acceptable, nil
otherwise.  Pure Emacs Lisp scanning, no external YAML parser.

Checks:
1. No line's indentation may contain a Tab character.
2. Skipping full-line comments (lines whose content, after
   stripping indentation, starts with `#') and block scalar bodies
   (lines following a key line ending with `|' or `>' whose
   indentation is deeper than that key line), strip quoted strings
   (skip paired single and double quotes) from the remaining lines
   and scan flow collection brackets [ ] { } with a stack.  Any
   mismatched closing bracket (empty stack or wrong top type) or a
   non-empty stack at the end returns nil.  Return t otherwise."
  (let ((lines (split-string text "\n" nil))
        (block-scalar-indent nil)   ; indent of the key line opening a block scalar
        (stack '())
        (ok t))
    (catch 'bad
      (dolist (line lines)
        (when (> (length line) 0)
          (let ((idx 0)
                (len (length line)))
            ;; Measure indentation; reject Tab in indentation.
            (while (and (< idx len)
                        (eq (aref line idx) ?\s))
              (setq idx (1+ idx)))
            (when (and (< idx len)
                       (eq (aref line idx) ?\t))
              (setq ok nil)
              (throw 'bad nil))
            (let ((indent idx)
                  (content (substring line idx)))
              (cond
               ;; Full-line comment: skip entirely.
               ((string-prefix-p "#" content))
               ;; Block scalar body: content indented deeper than the
               ;; key line that opened the block scalar.
               ((and block-scalar-indent (> indent block-scalar-indent)))
               (t
                ;; A key line ending with | or > opens a block scalar
                ;; body; its body lines must be indented deeper.
                (setq block-scalar-indent
                      (and (> len 0)
                           (let ((last (aref line (1- len))))
                             (if (or (eq last ?\|) (eq last ?>))
                                 indent
                               nil))))
                ;; Strip quoted strings and scan flow brackets.
                (let ((i 0) (n (length content)))
                  (while (< i n)
                    (let ((ch (aref content i)))
                      (cond
                       ((or (eq ch ?\') (eq ch ?\"))
                        ;; Skip a paired quote; doubled quote inside is
                        ;; an escape for single-quoted strings.
                        (let ((q ch) (j (1+ i)) (closed nil))
                          (while (and (< j n) (not closed))
                            (cond
                             ;; Doubled quote escapes itself.
                             ((and (eq q ?\') (eq (aref content j) ?\')
                                   (< (1+ j) n)
                                   (eq (aref content (1+ j)) ?\'))
                              (setq j (+ j 2)))
                             ((eq (aref content j) q)
                              (setq closed t)
                              (setq j (1+ j)))
                             (t (setq j (1+ j)))))
                          (setq i j)))
                       ((eq ch ?\[) (push ?\] stack) (setq i (1+ i)))
                       ((eq ch ?\{) (push ?\} stack) (setq i (1+ i)))
                       ((or (eq ch ?\]) (eq ch ?\}))
                        (if (or (null stack)
                                (not (eq (car stack) ch)))
                            (progn
                              (setq ok nil)
                              (throw 'bad nil))
                          (pop stack)
                          (setq i (1+ i))))
                       (t (setq i (1+ i)))))))))))))
      ;; Any unclosed bracket at end of scan is unbalanced.
      (when stack
        (setq ok nil)
        (throw 'bad nil)))
    ok))

(provide 'scalpel-locate-yaml)

;;; scalpel-locate-yaml.el ends here
