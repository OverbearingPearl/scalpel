;;; scalpel-diagnose-elisp.el --- Emacs Lisp bracket diagnostics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Emacs Lisp's answer to the bracket-diagnosis question.  The agent
;; module asks the registry in `scalpel-diagnose' what a refused reply's
;; bracket defect looks like; this file is the language-specific half.
;; It scans TEXT the way the Emacs Lisp reader would see delimiters:
;; round parentheses, square brackets, and braces, opened and closed
;; without any notion of strings or comments -- the same plain-walk
;; answer `scalpel-locate-balanced-p' bases its judgement on.
;;
;; Three facts are reported, all measured, not guessed: where the walk
;; first goes unbalanced (a stray closer with nothing open), which
;; opening delimiters are still open when the text ends -- how many of
;; them, and the line and column of the deepest still-open one -- and,
;; through an indentation heuristic, which of the still-open openers
;; the missing closer most plausibly belongs to: the innermost opener
;; whose column equals the indentation of the last non-blank line.
;; The heuristic is a hint, never a verdict, and travels in the report
;; as its own key so the caller can present it as such.  The registry
;; hands this back as a plist; the agent decides what the refusal
;; sentence says about it, so no wording names this language.

;;; Code:

(require 'cl-lib)
(require 'scalpel-diagnose)

(defun scalpel-diagnose-elisp-first-unbalance-offset (text)
  "Return the offset in TEXT where it first becomes bracket-unbalanced.
Scans the text as a plain character string, so the answer is the
same one `scalpel-locate-balanced-p' would be asked about; returns
the length of TEXT when the imbalance is only an unclosed bracket
that never finds its close."
  (let ((depth 0)
        (openers '(?\( ?\[ ?\{))
        (closers '(?\) ?\] ?\})))
    (catch 'result
      (cl-loop for i from 0 below (length text)
               for ch = (aref text i)
               do (cond
                   ((memq ch openers) (setq depth (1+ depth)))
                   ((memq ch closers)
                    (setq depth (1- depth))
                    (when (< depth 0)
                      (throw 'result i))))
               finally (throw 'result (length text))))))

(defun scalpel-diagnose-elisp--last-nonblank-indent (text)
  "Return the indentation column of the last non-blank line in TEXT.
Return nil when TEXT holds no non-blank line.  A blank line at the
end of TEXT is transparent: the scan walks back over it to find the
last line that contains any character other than whitespace."
  (let ((end (length text)))
    (catch 'indent
      (while (> end 0)
        (let ((line-start end))
          (while (and (> line-start 0)
                      (not (eq (aref text (1- line-start)) ?\n)))
            (setq line-start (1- line-start)))
          (let ((i line-start))
            (while (and (< i end) (memq (aref text i) '(?\s ?\t)))
              (setq i (1+ i)))
            (if (< i end)
                (throw 'indent (- i line-start))
              (setq end line-start)))))
      nil)))

(defun scalpel-diagnose-elisp-open-stack-report (text)
  "Walk TEXT with a plain bracket walk, keeping the whole open stack.
Return a plist describing the delimiters still open when TEXT ends:
:count is how many remain open; :offset, :line and :col locate the
deepest still-open opener, exactly as before.  :openers is every
still-open opener as a plist (:offset :line :col :depth) in source
order, so the caller can name more than one candidate.  :likely is
the indentation heuristic's guess at which opener the missing closer
belongs to -- the innermost still-open opener whose column equals
the indentation of the last non-blank line -- or nil when no opener
matches, so a wrong guess is visible as a missing key's value rather
than silently attributed.  Return nil when nothing is left open.
No string or comment awareness, like the rest of this file."
  (let ((openers '(?\( ?\[ ?\{))
        (closers '(?\) ?\] ?\}))
        (stack nil)
        (depth 0)
        (line 1)
        (col 0))
    (cl-loop for i from 0 below (length text)
             for ch = (aref text i)
             do (cond
                 ((eq ch ?\n)
                  (setq line (1+ line)
                        col 0))
                 ((memq ch openers)
                  (setq depth (1+ depth))
                  (push (list :offset i :line line :col col :depth depth)
                        stack))
                 ((memq ch closers)
                  (when stack
                    (setq depth (1- depth)
                          stack (cdr stack))))
                 (t (setq col (1+ col)))))
    (when stack
      (let* ((entries (nreverse stack))
             (top (car (last entries)))
             (indent (scalpel-diagnose-elisp--last-nonblank-indent text))
             (likely (when indent
                       (car (last
                             (cl-remove-if-not
                              (lambda (entry)
                                (= (plist-get entry :col) indent))
                              entries))))))
        (list :count (length entries)
              :offset (plist-get top :offset)
              :line (plist-get top :line)
              :col (plist-get top :col)
              :openers entries
              :likely likely)))))

(defun scalpel-diagnose-elisp-bracket-report (text)
  "Answer the bracket question about TEXT for Emacs Lisp files.
Return nil when TEXT's brackets balance.  When the walk first finds
a closer with nothing open, return (:kind unbalance :offset N).
When brackets are merely left open at the end, return (:kind
unclosed :count N :offset O :line L :col C :openers ENTRIES :likely
ENTRY), where O is the offset and L and C the line and column of
the deepest still-open opener, ENTRIES the full list of still-open
openers in source order, and ENTRY the indentation heuristic's
guess at the opener the missing closer belongs to, or nil when no
opener matches the last non-blank line's indentation.  ENTRY is a
hint, not a verdict; callers should present it as such.  This is
the provider the registry in `scalpel-diagnose' dispatches to for
.el files; the caller builds the refusal sentence from the facts,
so the wording stays in the agent, not here."
  (let ((offset (scalpel-diagnose-elisp-first-unbalance-offset text)))
    (cond
     ((and (numberp offset) (< offset (length text)))
      (list :kind 'unbalance :offset offset))
     (t
      (let ((report (scalpel-diagnose-elisp-open-stack-report text)))
        (when report
          (list :kind 'unclosed
                :count (plist-get report :count)
                :offset (plist-get report :offset)
                :line (plist-get report :line)
                :col (plist-get report :col)
                :openers (plist-get report :openers)
                :likely (plist-get report :likely))))))))

(scalpel-diagnose-register-paren-provider '("el")
                                            #'scalpel-diagnose-elisp-bracket-report)

(provide 'scalpel-diagnose-elisp)

;;; scalpel-diagnose-elisp.el ends here
