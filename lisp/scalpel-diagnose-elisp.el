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
;; Two facts are reported, both measured, not guessed: where the walk
;; first goes unbalanced (a stray closer with nothing open), and which
;; opening delimiters are still open when the text ends -- how many of
;; them, and the line and column of the deepest still-open one.  The
;; registry hands this back as a plist; the agent decides what the
;; refusal sentence says about it, so no wording names this language.

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

(defun scalpel-diagnose-elisp-open-stack-report (text)
  "Return a report describing unmatched opening delimiters in TEXT.
The function tracks opening round parentheses, square brackets, and
braces, along with their positions.  Each stack entry records the
0-based offset of the opening delimiter together with its 1-based
line number and column.  A closing delimiter removes the most recent
entry from the stack when the stack is non-empty.  The return value
is a four-element list whose car is the number of delimiters left
open and whose elements at indices 1, 2, and 3 are the 0-based
offset, the 1-based line number, and the 1-based column of the most
recent still-open delimiter; those line and column values are
reused from the stack entry recorded at push time.  Return nil when
the stack is empty."
  (let ((openers '(?\( ?\[ ?\{))
        (closers '(?\) ?\] ?\}))
        (stack nil)
        (line 1)
        (col 0))
    (cl-loop for i from 0 below (length text)
             for ch = (aref text i)
             do (progn
                  (cond
                   ((eq ch ?\n)
                    (setq line (1+ line)
                          col 0))
                   (t (setq col (1+ col))))
                  (cond
                   ((memq ch openers)
                    (push (list i line col) stack))
                   ((memq ch closers)
                    (when stack (pop stack)))))
             finally return (when stack
                              (let ((entry (car stack)))
                                (list (length stack)
                                      (nth 0 entry)
                                      (nth 1 entry)
                                      (nth 2 entry)))))))

(defun scalpel-diagnose-elisp-bracket-report (text)
  "Answer the bracket question about TEXT for Emacs Lisp files.
Return nil when TEXT's brackets balance.  When the walk first finds
a closer with nothing open, return (:kind unbalance :offset N).
When brackets are merely left open at the end, return (:kind
unclosed :count N :offset O :line L :col C), where O is the offset,
and L and C the line and column, of the deepest still-open opener.
This is the provider the registry in `scalpel-diagnose' dispatches
to for .el files; the caller builds the refusal sentence from the
facts, so the wording stays in the agent, not here."
  (let ((offset (scalpel-diagnose-elisp-first-unbalance-offset text)))
    (cond
     ((and (numberp offset) (< offset (length text)))
      (list :kind 'unbalance :offset offset))
     (t
      (let ((report (scalpel-diagnose-elisp-open-stack-report text)))
        (when report
          (list :kind 'unclosed
                :count (nth 0 report)
                :offset (nth 1 report)
                :line (nth 2 report)
                :col (nth 3 report))))))))

(scalpel-diagnose-register-paren-provider '("el")
                                            #'scalpel-diagnose-elisp-bracket-report)

(provide 'scalpel-diagnose-elisp)

;;; scalpel-diagnose-elisp.el ends here
