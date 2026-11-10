;;; scalpel-locate-js.el --- JavaScript locator for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Locate top level JavaScript definitions in .js files: function and
;; class declarations plus top level const, let and var names.  The
;; scanner walks the text tracking bracket depth and skips strings,
;; template literals, comments and regex literals.  Registration
;; happens in scalpel-locate.el; this file must not require
;; scalpel-locate, which would load circularly.

;;; Code:

(defconst scalpel-locate-js--decl-regexp
  "\\<\\(?:async[ \t\n\r]+\\)?\\(?:function[ \t\n\r]+\\|class[ \t\n\r]+\\)\\([A-Za-z_$][A-Za-z0-9_$]*\\)\\|\\<\\(?:const\\|let\\|var\\)[ \t\n\r]+\\([A-Za-z_$][A-Za-z0-9_$]*\\)"
  "Regexp matching a declaration keyword and its name.
Group 1 holds a function or class name, group 2 a const, let or
var name.")

(defun scalpel-locate-js--skip-comment-at ()
  "Skip a comment when point sits on one, return non-nil when skipped."
  (when (eq (char-after) ?/)
    (cond ((eq (char-after (1+ (point))) ?/)
           (skip-chars-forward "^\n")
           t)
          ((eq (char-after (1+ (point))) ?*)
           (search-forward "*/" nil t)
           t))))

(defun scalpel-locate-js--skip-string (quote)
  "Skip a quoted string starting at the QUOTE character at point."
  (forward-char)
  (catch 'done
    (while (not (eobp))
      (let ((ch (char-after)))
        (cond ((eq ch 92) (forward-char 2))
              ((eq ch quote)
               (forward-char)
               (throw 'done nil))
              (t (forward-char)))))))

(defun scalpel-locate-js--skip-template ()
  "Skip a template literal starting at the backtick at point.
Interpolation braces are skipped whole so their contents never
disturb any outer depth count."
  (forward-char)
  (catch 'done
    (while (not (eobp))
      (let ((ch (char-after)))
        (cond ((eq ch 92) (forward-char 2))
              ((eq ch 96)
               (forward-char)
               (throw 'done nil))
              ((and (eq ch 36) (eq (char-after (1+ (point))) 123))
               (forward-char)
               (scalpel-locate-js--skip-braces))
              (t (forward-char)))))))

(defun scalpel-locate-js--skip-braces ()
  "Skip one balanced brace group starting at point.
Strings, template literals and comments inside are skipped so
their contents never disturb the depth count."
  (let ((depth 0))
    (catch 'done
      (while (not (eobp))
        (let ((ch (char-after)))
          (cond ((eq ch 123)
                 (setq depth (1+ depth))
                 (forward-char))
                ((eq ch 125)
                 (setq depth (1- depth))
                 (forward-char)
                 (when (= depth 0)
                   (throw 'done nil)))
                ((memq ch (list 34 39)) (scalpel-locate-js--skip-string ch))
                ((eq ch 96) (scalpel-locate-js--skip-template))
                ((scalpel-locate-js--skip-comment-at))
                (t (forward-char))))))))

(defun scalpel-locate-js--skip-regex ()
  "Skip a regex literal starting at the slash at point.
A backslash escapes the next character and a bracketed class is
skipped whole; an unterminated literal stops at end of line."
  (forward-char)
  (catch 'done
    (while (not (eobp))
      (let ((ch (char-after)))
        (cond ((eq ch 92) (forward-char 2))
              ((eq ch 91)
               (forward-char)
               (while (and (not (eobp))
                           (not (eq (char-after) 93)))
                 (if (eq (char-after) 92)
                     (forward-char 2)
                   (forward-char)))
               (unless (eobp)
                 (forward-char)))
              ((eq ch 47) (forward-char) (throw 'done nil))
              ((eq ch 10) (throw 'done nil))
              (t (forward-char)))))))

(defun scalpel-locate-js--regex-context-p ()
  "Return non-nil when the slash at point opens a regex literal.
A slash following an operand character is division; elsewhere it
is treated as a regex so its body cannot disturb bracket counts."
  (save-excursion
    (skip-chars-backward " \t\n\r")
    (or (bobp)
        (memq (char-before)
              (list 40 44 61 58 91 33 38 124 63 123 125 59 42 43 45 126 37 60 62)))))

(defun scalpel-locate-js--skip-balanced (open close)
  "Skip from OPEN at point through its matching CLOSE.
Strings, template literals and comments inside are skipped so
their contents never disturb the depth count."
  (let ((depth 0))
    (catch 'done
      (while (not (eobp))
        (let ((ch (char-after)))
          (cond ((eq ch open)
                 (setq depth (1+ depth))
                 (forward-char))
                ((eq ch close)
                 (setq depth (1- depth))
                 (forward-char)
                 (when (= depth 0)
                   (throw 'done nil)))
                ((memq ch (list 34 39)) (scalpel-locate-js--skip-string ch))
                ((eq ch 96) (scalpel-locate-js--skip-template))
                ((scalpel-locate-js--skip-comment-at))
                (t (forward-char))))))))

(defun scalpel-locate-js--scan-region (beg end)
  "Scan BEG to END in the current buffer.
Return a list whose car is the top level declaration names and
whose cadr is non-nil when all brackets balance."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((names nil)
            (curly 0) (round 0) (bracket 0)
            (balanced t)
            (done nil))
        (while (not done)
          (if (eobp)
              (setq done t)
            (let ((ch (char-after)))
              (cond
               ((memq ch (list 32 9 10 13)) (forward-char))
               ((eq ch 47)
                (cond ((scalpel-locate-js--skip-comment-at))
                      ((scalpel-locate-js--regex-context-p)
                       (scalpel-locate-js--skip-regex))
                      (t (forward-char))))
               ((memq ch (list 34 39)) (scalpel-locate-js--skip-string ch))
               ((eq ch 96) (scalpel-locate-js--skip-template))
               ((eq ch 123)
                (setq curly (1+ curly))
                (forward-char))
               ((eq ch 125)
                (setq curly (1- curly))
                (forward-char)
                (when (< curly 0)
                  (setq balanced nil done t)))
               ((eq ch 40)
                (setq round (1+ round))
                (forward-char))
               ((eq ch 41)
                (setq round (1- round))
                (forward-char)
                (when (< round 0)
                  (setq balanced nil done t)))
               ((eq ch 91)
                (setq bracket (1+ bracket))
                (forward-char))
               ((eq ch 93)
                (setq bracket (1- bracket))
                (forward-char)
                (when (< bracket 0)
                  (setq balanced nil done t)))
               ((and (= curly 0) (= round 0) (= bracket 0)
                     (looking-at scalpel-locate-js--decl-regexp))
                (let ((name (or (match-string-no-properties 1)
                                (match-string-no-properties 2))))
                  (when name (push name names)))
                (goto-char (match-end 0)))
               (t (forward-char))))))
        (list (nreverse names)
              (and balanced (= curly 0) (= round 0) (= bracket 0)))))))

(defun scalpel-locate-js-scan-region (beg end)
  "Scan BEG..END for JS declarations via `scalpel-locate-js--scan-region'.
Public entry point for same-package callers such as the HTML
locator, so they need not reference the internal
`scalpel-locate-js--scan-region' or `scalpel-locate-js--decl-regexp'
directly."
  (scalpel-locate-js--scan-region beg end))

(defun scalpel-locate-js-list-symbols (_file)
  "Return top level declaration names of the current JS buffer."
  (car (scalpel-locate-js--scan-region (point-min) (point-max))))

(defun scalpel-locate-js-balanced-p (text)
  "Return non-nil when the brackets in TEXT balance as JS source."
  (with-temp-buffer
    (insert text)
    (nth 1 (scalpel-locate-js--scan-region (point-min) (point-max)))))

(defun scalpel-locate-js-single-definition-p (text)
  "Return non-nil when TEXT is exactly one complete JS definition."
  (with-temp-buffer
    (insert text)
    (let ((result (scalpel-locate-js--scan-region (point-min) (point-max))))
      (and (nth 1 result)
           (= (length (car result)) 1)
           (progn
             (goto-char (point-min))
             (skip-chars-forward " \t\n\r")
             (looking-at scalpel-locate-js--decl-regexp))))))

(defun scalpel-locate-js--definition-end ()
  "Return the end position of the declaration whose name just matched.
A function or class declaration is ended by its brace group; a
variable declaration runs to a semicolon or a newline at depth
zero."
  (skip-chars-forward " \t\n\r")
  (if (eq (char-after) 40)
      (progn
        (scalpel-locate-js--skip-balanced 40 41)
        (skip-chars-forward " \t\n\r")
        (if (eq (char-after) 123)
            (progn
              (scalpel-locate-js--skip-balanced 123 125)
              (point))
          (point)))
    (let ((depth 0)
          (stop nil))
      (while (and (not stop) (not (eobp)))
        (let ((ch (char-after)))
          (cond
           ((scalpel-locate-js--skip-comment-at))
           ((memq ch (list 34 39)) (scalpel-locate-js--skip-string ch))
           ((eq ch 96) (scalpel-locate-js--skip-template))
           ((and (eq ch 47) (scalpel-locate-js--regex-context-p))
            (scalpel-locate-js--skip-regex))
           ((memq ch (list 40 91 123))
            (setq depth (1+ depth))
            (forward-char))
           ((memq ch (list 41 93 125))
            (if (= depth 0)
                (setq stop t)
              (setq depth (1- depth))
              (forward-char)))
           ((and (= depth 0) (eq ch 59))
            (forward-char)
            (setq stop t))
           ((and (= depth 0) (eq ch 10))
            (setq stop t))
           (t (forward-char)))))
      (point))))

(defun scalpel-locate-js-range (_file symbol)
  "Return the bounds of SYMBOL's declaration in the current buffer.
The buffer holds the JS file, opened by the dispatcher."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found)
                  (re-search-forward scalpel-locate-js--decl-regexp nil t))
        (let ((name (or (match-string-no-properties 1)
                        (match-string-no-properties 2))))
          (if (and name (string= name symbol))
              (setq found (cons (match-beginning 0)
                                (scalpel-locate-js--definition-end)))
            (goto-char (match-end 0)))))
      found)))

(provide 'scalpel-locate-js)

;;; scalpel-locate-js.el ends here
