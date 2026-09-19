;;; scalpel-redact.el --- Bidirectional text handling between client and LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Bidirectional text transformation applied at the client/LLM
;; boundary.  Outbound text (prompts, history, reports) is rewritten
;; to placeholder words the model cannot mistake for real values;
;; inbound replies have the placeholders restored before anything
;; downstream parses them.  The first rule hides the local user name,
;; which models routinely mangle inside absolute paths; further rules
;; (tokens, host names, other path parts) can be registered later.

;;; Code:

(defcustom scalpel-redact-rules nil
  "Ordered list of redaction rules applied at the LLM boundary.
Each rule is a plist:
  :pattern     regexp matching the secret text in outbound strings
  :placeholder the placeholder text replacing each match, and the
               only text the restore step maps back
Rules are applied in order on send and in reverse on restore, so a
rule may not produce text another rule's placeholder contains."
  :type '(repeat plist)
  :group 'scalpel)

(defcustom scalpel-redact-enabled t
  "Non-nil means redaction is active at the LLM boundary.
When non-nil, `scalpel-redact-apply' applies the rules to outbound
text and `scalpel-redact-restore' rewrites placeholders back to the
original text.  When nil, BOTH functions are no-ops: text passes
through untouched in either direction, so a placeholder the user
types by hand is never rewritten."
  :type 'boolean
  :group 'scalpel)

(defun scalpel-redact-register (pattern placeholder secret)
  "Register a redaction rule mapping PATTERN to PLACEHOLDER with SECRET.
A rule for the same PATTERN replaces the earlier one, so reloading
a configuration does not duplicate rules.
PLACEHOLDER is the text that redaction substitutes into buffers;
SECRET is the original secret text that replaces PLACEHOLDER on restore."
  (setq scalpel-redact-rules
        (append
         (cl-remove-if
          (lambda (rule) (equal (plist-get rule :pattern) pattern))
          scalpel-redact-rules)
         (list (list :pattern pattern :placeholder placeholder :secret secret))))
  nil)

(defun scalpel-redact--home-pattern ()
  "Regexp matching the user name part of absolute home paths."
  (concat "/Users/" (regexp-quote (user-real-login-name))))

(defun scalpel-redact--home-placeholder ()
  "Placeholder standing in for the user name in home paths."
  "{{SCALPEL_USER}}")

(defun scalpel-redact-install-defaults ()
  "Install the built-in rules when none are registered yet.
The built-in rule rewrites the user name inside \"/Users/NAME/\"
home paths, the value models most often mistype.  The pattern,
placeholder, and secret all use the slash-less \"/Users/NAME\"
prefix with no trailing slash; the slash after the user name
always comes from the surrounding text itself, so apply and
restore are exact inverse operations and no double slash can
ever appear."
  (unless scalpel-redact-rules
    (scalpel-redact-register
     (scalpel-redact--home-pattern)
     (scalpel-redact--home-placeholder)
     (concat "/Users/" (user-real-login-name)))))

(defun scalpel-redact-apply (text)
  "Return TEXT with every rule's pattern replaced by its placeholder.
Applies rules in registration order; later rules see the output of
earlier ones, so a rule must not match another's placeholder."
  (if (not scalpel-redact-enabled)
      text
    (scalpel-redact-install-defaults)
    (let ((result text))
      (dolist (rule scalpel-redact-rules)
        (setq result
              (replace-regexp-in-string
               (plist-get rule :pattern)
               (plist-get rule :placeholder)
               result
               'fixedcase 'literal)))
      result)))

(defun scalpel-redact-restore (text)
  "Return TEXT with every placeholder replaced back by its secret.
Applies rules in reverse registration order, mirroring
`scalpel-redact-apply'.  Only rewrites placeholders that were
redacted while `scalpel-redact-enabled' was on; when the switch is
off, restore is a no-op and TEXT is returned unchanged, so a
placeholder typed manually by the user is never rewritten into the
secret.  Placeholders the model split or altered are left alone:
they surface in the reply and are diagnosed there rather than being
silently dropped."
  (if (not scalpel-redact-enabled)
      text
    (scalpel-redact-install-defaults)
    (let ((result text))
      (dolist (rule (reverse scalpel-redact-rules))
        (setq result
              (replace-regexp-in-string
               (regexp-quote (plist-get rule :placeholder))
               (plist-get rule :secret)
               result
               'fixedcase 'literal)))
      result)))

(defun scalpel-redact-placeholder-drift (text)
  "Detect mistyped REDACT placeholders in TEXT and return a diagnostic or nil.
Redaction is only active when `scalpel-redact-enabled' is non-nil;
defaults are installed first so the built-in rules are considered.
We scan TEXT for {{...}} tokens made of uppercase letters and
underscores and compare each token, as a whole, against the
registered placeholders' inner names: an edit distance of one or
two characters counts as drift (e.g. {{SCCALPEL_USER}} vs
{{SCALPEL_USER}}).  Restore works by literal match only, so a
drifted token will never be restored; naming the exact placeholder
lets the model spell it character for character next round and
keeps the restore path exact.  Returns nil when redaction is off
or no drifted token is found."
  (when (and (boundp 'scalpel-redact-enabled)
             scalpel-redact-enabled
             (fboundp 'scalpel-redact-install-defaults)
             (scalpel-redact-install-defaults)
             (boundp 'scalpel-redact-rules)
             scalpel-redact-rules)
    (let ((placeholders (mapcar (lambda (rule)
                                  (let ((ph (plist-get rule :placeholder)))
                                    (and (stringp ph)
                                         (string-match
                                          "\\`{{\\([A-Z_]+\\)}}\\'" ph)
                                         (match-string 1 ph))))
                                scalpel-redact-rules))
          (found nil)
          (pos 0))
      (while (string-match "{{[A-Z_]+}}" text pos)
        (let* ((token (match-string 0 text))
               (inner (substring token 2 (- (length token) 2))))
          (setq pos (match-end 0))
          (unless (member token (mapcar (lambda (rule)
                                          (plist-get rule :placeholder))
                                        scalpel-redact-rules))
            (let ((close (seq-find
                          (lambda (name)
                            (let ((d (scalpel-redact--levenshtein inner name)))
                              (and (>= d 1) (<= d 2))))
                          (delq nil placeholders))))
              (when close
                (push (concat "\"" token "\" looks like a mistyped "
                              "redaction placeholder; the exact placeholder "
                              "is " close " -- spell it character for "
                              "character so it can be restored")
                      found))))))
      (when found
        (string-join (nreverse found) "\n")))))

(defun scalpel-redact--levenshtein (a b)
  "Return the edit distance between strings A and B."
  (let* ((la (length a))
         (lb (length b))
         (w (1+ lb))
         (grid (make-vector (* (1+ la) w) 0)))
    (dotimes (i (1+ la))
      (aset grid (* i w) i))
    (dotimes (j (1+ lb))
      (aset grid j j))
    (dotimes (i la)
      (dotimes (j lb)
        (let ((idx (+ (* (1+ i) w) (1+ j))))
          (aset grid idx
                (min (1+ (aref grid (+ (* i w) (1+ j))))
                     (1+ (aref grid (+ (* (1+ i) w) j)))
                     (+ (aref grid (+ (* i w) j))
                        (if (= (aref a i) (aref b j)) 0 1)))))))
    (aref grid (+ (* la w) lb))))

(provide 'scalpel-redact)

;;; scalpel-redact.el ends here
