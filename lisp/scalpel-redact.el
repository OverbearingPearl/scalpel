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
  (concat "/Users/" (regexp-quote (user-real-login-name)) "/"))

(defun scalpel-redact--home-placeholder ()
  "Placeholder standing in for the user name in home paths."
  "{{SCALPEL_USER}}/")

(defun scalpel-redact-install-defaults ()
  "Install the built-in rules when none are registered yet.
The built-in rule rewrites the user name inside \"/Users/NAME/\"
home paths, the value models most often mistype."
  (unless scalpel-redact-rules
    (scalpel-redact-register
     (scalpel-redact--home-pattern)
     (scalpel-redact--home-placeholder)
     (concat "/Users/" (user-real-login-name)))))

(defun scalpel-redact-apply (text)
  "Return TEXT with every rule's pattern replaced by its placeholder.
Applies rules in registration order; later rules see the output of
earlier ones, so a rule must not match another's placeholder."
  (scalpel-redact-install-defaults)
  (let ((result text))
    (dolist (rule scalpel-redact-rules)
      (setq result
            (replace-regexp-in-string
             (plist-get rule :pattern)
             (plist-get rule :placeholder)
             result
             'fixedcase 'literal)))
    result))

(defun scalpel-redact-restore (text)
  "Return TEXT with every placeholder replaced back by its secret.
Applies rules in reverse registration order, mirroring
`scalpel-redact-apply'.  Placeholders the model split or altered
are left alone: they surface in the reply and are diagnosed there
rather than being silently dropped."
  (scalpel-redact-install-defaults)
  (let ((result text))
    (dolist (rule (reverse scalpel-redact-rules))
      (setq result
            (replace-regexp-in-string
             (regexp-quote (plist-get rule :placeholder))
             (plist-get rule :secret)
             result
             'fixedcase 'literal)))
    result))

(provide 'scalpel-redact)

;;; scalpel-redact.el ends here
