;;; scalpel-llm-laguna.el --- Laguna reply dialect for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Reply dialect for Laguna backends.  Laguna writes its answer in a
;; text calling convention of its own -- one `<tool_call>' block per
;; action, each holding a tool name and `<arg_key>'/`<arg_value>'
;; pairs -- instead of the JSON action array Scalpel asks for.  The
;; names inside those blocks are Scalpel's own, so the plan is right
;; and only the envelope around it is wrong; this module converts the
;; envelope, one action per block, in the order written.
;;
;; The conversion is narrow on purpose.  It runs only for a reply that
;; holds a complete, well-formed call: a complete call is taken even
;; when a bracket inside an argument makes the text look like JSON
;; (shell commands carry brackets all the time), while markup the
;; reply merely mentions, and a call the backend cut off, still reach
;; `scalpel-llm-dialect--parse-error' and are refused as loudly as
;; before.  A field this dialect is known to omit is filled from
;; `scalpel-llm-laguna--assumed-fields' and reported in the echo
;; area, so the substitution is visible rather than silent.
;;
;; Dispatch tries the backend's gptel name and then its model name, so
;; a backend named after the provider -- "OpenRouter", say -- reaches
;; this parser through the model it serves, with no registration of
;; its own.  A model name is read only when the backend declares that
;; model in `:models', because `gptel-model' survives the backend it
;; was chosen for; the README's configuration declares the Laguna
;; model on the OpenRouter backend, which is what makes this parser
;; reachable there.  A backend serving a model no registration matches
;; still gets the loud refusal below.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'scalpel-llm-dialect)

(defconst scalpel-llm-laguna--call-tag "tool_call"
  "Tag name wrapping one tool call written as plain text.
Structural contract shared by the scanner and the tests.")

(defconst scalpel-llm-laguna--key-tag "arg_key"
  "Tag name wrapping an argument name inside a call.
Structural contract shared by the scanner and the tests.")

(defconst scalpel-llm-laguna--value-tag "arg_value"
  "Tag name wrapping an argument value inside a call.
Structural contract shared by the scanner and the tests.")

(defconst scalpel-llm-laguna--assumed-fields
  '(("shell" :long-running t))
  "Fields this dialect leaves out, with the value assumed for them.
Each entry is (TOOL . PLIST): a field in PLIST that the reply did not
state is filled in.  Every observed reply gave \"command\" and
\"reason\" for a shell call and no \"long-running\", and the value
assumed here is t, which makes the round ask the user before the
command runs.  Assuming false instead would let the model decide, by
silence, that a command needs no confirmation, and that decision
belongs to the user.  Each fill is reported through `message', so it
never happens quietly.")

(defun scalpel-llm-laguna--marker (tag &optional closing)
  "Return the marker wrapping TAG, or its closing form when CLOSING.
The result is regexp-quoted, so `string-match' matches it literally
and the match data names the marker's own bounds."
  (concat "<" (if closing "/" "") (regexp-quote tag) ">"))

(defun scalpel-llm-laguna--tag-value (raw pos tag)
  "Return (VALUE . END) for the next TAG pair in RAW at or after POS.
VALUE is the text between the pair's own markers, taken verbatim: it
may span lines, and whitespace around it is content rather than
padding, so a command or a code body is not rewritten on the way in.
END is the position just past the closing marker.  Return nil when
no complete TAG pair follows POS."
  (let ((start (string-match (scalpel-llm-laguna--marker tag) raw pos)))
    (when start
      (let* ((value-start (match-end 0))
             (end (string-match (scalpel-llm-laguna--marker tag t)
                                raw value-start)))
        (when end
          (cons (substring raw value-start end) (match-end 0)))))))

(defun scalpel-llm-laguna--action (tool args)
  "Return an action plist for TOOL built from ARGS.
ARGS is the list of (NAME . VALUE) strings the call wrote; each NAME
becomes a keyword.  A field listed for TOOL in
`scalpel-llm-laguna--assumed-fields' that ARGS leaves out is filled
in, and every fill is reported through `message'.  The fields are not
otherwise checked here: the per-tool contract belongs to
`scalpel-agent--validate-action', so it stays in one place."
  (let ((action (append (list :tool tool)
                        (cl-loop for (name . value) in args
                                 append (list (intern (concat ":" name))
                                              value))))
        (assumed (cdr (assoc tool scalpel-llm-laguna--assumed-fields))))
    (while assumed
      ;; A plist's keys are symbols, so `memq' is the presence test
      ;; that does not have to consult the value to find the key.
      (unless (memq (car assumed) action)
        (message "Scalpel: Laguna stated no %S for a %s action; assuming %S"
                 (car assumed) tool (cadr assumed))
        (setq action (plist-put action (car assumed) (cadr assumed))))
      (setq assumed (cddr assumed)))
    action))

(defun scalpel-llm-laguna--call-to-action (body)
  "Return the call written in BODY as an action plist, or nil.
BODY is the text between one call's markers: the tool name followed
by zero or more argument pairs.  Return nil when the tool name is
empty or an argument is incomplete, so a caller can refuse the whole
reply instead of acting on the part that happened to parse."
  (let* ((key-open (scalpel-llm-laguna--marker scalpel-llm-laguna--key-tag))
         (first-key (or (string-match key-open body) (length body)))
         (tool (string-trim (substring body 0 first-key)))
         (pos first-key)
         (args nil)
         (ok (not (string-empty-p tool))))
    (while (and ok (string-match key-open body pos))
      (let ((key (scalpel-llm-laguna--tag-value
                  body pos scalpel-llm-laguna--key-tag)))
        (if (null key)
            (setq ok nil)
          (let ((value (scalpel-llm-laguna--tag-value
                        body (cdr key) scalpel-llm-laguna--value-tag)))
            (if (null value)
                (setq ok nil)
              (let ((name (string-trim (car key))))
                (unless (string-empty-p name)
                  (push (cons name (car value)) args))
                (setq pos (cdr value))))))))
    (when ok
      (scalpel-llm-laguna--action tool (nreverse args)))))

(defun scalpel-llm-laguna--parse-calls (raw)
  "Return one action plist per tool call in RAW, or nil.
RAW is the whole planner reply.  Return nil when RAW holds no
complete call, and also when any call is unterminated or holds an
incomplete argument: a reply the backend cut off must not have its
finished prefix executed, so the refusal covers the reply rather
than the call."
  (let ((pos 0)
        (open (scalpel-llm-laguna--marker scalpel-llm-laguna--call-tag))
        (close (scalpel-llm-laguna--marker scalpel-llm-laguna--call-tag t))
        (calls nil)
        (ok t))
    (while (and ok (string-match open raw pos))
      (let ((body-start (match-end 0)))
        (let ((end (string-match close raw body-start)))
          (if (null end)
              (setq ok nil)
            ;; The end position is read here, before anything else
            ;; matches, because scanning the body moves the match data.
            (let ((next (match-end 0))
                  (action nil))
              (setq action (scalpel-llm-laguna--call-to-action
                            (substring raw body-start end)))
              (if action
                  (push action calls)
                (setq ok nil))
              (setq pos next))))))
    (when ok
      (nreverse calls))))

(defun scalpel-llm-laguna-parse-reply (raw)
  "Parse a Laguna raw reply RAW into a list of action plists.
RAW holding a complete text call is converted, one action per call,
in the order written.  Anything else goes to
`scalpel-llm-dialect--default-parse' unchanged, which reads a JSON
action array and refuses the rest loudly: a call the backend cut off,
and markup the reply only quotes, both yield no complete call here
and so are refused there."
  (let ((calls (scalpel-llm-laguna--parse-calls raw)))
    (if calls
        calls
      (scalpel-llm-dialect--default-parse raw))))

(scalpel-llm-dialect-register
 "laguna"
 (list :parse-reply #'scalpel-llm-laguna-parse-reply))

(provide 'scalpel-llm-laguna)

;;; scalpel-llm-laguna.el ends here
