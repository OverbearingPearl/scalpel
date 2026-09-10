;;; scalpel-agent.el --- LLM planning and action execution -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Provides context, structured-plan parsing, and action dispatch.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'scalpel-llm)
(require 'scalpel-locate)
(require 'scalpel-execute)

(defcustom scalpel-agent-system-prompt
  (if (boundp 'scalpel-system-prompt)
      scalpel-system-prompt
    "You are a precise code transformation tool. The user gives you
context and an instruction. Return ONLY a JSON array of actions.
The top-level response must be a JSON array, never a single object.
Each action is either {\"tool\":\"edit\",\"file\":\"/abs/path.el\",\"symbol\":\"name\",\"instruction\":\"...\"}
or {\"tool\":\"reply\",\"text\":\"...\"}. Never emit code or diff text in this response.")
  "System prompt for the Scalpel agent planner."
  :type 'string
  :group 'scalpel)

(defun scalpel-agent-context ()
  "List open files that have a registered locator, with their symbols."
  (let (files)
    (dolist (buf (buffer-list))
      (let ((file (buffer-file-name buf)))
        (when (and file (scalpel-locate-provider-for-file file))
          (push (format "FILE: %s\nSYMBOLS: %s"
                        file
                        (string-join (scalpel-locate-list-symbols file) ", "))
                files))))
    (string-join (nreverse files) "\n\n")))

(defun scalpel-agent--strip-fences (raw)
  "Strip markdown code fences surrounding RAW, if present."
  (let ((text (string-trim raw)))
    (when (string-match-p "```" text)
      (setq text (string-trim
                  (replace-regexp-in-string
                   "```[a-zA-Z]*\n?\\|\n?```\\'" "" text))))
    text))

(defun scalpel-agent--parse-json (raw)
  "Parse RAW to a list of action plists.
Signal `user-error' when RAW is not valid JSON or not a JSON array
of objects."
  (let ((parsed (condition-case nil
                    (json-parse-string (scalpel-agent--strip-fences raw)
                                       :object-type 'plist
                                       :array-type 'list)
                  (error
                   (user-error "Scalpel: planner returned invalid JSON: %S"
                               raw)))))
    (when (and (plistp parsed) (plist-get parsed :tool))
      (setq parsed (list parsed)))
    (unless (and (listp parsed)
                 (cl-every (lambda (item) (plist-get item :tool)) parsed))
      (user-error
       (concat "Scalpel: planner returned unexpected structure "
               "(expected a JSON array of action objects): %S")
       raw))
    parsed))

(defun scalpel-agent-plan (instruction)
  "Ask the LLM for a structured plan for INSTRUCTION.
Return a list of plists with keys :tool :file :symbol :instruction :text."
  (let* ((prompt (format "%s\n\nUser instruction:\n%s"
                         (scalpel-agent-context) instruction))
         (raw (scalpel-llm-request prompt scalpel-agent-system-prompt))
         (actions (scalpel-agent--parse-json raw)))
    (mapcar
     (lambda (item)
       (list :tool (plist-get item :tool)
             :file (plist-get item :file)
             :symbol (plist-get item :symbol)
             :instruction (plist-get item :instruction)
             :text (plist-get item :text)))
     actions)))

(defun scalpel-agent--apply-if-unchanged (file symbol expected-body new-text)
  "Replace SYMBOL in FILE with NEW-TEXT only if EXPECTED-BODY is unchanged.
Return a human-readable report string.  Signal `user-error' if the target
region was modified while an LLM request was in flight."
  (with-current-buffer (find-file-noselect file)
    (let* ((current-range (scalpel-locate-range file symbol))
           (current-body (buffer-substring-no-properties
                          (car current-range) (cdr current-range))))
      (unless (string= expected-body current-body)
        (user-error
         (concat "Scalpel: target region changed while editing %s; "
                 "aborting.  Re-run after reviewing the buffer")
         symbol))
      (scalpel-execute-replace (car current-range) (cdr current-range) new-text)
      (format "Edited %s in %s" symbol (buffer-name (current-buffer))))))

(defun scalpel-agent--single-definition-p (text)
  "Return non-nil when TEXT is exactly one top-level defining form."
  (condition-case nil
      (let* ((parsed (read-from-string text))
             (form (car parsed))
             (end (cdr parsed)))
        (and (listp form)
             (memq (car form)
                   '(defun defmacro defvar defcustom defconst))
             (= end (length text))))
    (error nil)))

(defun scalpel-agent-edit (file symbol instruction)
  "Edit SYMBOL in FILE per INSTRUCTION using boundary-locked apply.
Return human-readable report string."
  (unless (and file symbol instruction)
    (user-error "Scalpel: malformed edit action"))
  (let ((range (scalpel-locate-range file symbol)))
    (unless range
      (user-error "Scalpel: can't locate %s in %s" symbol file))
    (with-current-buffer (find-file-noselect file)
      (let* ((beg (car range))
             (end (cdr range))
             (body (buffer-substring-no-properties beg end))
             (signature (save-excursion
                          (goto-char beg)
                          (buffer-substring-no-properties
                           beg (line-end-position))))
             (prompt (concat "Signature: %s\n\nCurrent block:\n%s\n\n"
                             "Instruction: %s\n\n"
                             "Return only the full replacement definition, as plain "
                             "Emacs Lisp text. Do not include markdown fences or "
                             "explanations. If the requested change is impossible or "
                             "unnecessary for this block, return exactly: NO_CHANGE"))
             (new-text (string-trim (scalpel-llm-request
                                     (format prompt signature body instruction)))))
        (cond
         ((string= new-text "NO_CHANGE")
          (format "No change needed: %s in %s" symbol
                  (buffer-name (current-buffer))))
         ((scalpel-agent--single-definition-p new-text)
          (scalpel-agent--apply-if-unchanged
           file symbol body new-text))
         (t
          (user-error
           (concat "Scalpel: planner returned no usable replacement for %s. "
                   "Refusing to edit. Reply was: %S")
           symbol new-text)))))))

(defun scalpel-agent-execute-action (action)
  "Execute a single ACTION plist and return a report string."
  (let ((tool (plist-get action :tool)))
    (pcase tool
      ("edit"
       (scalpel-agent-edit
        (plist-get action :file)
        (plist-get action :symbol)
        (plist-get action :instruction)))
      ("reply"
       (format "%s" (or (plist-get action :text) "")))
      (_
       (format "Unknown action: %S" tool)))))

(defun scalpel-agent-run (instruction)
  "Run a full agent cycle for INSTRUCTION and return the combined report."
  (mapconcat #'scalpel-agent-execute-action
             (scalpel-agent-plan instruction)
             "\n"))

(provide 'scalpel-agent)

;;; scalpel-agent.el ends here
