;;; scalpel.el --- Emacs-native deterministic coding agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9.6"))
;; Keywords: tools
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Scalpel is a deterministic Emacs-native coding agent.  The LLM plans and
;; generates text; deterministic locators resolve exact byte ranges, and edits
;; are applied only inside that resolved boundary.

;;; Code:

;; Ensure that the `lisp' subdirectory is on `load-path' so that
;; internal modules can be `require'd from the top-level entry point.
(let* ((file (or load-file-name
                 (and (boundp 'buffer-file-name) buffer-file-name)))
       (root (and file (file-name-directory file)))
       (lisp-dir (and root (expand-file-name "lisp" root))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'scalpel-console)

(defgroup scalpel nil
  "Deterministic Emacs-native coding agent."
  :group 'tools)

(defcustom scalpel-system-prompt
  "You are a precise code transformation tool. The user gives you
context and an instruction. Return ONLY a JSON array of actions.
The top-level response must be a JSON array, never a single object.
Each action is either {\"tool\":\"edit\",\"file\":\"/abs/path.el\",\"symbol\":\"name\",\"instruction\":\"...\"}
or {\"tool\":\"reply\",\"text\":\"...\"}. Never emit code or diff text in this response.
Files shown as \"FILE (READONLY)\" are references only: never emit an
edit action for them."
  "System prompt used by Scalpel when asking the LLM to plan or edit."
  :type 'string
  :group 'scalpel)

;;;###autoload
(defun scalpel-open ()
  "Open the Scalpel agent console."
  (interactive)
  (scalpel-console-open))

;;;###autoload
(defun scalpel-set-backend ()
  "Interactively switch the gptel backend used for future Scalpel requests."
  (interactive)
  (require 'gptel)
  (call-interactively #'gptel-menu))

(provide 'scalpel)

;;; scalpel.el ends here
