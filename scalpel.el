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
;;
;; The design rests on four linked commitments.  First, an edit must be exact: a
;; wrong or over-broad change is a defect, not an artefact for the user to
;; review.  Second, because the boundary lock makes exactness structural rather
;; than best-effort, applying an edit does not stop for a per-edit confirmation:
;; confirming every hunk would tax every correct edit to guard against a failure
;; the locator already prevents.  Third, an edit that needed no approval must
;; leave a trace: every modification is reported as it lands and the session can
;; be diffed as a whole, so the agent's work is legible after the fact instead
;; of gated before it.  Fourth, visibility alone takes nothing back, so session
;; recording and one-shot rollback are part of the contract rather than optional
;; polish: what shows the change is what undoes it.
;;
;; Planning and file edits need only Emacs and gptel, so they work on every
;; platform Emacs supports.  Shell actions additionally need a command
;; sandbox: `bubblewrap' on Linux, or the deprecated `sandbox-exec' on macOS
;; (treated as experimental).  Without a working sandbox a shell action is
;; refused; nothing else is affected.

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

;;;###autoload
(defun scalpel-open ()
  "Open the Scalpel agent console."
  (interactive)
  (scalpel-console-open))

(defalias 'scalpel-set-backend #'scalpel-llm-select-backend
  "Interactively switch the gptel backend used for future Scalpel requests.")

(defvar scalpel-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'scalpel-open)
    (define-key map (kbd "b") #'scalpel-set-backend)
    map)
  "Keymap for Scalpel package-level commands.

`o' opens the console (`scalpel-open'); `b' switches the gptel
backend (`scalpel-set-backend').

This map carries no default prefix: Emacs reserves most prefix
keys for users and for major modes, so Scalpel does not install a
global binding.  Install one yourself with `global-set-key' using
a prefix of your choice.")

(provide 'scalpel)

;;; scalpel.el ends here
