;;; scalpel-execute-elisp.el --- Emacs Lisp execute provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Execute-side rules for Emacs Lisp.  A deletion range reported by the
;; locator covers the definition's own form and nothing else; an
;; autoload cookie sits on its own line above the form it autoloads, so
;; a range that stopped at the form would leave the cookie behind as an
;; orphan comment autoloading a symbol the file no longer defines.
;; This module answers only with the position a deletion should start
;; at; the deletion itself, and the blank-line reconciliation around
;; it, stay in `scalpel-execute'.

;;; Code:

(defconst scalpel-execute-elisp--cookie-regexp "^;;;###"
  "Regexp matching an autoload cookie line at the beginning of a line.
`;;;###autoload' is the common spelling; the form that carries an
argument -- `;;;###autoload (push ...)' -- starts the same way, so the
pattern stops at the marker rather than naming one spelling.")

(defun scalpel-execute-elisp--deletion-start (beg)
  "Return the position a deletion of the definition at BEG should start at.
Every `;;;###' cookie line directly above BEG is carried into the
deletion, so the cookie goes with the definition it autoloads.  A
blank line, a different comment, code, or the beginning of the buffer
ends the run.  A BEG that does not begin its own line comes back
unchanged: there is then no line above it to carry."
  (save-excursion
    (goto-char beg)
    (if (not (bolp))
        beg
      (while (and (> (point) (point-min))
                  (save-excursion
                    (forward-line -1)
                    (looking-at-p scalpel-execute-elisp--cookie-regexp)))
        (forward-line -1))
      (point))))

(provide 'scalpel-execute-elisp)

;;; scalpel-execute-elisp.el ends here
