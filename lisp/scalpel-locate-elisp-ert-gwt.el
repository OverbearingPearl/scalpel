;;; scalpel-locate-elisp-ert-gwt.el --- Locator adapter for ert-gwt tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; `ert-gwt-deftest' generates test names (ert-gwt-deftest-N) at macro
;; expansion time, so they never appear in the source text and the
;; literal name search cannot see them.  This file, on the Scalpel
;; side, registers that definer with the anonymous-definer registry,
;; so ert-gwt itself stays free of Scalpel-specific code: the adapter
;; reads only the file-local property that `ert-gwt-deftest' expansion
;; already records on each generated symbol.

;;; Code:

(require 'scalpel-locate-elisp)
(require 'ert-gwt)

(defun scalpel-locate-elisp-ert-gwt--names (file)
  "Return, in source order, the ert-gwt test names defined in FILE.
Read FILE into a temp buffer and walk its top-level forms with
`read'; for each (`ert-gwt-deftest' ...) call synthesize the symbol
name ert-gwt-deftest-N, where N is the 1-based ordinal of the call
in the file, matching the numbering the list-symbols fallback
uses.  This derives the names from the source itself, so the file
does not need to have been loaded and the result survives the test
runner unloading features.  Return nil when FILE is nil or the
file contains no `ert-gwt-deftest' call."
  (when (and file (file-readable-p file))
    (let ((names nil)
          (count 0))
      (with-temp-buffer
        (insert-file-contents file)
        (condition-case nil
            (let (form)
              (while t
                (setq form (read (current-buffer)))
                (when (and (consp form)
                           (eq (car form) 'ert-gwt-deftest))
                  (setq count (1+ count))
                  (push (format "ert-gwt-deftest-%d" count) names))))
          (end-of-file nil)))
      (nreverse names))))

(scalpel-locate-elisp-register-anonymous-definer
 'ert-gwt-deftest #'scalpel-locate-elisp-ert-gwt--names)

(provide 'scalpel-locate-elisp-ert-gwt)

;;; scalpel-locate-elisp-ert-gwt.el ends here
