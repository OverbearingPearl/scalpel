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
  "Return, in call order, the ert-gwt test names defined in FILE.
Generated symbols carry the defining file in the property
`ert-gwt--defining-file'; collect the interned symbols whose
property equals the truename of FILE and sort them by numeric
suffix, which preserves the order of the deftest calls in the file,
the order the anonymous range lookup relies on.  Return nil when
FILE is nil or no symbol was recorded for it."
  (when file
    (let ((truename (file-truename file))
          (names nil))
      (mapatoms
       (lambda (sym)
         (when (equal (get sym 'ert-gwt--defining-file) truename)
           (push (symbol-name sym) names))))
      (when names
        (sort names
              (lambda (a b)
                (let ((sa (if (string-match "[0-9]+\\'" a)
                              (match-string 0 a)
                            "0"))
                      (sb (if (string-match "[0-9]+\\'" b)
                              (match-string 0 b)
                            "0")))
                  (< (string-to-number sa) (string-to-number sb)))))))))

(scalpel-locate-elisp-register-anonymous-definer
 'ert-gwt-deftest #'scalpel-locate-elisp-ert-gwt--names)

(provide 'scalpel-locate-elisp-ert-gwt)

;;; scalpel-locate-elisp-ert-gwt.el ends here
