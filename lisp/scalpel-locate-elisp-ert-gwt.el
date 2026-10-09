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
`read'; for each (`ert-gwt-deftest' ...) call pass the clause list
\(cdr form) to `ert-gwt-name-from-clauses' with the fixed prefix
\"ert-gwt-deftest\" and a single hash table (with `equal' key
comparison, since keys are clause list objects that only `equal'
can match) shared by all forms of the file, so the naming
rule—including the suffix numbering for duplicated blocks, which
is incremented across the whole file—is applied by the single
documented source of truth that ert-gwt exposes for external
tools like this locator.  The prefix does not depend on FILE, so
generated names are determined solely by clause content and stay
deterministic regardless of the file's own name.  No evaluation,
no loading, and no session state is touched.  Return nil when
FILE is nil or the file contains no `ert-gwt-deftest' call."
  (when (and file (file-readable-p file))
    (let ((names nil)
          (seen (make-hash-table :test 'equal)))
      (with-temp-buffer
        (insert-file-contents file)
        (condition-case nil
            (let (form)
              (while t
                (setq form (read (current-buffer)))
                (when (and (consp form)
                           (eq (car form) 'ert-gwt-deftest))
                  (push (ert-gwt-name-from-clauses
                         "ert-gwt-deftest"
                         (cdr form)
                         seen)
                        names))))
          (end-of-file nil)))
      (nreverse names))))

(scalpel-locate-elisp-register-anonymous-definer
   'ert-gwt-deftest #'scalpel-locate-elisp-ert-gwt--names)

(provide 'scalpel-locate-elisp-ert-gwt)

;;; scalpel-locate-elisp-ert-gwt.el ends here
