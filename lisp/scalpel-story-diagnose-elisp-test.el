;;; scalpel-story-diagnose-elisp-test.el --- Bracket stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for Emacs Lisp bracket diagnosis.  When a
;; planner reply is refused for a bracket defect I want the facts it
;; reports to be measured, not guessed: balanced text stays silent, a
;; stray closer is blamed at its own offset, brackets merely left open
;; are located at the deepest still-open opener, and the indentation
;; heuristic names the opener the missing closer plausibly belongs to
;; only when some opener actually matches that indentation.  No file
;; is touched: the diagnosis works on the reply text itself.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-diagnose-elisp)

(ert-gwt-deftest
  (:given ((text "(defun f (x) (+ x 1))\n")))
  (:when (progn))
  (:then (should (null (scalpel-diagnose-elisp-bracket-report text)))))

(ert-gwt-deftest
  (:given ((text "a)\n")))
  (:when (progn))
  (:then (should (equal (scalpel-diagnose-elisp-bracket-report text)
                        (list :kind 'unbalance :offset 1)))))

(ert-gwt-deftest
  (:given ((text "(defun f (x)\n  body")))
  (:when (progn))
  (:then (should (equal (scalpel-diagnose-elisp-bracket-report text)
                        (list :kind 'unclosed
                              :count 1
                              :offset 0
                              :line 1
                              :col 0
                              :openers (list (list :offset 0
                                                   :line 1
                                                   :col 0
                                                   :depth 1))
                              :likely nil)))))

(ert-gwt-deftest
  (:given ((text "(a\n  (b\n  c")))
  (:when (progn))
  (:then (should (equal (scalpel-diagnose-elisp-bracket-report text)
                        (list :kind 'unclosed
                              :count 2
                              :offset 5
                              :line 2
                              :col 2
                              :openers (list (list :offset 0
                                                   :line 1
                                                   :col 0
                                                   :depth 1)
                                             (list :offset 5
                                                   :line 2
                                                   :col 2
                                                   :depth 2))
                              :likely (list :offset 5
                                            :line 2
                                            :col 2
                                            :depth 2))))))

(ert-gwt-deftest
  (:given ((text "(a\n(b\n   c")))
  (:when (progn))
  (:then (should (equal (scalpel-diagnose-elisp-bracket-report text)
                        (list :kind 'unclosed
                              :count 2
                              :offset 3
                              :line 2
                              :col 0
                              :openers (list (list :offset 0
                                                   :line 1
                                                   :col 0
                                                   :depth 1)
                                             (list :offset 3
                                                   :line 2
                                                   :col 0
                                                   :depth 2))
                              :likely nil)))))

(provide 'scalpel-story-diagnose-elisp-test)

;;; scalpel-story-diagnose-elisp-test.el ends here
