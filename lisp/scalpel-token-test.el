;;; scalpel-token-test.el --- Tests for scalpel-token -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the token accounting buffer.  No LLM is contacted.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-token)
(require 'scalpel-utils-test)

(defmacro scalpel-token-test-with-clean-state (&rest body)
  "Run BODY with fresh accounting state and the token buffer cleaned up.
The accounting variables are rebound to private values so a test
never disturbs real totals, and the token buffer is killed after
BODY, success or failure."
  (declare (indent 0))
  `(let ((scalpel-token--console-totals (make-hash-table :test 'equal))
         scalpel-token--grand-up
         scalpel-token--grand-down)
     (setq scalpel-token--grand-up 0
           scalpel-token--grand-down 0)
     (unwind-protect
         (progn ,@body)
       (when (get-buffer scalpel-token-buffer-name)
         (with-current-buffer scalpel-token-buffer-name
           (set-buffer-modified-p nil))
         (kill-buffer scalpel-token-buffer-name)))))

(ert-deftest scalpel-token-test-record-accumulates-per-console-and-grand ()
  "One record updates the console's totals and the grand totals."
  (scalpel-token-test-with-clean-state
    (scalpel-token-record "*scalpel: ~/proj*"
                          10 5 (list :system 4 :context 3 :history 2 :instruction 1))
    (scalpel-token-record "*scalpel: ~/proj*"
                          1 2 (list :system 0 :context 1 :history 0 :instruction 0))
    (should (equal (scalpel-token--console-totals "*scalpel: ~/proj*") '(11 7)))
    (should (= scalpel-token--grand-up 11))
    (should (= scalpel-token--grand-down 7))
    (with-current-buffer (get-buffer scalpel-token-buffer-name)
      (ert-info ((format "Buffer:\n%S" (buffer-string)))
        (should (string-match-p "round 10/5" (buffer-string)))
        (should (string-match-p "4/3/2/1" (buffer-string)))
        (should (string-match-p "console 11/7" (buffer-string)))
        (should (string-match-p "ALL 11/7" (buffer-string)))))))

(ert-deftest scalpel-token-test-console-totals-defaults-to-zero ()
  "An unknown console reports (0 0)."
  (scalpel-token-test-with-clean-state
    (should (equal (scalpel-token--console-totals "no-such-console")
                   '(0 0)))))

(ert-deftest scalpel-token-test-open-creates-buffer-with-header ()
  "Opening shows the buffer and installs its header once.
`scalpel-token-open' selects the token buffer in the current window;
the excursion keeps an interactive ERT run from moving the user's
display onto a buffer the test then kills."
  (scalpel-token-test-with-clean-state
    (scalpel-utils-test-with-preserved-windows
      (scalpel-token-open)
      (should (get-buffer scalpel-token-buffer-name))
      (with-current-buffer scalpel-token-buffer-name
        (should (derived-mode-p 'special-mode))
        (should (string-match-p "Scalpel token estimates" (buffer-string)))
        ;; Opening twice must not duplicate the header.
        (scalpel-token-open)
        (should (= (how-many "Scalpel token estimates"
                             (point-min) (point-max))
                   1))))))

(ert-deftest scalpel-token-test-reset-clears-accounting-and-buffer ()
  "Reset zeroes the totals and reprints the header."
  (scalpel-token-test-with-clean-state
    (scalpel-token-record "c" 7 3 (list :system 7 :context 0 :history 0 :instruction 0))
    (scalpel-token-reset)
    (should (= scalpel-token--grand-up 0))
    (should (= scalpel-token--grand-down 0))
    (should (equal (scalpel-token--console-totals "*any*") '(0 0)))
    (with-current-buffer (get-buffer scalpel-token-buffer-name)
      (should (string-match-p "Scalpel token estimates" (buffer-string)))
      (should-not (string-match-p "round 7/3" (buffer-string))))))

(ert-deftest scalpel-token-test-reset-without-buffer-is-safe ()
  "Resetting with no token buffer must not create one."
  (scalpel-token-test-with-clean-state
    (scalpel-token-reset)
    (should-not (get-buffer scalpel-token-buffer-name))))

(provide 'scalpel-token-test)

;;; scalpel-token-test.el ends here
