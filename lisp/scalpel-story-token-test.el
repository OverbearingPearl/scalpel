;;; scalpel-story-token-test.el --- Token accounting stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the token accounting buffer.  I run
;; rounds in two consoles and each console keeps its own cumulative
;; estimate while the grand total spans all consoles; every round
;; appends one readable accounting line; when I reset, the per-console
;; table, the grand totals and the buffer body all clear while the
;; header survives so the next round still prints under it.  Cleanup
;; installs a fresh totals table and kills the accounting buffer so no
;; state leaks between stories.  Every clause is a single form and
;; every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-token)

(defun scalpel-story-token-test-cleanup ()
  "Restore pristine accounting state and remove the token buffer."
  (setq scalpel-token--console-totals (make-hash-table :test 'equal)
        scalpel-token--grand-up 0
        scalpel-token--grand-down 0)
  (when (get-buffer scalpel-token-buffer-name)
    (kill-buffer scalpel-token-buffer-name)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-token--console-totals
                    (make-hash-table :test 'equal)))
           (_ (setq scalpel-token--grand-up 0))
           (_ (setq scalpel-token--grand-down 0))
           (totals nil)))
  (:when (progn
           (scalpel-token-record
            "c1" 10 20
            (list :system 1 :context 2 :history 3 :instruction 4))
           (setq totals (scalpel-token--console-totals "c1"))))
  (:then (should (equal totals (list 10 20))))
  (:cleanup (scalpel-story-token-test-cleanup)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-token--console-totals
                    (make-hash-table :test 'equal)))
           (_ (setq scalpel-token--grand-up 0))
           (_ (setq scalpel-token--grand-down 0))
           (totals nil)))
  (:when (progn
           (scalpel-token-record
            "c1" 10 20
            (list :system 1 :context 2 :history 3 :instruction 4))
           (scalpel-token-record
            "c1" 5 7
            (list :system 1 :context 2 :history 3 :instruction 4))
           (setq totals (scalpel-token--console-totals "c1"))))
  (:then (should (equal totals (list 15 27))))
  (:cleanup (scalpel-story-token-test-cleanup)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-token--console-totals
                    (make-hash-table :test 'equal)))
           (_ (setq scalpel-token--grand-up 0))
           (_ (setq scalpel-token--grand-down 0))))
  (:when (progn
           (scalpel-token-record
            "c1" 10 20
            (list :system 1 :context 2 :history 3 :instruction 4))
           (scalpel-token-record
            "c2" 3 4
            (list :system 1 :context 2 :history 3 :instruction 4))))
  (:then (should (and (= scalpel-token--grand-up 13)
                      (= scalpel-token--grand-down 24))))
  (:cleanup (scalpel-story-token-test-cleanup)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-token--console-totals
                    (make-hash-table :test 'equal)))
           (_ (setq scalpel-token--grand-up 0))
           (_ (setq scalpel-token--grand-down 0))
           (body nil)))
  (:when (progn
           (scalpel-token-record
            "c1" 10 20
            (list :system 1 :context 2 :history 3 :instruction 4))
           (with-current-buffer scalpel-token-buffer-name
             (setq body (buffer-string)))))
  (:then (should (string-search "c1" body)))
  (:then (should (string-search "round 10/20" body)))
  (:cleanup (scalpel-story-token-test-cleanup)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-token--console-totals
                    (make-hash-table :test 'equal)))
           (_ (setq scalpel-token--grand-up 0))
           (_ (setq scalpel-token--grand-down 0))
           (_ (scalpel-token-record
               "c1" 10 20
               (list :system 1 :context 2 :history 3 :instruction 4)))
           (totals nil)
           (body nil)))
  (:when (progn
           (scalpel-token-reset)
           (setq totals (scalpel-token--console-totals "c1"))
           (with-current-buffer scalpel-token-buffer-name
             (setq body (buffer-string)))))
  (:then (should (equal totals (list 0 0))))
  (:then (should (= scalpel-token--grand-up 0)))
  (:then (should (string-search
                  "Scalpel token estimates" body)))
  (:then (should (not (string-search "c1" body))))
  (:cleanup (scalpel-story-token-test-cleanup)))

(provide 'scalpel-story-token-test)

;;; scalpel-story-token-test.el ends here
