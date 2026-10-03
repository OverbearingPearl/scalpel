;;; scalpel-token-test.el --- Stories for token accounting -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the token accounting buffer:
;; I run rounds and see per-console and grand totals accumulate, I
;; reset and everything returns to zero.  Assertions observe only the
;; accounting variables and the accounting buffer, never internals of
;; other modules.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-token)

(ert-gwt-deftest
  (:given ((scalpel-token--console-totals (make-hash-table :test 'equal))
           (scalpel-token--grand-up 0)
           (scalpel-token--grand-down 0)))
  (:when (scalpel-token-record
          "story-console" 10 20
          (list :system 5 :context 5 :history 0 :instruction 0)))
  (:then (should (equal '(10 20)
                        (gethash "story-console"
                                 scalpel-token--console-totals))))
  (:then (should (and (= 10 scalpel-token--grand-up)
                      (= 20 scalpel-token--grand-down))))
  (:then (should (bufferp (get-buffer scalpel-token-buffer-name))))
  (:then (should (string-search "story-console"
                                (with-current-buffer
                                    scalpel-token-buffer-name
                                  (buffer-string)))))
  (:cleanup (when (get-buffer scalpel-token-buffer-name)
              (kill-buffer scalpel-token-buffer-name))))

(ert-gwt-deftest
  (:given ((scalpel-token--console-totals (make-hash-table :test 'equal))
           (scalpel-token--grand-up 0)
           (scalpel-token--grand-down 0)))
  (:when (progn
           (scalpel-token-record
            "console-a" 10 10
            (list :system 0 :context 0 :history 0 :instruction 0))
           (scalpel-token-record
            "console-a" 5 5
            (list :system 0 :context 0 :history 0 :instruction 0))
           (scalpel-token-record
            "console-b" 1 2
            (list :system 0 :context 0 :history 0 :instruction 0))))
  (:then (should (equal '(15 15)
                        (gethash "console-a"
                                 scalpel-token--console-totals))))
  (:then (should (equal '(1 2)
                        (gethash "console-b"
                                 scalpel-token--console-totals))))
  (:then (should (= 16 scalpel-token--grand-up)))
  (:then (should (= 17 scalpel-token--grand-down)))
  (:cleanup (when (get-buffer scalpel-token-buffer-name)
              (kill-buffer scalpel-token-buffer-name))))

(ert-gwt-deftest
  (:given ((scalpel-token--console-totals (make-hash-table :test 'equal))
           (scalpel-token--grand-up 0)
           (scalpel-token--grand-down 0)))
  (:when (progn
           (scalpel-token-record
            "story-console" 3 4
            (list :system 0 :context 0 :history 0 :instruction 0))
           (scalpel-token-reset)))
  (:then (should (null (gethash "story-console"
                                 scalpel-token--console-totals))))
  (:then (should (and (zerop scalpel-token--grand-up)
                      (zerop scalpel-token--grand-down))))
  (:then (should (bufferp (get-buffer scalpel-token-buffer-name))))
  (:cleanup (when (get-buffer scalpel-token-buffer-name)
              (kill-buffer scalpel-token-buffer-name))))

(provide 'scalpel-token-test)

;;; scalpel-token-test.el ends here
