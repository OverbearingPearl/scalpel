;;; scalpel-story-review-test.el --- Review buffer stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the session review listing.  I
;; look at the changed-file list and see my records grouped by file
;; in the order I first touched them; when I walk the listing with
;; TAB I land on the next block and know the point sits in one; and
;; when I read a diff hunk I get its context, removed and added
;; lines parsed as separate operations.  Every clause is a single
;; form and every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-review)

(defvar scalpel-story-review-test--buffer nil
  "Holds the review test buffer across clauses.")

(ert-gwt-deftest
  (:given ((record-a (list :file "/tmp/a.el" :old-text "" :new-text "x"))
           (record-b (list :file "/tmp/b.el" :old-text "" :new-text "y"))
           (record-c (list :file "/tmp/a.el" :old-text "x" :new-text "z"))
           (groups nil)))
  (:when (progn
           (setq groups (scalpel-review--group-by-file
                         (list record-a record-b record-c)))))
  (:then (should (equal groups
                        (list (cons "/tmp/a.el"
                                    (list record-a record-c))
                              (cons "/tmp/b.el"
                                    (list record-b)))))))

(ert-gwt-deftest
  (:given ((ops nil)))
  (:when (progn
           (with-temp-buffer
             (insert "@@ -1,2 +1,2 @@\n context\n-removed\n+added\n")
             (setq ops (scalpel-review--hunk-operations
                        (point-min) (point-max))))))
  (:then (should (equal ops
                        (list (list 'context "context\n")
                              (list 'removed "removed\n")
                              (list 'added "added\n"))))))

(ert-gwt-deftest
  (:given ((_ (let ((buffer (generate-new-buffer " *review-story*")))
                (with-current-buffer buffer
                  (insert "head\nblock\ntail\n")
                  (let ((inhibit-read-only t))
                    (put-text-property
                     (+ (point-min) 5) (+ (point-min) 10)
                     'scalpel-review-record :rec)))
                (setq scalpel-story-review-test--buffer buffer)))
           (result nil)))
  (:when (progn
           (with-current-buffer scalpel-story-review-test--buffer
             (goto-char (point-min))
             (scalpel-review-next-block)
             (setq result
                   (scalpel-review--block-button-p (point))))))
  (:then (should (eq result :rec)))
  (:cleanup (when (buffer-live-p scalpel-story-review-test--buffer)
              (kill-buffer scalpel-story-review-test--buffer))
            (setq scalpel-story-review-test--buffer nil)))

(ert-gwt-deftest
  (:given ((_ (let ((buffer (generate-new-buffer " *review-story*")))
                (with-current-buffer buffer
                  (insert "plain\n"))
                (setq scalpel-story-review-test--buffer buffer)))
           (result nil)))
  (:when (progn
           (with-current-buffer scalpel-story-review-test--buffer
             (goto-char (point-min))
             (setq result (scalpel-review--block-button-p (point))))))
  (:then (should (null result)))
  (:cleanup (when (buffer-live-p scalpel-story-review-test--buffer)
              (kill-buffer scalpel-story-review-test--buffer))
            (setq scalpel-story-review-test--buffer nil)))

(provide 'scalpel-story-review-test)

;;; scalpel-story-review-test.el ends here
