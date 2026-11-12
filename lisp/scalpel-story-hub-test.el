;;; scalpel-story-hub-test.el --- Hub management stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the multi-session hub.  The
;; console-name matcher admits names fitting the console format and
;; refuses others; the hub lists a live console, selects it, renames
;; it to a mnemonic name still matching the format, and shows the
;; last instruction blank when lineage has no entry.  Every console
;; and hub buffer created here is killed in an `unwind-protect'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-hub)

(defvar scalpel-story-hub-test--result
  "Shared result slot used by the tests above, declared to silence byte-compile free-variable warnings.")

(ert-gwt-deftest
  (:given ((name "*scalpel: ~/proj*")))
  (:when (setq scalpel-story-hub-test--result (scalpel-hub--console-buffer-p name)))
  (:then (should (eq scalpel-story-hub-test--result t))))

(ert-gwt-deftest
  (:given ((name "*other: ~/proj*")))
  (:when (setq scalpel-story-hub-test--result (scalpel-hub--console-buffer-p name)))
  (:then (should (null scalpel-story-hub-test--result))))

(ert-deftest scalpel-story-hub-test--console-buffer-p-accepts-uniqueness-suffix ()
  (unwind-protect
      (with-current-buffer (get-buffer-create "*scalpel: ~/proj*<2>")
        (setq scalpel-story-hub-test--result
              (scalpel-hub--console-buffer-p (buffer-name (current-buffer))))
        (should (eq scalpel-story-hub-test--result t)))
    (condition-case nil
        (kill-buffer "*scalpel: ~/proj*<2>")
      (file-error nil))))

(ert-gwt-deftest
  (:given ((name "*scalpel: ~/proj some tag*")))
  (:when (setq scalpel-story-hub-test--result (scalpel-hub--console-buffer-p name)))
  (:then (should (eq scalpel-story-hub-test--result t))))

(ert-gwt-deftest
  (:given ((buf-a (get-buffer-create "*scalpel: ~/proj alpha*"))
           (buf-b (get-buffer-create "*scalpel: ~/proj beta*")))
   :after-setup
   (with-current-buffer buf-a
     (setq scalpel-console--root "~/proj"))
   (with-current-buffer buf-b
     (setq scalpel-console--root "~/proj")))
  (:when (unwind-protect
             (let* ((entries (scalpel-hub--entries))
                    (row-a (seq-find
                            (lambda (row)
                              (string= (car row)
                                       "*scalpel: ~/proj alpha*"))
                            entries))
                    (row-b (seq-find
                            (lambda (row)
                              (string= (car row)
                                       "*scalpel: ~/proj beta*"))
                            entries)))
               (setq scalpel-story-hub-test--result
                     (and row-a row-b
                          (list :root-a (aref (nth 1 row-a) 0)
                                :root-b (aref (nth 1 row-b) 0)))))
           (condition-case nil (kill-buffer "*scalpel: ~/proj alpha*") (error nil))
           (condition-case nil (kill-buffer "*scalpel: ~/proj beta*") (error nil))))
  (:then (should scalpel-story-hub-test--result)
         (should (string=
                  (plist-get scalpel-story-hub-test--result :root-a)
                  "*scalpel: ~/proj alpha*"))
         (should (string=
                  (plist-get scalpel-story-hub-test--result :root-b)
                  "*scalpel: ~/proj beta*"))))

(ert-gwt-deftest
  (:given ((buf (generate-new-buffer "*scalpel: ~/scalpel-story-hub-test*")))
          (with-current-buffer buf
            (setq scalpel-console--root "~/scalpel-story-hub-test")))
  (:when (setq scalpel-story-hub-test--result
               (cl-find-if
                (lambda (b)
                  (equal (buffer-name b)
                         "*scalpel: ~/scalpel-story-hub-test*"))
                (scalpel-hub--consoles))))
  (:then (should (equal scalpel-story-hub-test--result buf))))

(ert-gwt-deftest
  (:given ((buf (generate-new-buffer "*scalpel: ~/scalpel-story-hub-test*")))
          (with-current-buffer buf
            (setq scalpel-console--root "~/scalpel-story-hub-test")
            (rename-buffer (scalpel-console--buffer-name
                            scalpel-console--root "tag"))))
  (:when (setq scalpel-story-hub-test--result
               (buffer-name buf)))
  (:then (should (equal scalpel-story-hub-test--result "*scalpel: ~/scalpel-story-hub-test tag*"))))

(ert-gwt-deftest
  (:given ((last (scalpel-hub--last-instruction "*scalpel: nowhere*"))))
  (:when (setq scalpel-story-hub-test--result last))
  (:then (should (null scalpel-story-hub-test--result))))

(ert-deftest scalpel-story-hub-test-root-column-width ()
  "The hub Root column widens to fit a very long console root path.
After opening the hub with `scalpel-hub-open' the hub buffer's
`tabulated-list-format' has a Root column entry whose width is at
least as large as the `string-width' of the Root cell text actually
rendered for that console's row reported by `scalpel-hub--entries'
\(which may be an abbreviated path when the root is unique), and at
least the base width 30."
  (let* ((root (concat "/tmp/very/long/root/"
                       (make-string 100 ?a)))
         (console (get-buffer-create (concat "*scalpel: " root "*")))
         hub-buf entry cell)
    (unwind-protect
        (progn
          (with-current-buffer console
            (setq-local scalpel-console--root root)
            (scalpel-console-mode)
            (scalpel-hub-open))
          (setq hub-buf (get-buffer "*scalpel-hub*"))
          (should hub-buf)
          ;; `tabulated-list-format' is a vector of column specs;
          ;; each spec is a list whose car is the column name.
          (setq entry (seq-find (lambda (col)
                                  (equal (car col) "Root"))
                                (buffer-local-value
                                 'tabulated-list-format hub-buf)))
          (should entry)
          ;; Rows are keyed by the console's buffer name; the row
          ;; vector lives in the cadr of the entry.
          (setq cell
                (let ((row (seq-find
                            (lambda (e)
                              (equal (car e)
                                     (buffer-name console)))
                            (with-current-buffer hub-buf
                              (scalpel-hub--entries)))))
                  (should row)
                  (aref (cadr row) 0)))
          (ert-info ((format "root: %S, rendered cell: %S, Root column entry: %S"
                             root cell entry))
            (should (numberp (nth 1 entry)))
            (should (>= (nth 1 entry) (string-width cell)))
            (should (>= (nth 1 entry) 30))))
      (when (buffer-live-p hub-buf)
        (kill-buffer hub-buf))
      (when (buffer-live-p console)
        (kill-buffer console)))))

(ert-deftest scalpel-story-hub-test-refresh-preserves-point-console-identity ()
  (let (hub-buf
        (tmp-root (make-temp-file "scalpel-hub-test-root" t))
        (buf-a (get-buffer-create "*scalpel: a*"))
        (buf-b (get-buffer-create "*scalpel: b*")))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (scalpel-console-mode)
            (setq-local scalpel-console--root tmp-root))
          (with-current-buffer buf-b
            (scalpel-console-mode)
            (setq-local scalpel-console--root tmp-root))
          (with-current-buffer buf-a
            (scalpel-hub-open))
          (setq hub-buf (get-buffer "*scalpel-hub*"))
          (ert-info ("move point to the row whose entry id is buf-b's buffer name")
            (with-current-buffer hub-buf
              (let ((row (seq-position
                          tabulated-list-entries
                          (buffer-name buf-b)
                          (lambda (entry id) (equal (car entry) id)))))
                (should row)
                (goto-char (point-min))
                (forward-line row))))
          (with-current-buffer hub-buf
            (scalpel-hub-refresh)
            (should (equal (tabulated-list-get-id) (buffer-name buf-b)))))
      (when (buffer-live-p hub-buf) (kill-buffer hub-buf))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b))
      (condition-case nil
          (delete-directory tmp-root t)
        (file-error nil)))))

(provide 'scalpel-story-hub-test)

;;; scalpel-story-hub-test.el ends here
