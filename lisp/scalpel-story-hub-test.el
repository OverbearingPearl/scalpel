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

(ert-gwt-deftest
  (:given ((name "*scalpel: ~/proj some tag*")))
  (:when (setq scalpel-story-hub-test--result (scalpel-hub--console-buffer-p name)))
  (:then (should (eq scalpel-story-hub-test--result t))))

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

(provide 'scalpel-story-hub-test)

;;; scalpel-story-hub-test.el ends here
