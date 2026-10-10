;;; scalpel-story-edit-test.el --- User-visible edit stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the block-edit replacement guard:
;; a model reply that merely echoes a prompt-scaffold label such as
;; "Current block:" must not be accepted as a replacement, while a
;; genuine single-definition reply still is.  Every clause is a single
;; form; every THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-agent)

(defvar scalpel-story-edit-test-replacement nil
  "Holds a replacement decision captured inside a story.")

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "edit.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path
            (insert "(defun greet () \"hi\")\n"))
          (setq scalpel-story-edit-test-replacement nil))
  (:when (setq scalpel-story-edit-test-replacement
               (scalpel-agent--usable-replacement
                path "Current block:\n\n")))
  (:then (should (equal scalpel-story-edit-test-replacement
                        "Current block:\n\n")))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "edit.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path
            (insert "(defun greet () \"hi\")\n"))
          (setq scalpel-story-edit-test-replacement nil))
  (:when (setq scalpel-story-edit-test-replacement
               (scalpel-agent--usable-replacement
                path "(defun farewell () \"bye\")\n")))
  (:then (should (string-search "farewell"
                                scalpel-story-edit-test-replacement)))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(provide 'scalpel-story-edit-test)

;;; scalpel-story-edit-test.el ends here
