;;; scalpel-story-context-test.el --- User-visible context stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the agent's context management:
;; I add a file or directory to the session, I reset it, and I remove
;; one file while the others stay.  Read-capability stories live in
;; scalpel-story-read-test; every clause is a single form, because
;; ert-gwt accepts no clause labels; every THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-agent)

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "added.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path (insert "x\n")))
  (:when (scalpel-agent-context-add path))
  (:then (should (member (file-truename path)
                  (mapcar #'file-truename
                          scalpel-agent--context-files))))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((temp-root (make-temp-file "scalpel-agent-test-" t))
           (temp-file (expand-file-name "foo.txt" temp-root))
           (_ (with-temp-file temp-file (insert "x")))
           (_ (setq scalpel-agent--context-files nil))
           (_ (scalpel-agent-context-add temp-file))))
  (:when (scalpel-agent-context-reset))
  (:then (should (null scalpel-agent--context-files)))
  (:cleanup (setq scalpel-agent--context-files nil)
            (delete-directory temp-root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-agent-test" t))
           (a-file (expand-file-name "a.txt" root))
           (b-file (expand-file-name "b.txt" root)))
          (write-region "" nil a-file nil 'silent)
          (write-region "" nil b-file nil 'silent)
          (setq scalpel-agent--context-files nil))
  (:when (progn
           (scalpel-agent-context-add a-file)
           (scalpel-agent-context-add b-file)
           (scalpel-agent-context-remove a-file)))
  (:then (should (not (member (file-truename a-file)
                              scalpel-agent--context-files))))
  (:then (should (member (file-truename b-file)
                         scalpel-agent--context-files)))
  (:cleanup (setq scalpel-agent--context-files nil)
            (condition-case nil (delete-directory root t) (error nil))))

(provide 'scalpel-story-context-test)

;;; scalpel-story-context-test.el ends here
