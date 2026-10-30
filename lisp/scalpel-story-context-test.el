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

(ert-gwt-deftest
  (:given ((root (file-name-as-directory (make-temp-file "scalpel-gwt-" t)))
           (sub (expand-file-name "sub" root))
           (a (expand-file-name "a.txt" sub))
           (b (expand-file-name "b.txt" sub))
           (c (expand-file-name "c.txt" root)))
          (make-directory sub t)
          (with-temp-file a (insert "a"))
          (with-temp-file b (insert "b"))
          (with-temp-file c (insert "c"))
          (setq scalpel-agent--context-files nil)
          (scalpel-agent-context-add root))
  (:when (scalpel-agent-context-remove (expand-file-name "sub/*.txt" root)))
  (:then (should (not (member (file-truename a) scalpel-agent--context-files))))
  (:then (should (not (member (file-truename b) scalpel-agent--context-files))))
  (:then (should (member (file-truename c) scalpel-agent--context-files)))
  (:cleanup (progn
              (setq scalpel-agent--context-files nil)
              (delete-directory root t))))

(ert-gwt-deftest
  (:given ((root (make-temp-file "context-gwt-" t))
           (path (expand-file-name "readonly.txt" root))
           (scalpel-agent--context-files nil)
           (scalpel-agent--context-readonly-files nil))
          (with-temp-file path (insert "read only")))
  (:when (scalpel-agent-context-add-readonly path))
  (:then (should (member (file-truename path)
                         (mapcar #'file-truename
                                 scalpel-agent--context-files))))
  (:then (should (member (file-truename path)
                         (mapcar #'file-truename
                                 scalpel-agent--context-readonly-files))))
  (:cleanup (setq scalpel-agent--context-files nil
                  scalpel-agent--context-readonly-files nil)
            (delete-directory root t)))

(ert-gwt-deftest
  (:given ((temp-dir (make-temp-file "context-gwt-" t))
           (temp-file (expand-file-name "upgrade.txt" temp-dir))
           (scalpel-agent--context-files nil)
           (scalpel-agent--context-readonly-files nil))
          (with-temp-file temp-file (insert "upgrade"))
          (scalpel-agent-context-add-readonly temp-file))
  (:when (scalpel-agent-context-add temp-file))
  (:then (should (member (file-truename temp-file)
                         (mapcar #'file-truename
                                 scalpel-agent--context-files))))
  (:then (should (not (member (file-truename temp-file)
                              scalpel-agent--context-readonly-files))))
  (:cleanup (setq scalpel-agent--context-files nil
                  scalpel-agent--context-readonly-files nil)
            (delete-directory temp-dir t)))

(ert-gwt-deftest
  (:given ((temp-dir (make-temp-file "context-gwt-" t))
           (temp-file (expand-file-name "removeme.txt" temp-dir))
           (scalpel-agent--context-files nil)
           (scalpel-agent--context-readonly-files nil))
          (with-temp-file temp-file (insert "remove me"))
          (scalpel-agent-context-add-readonly temp-file))
  (:when (scalpel-agent-context-remove temp-file))
  (:then (should (not (member (file-truename temp-file)
                              (mapcar #'file-truename
                                      scalpel-agent--context-files)))))
  (:then (should (not (member (file-truename temp-file)
                              (mapcar #'file-truename
                                      scalpel-agent--context-readonly-files)))))
  (:cleanup (setq scalpel-agent--context-files nil
                  scalpel-agent--context-readonly-files nil)
            (delete-directory temp-dir t)))

(ert-gwt-deftest
  (:given ((temp-dir (make-temp-file "context-gwt-" t))
           (temp-file (expand-file-name "reset.txt" temp-dir))
           (_ (with-temp-file temp-file (insert "reset")))
           (_ (setq scalpel-agent--context-files nil
                    scalpel-agent--context-readonly-files nil))
           (_ (scalpel-agent-context-add-readonly temp-file))))
  (:when (scalpel-agent-context-reset))
  (:then (should (null scalpel-agent--context-files)))
  (:then (should (null scalpel-agent--context-readonly-files)))
  (:cleanup (setq scalpel-agent--context-files nil
                  scalpel-agent--context-readonly-files nil)
            (delete-directory temp-dir t)))

(ert-gwt-deftest
  (:given ((temp-dir (make-temp-file "context-gwt-" t))
           (file-gone (expand-file-name "gone.txt" temp-dir))
           (file-kept (expand-file-name "kept.txt" temp-dir))
           (_ (with-temp-file file-gone (insert "gone")))
           (_ (with-temp-file file-kept (insert "kept")))
           (_ (setq scalpel-agent--context-files nil
                    scalpel-agent--context-readonly-files nil))
           (_ (scalpel-agent-context-add-readonly file-gone))
           (_ (scalpel-agent-context-add-readonly file-kept))
           (_ (delete-file file-gone))))
  (:when (scalpel-agent-context-prune-missing))
  (:then (should (not (member (file-truename file-gone)
                              (mapcar #'file-truename
                                      scalpel-agent--context-readonly-files)))))
  (:then (should (member (file-truename file-kept)
                         (mapcar #'file-truename
                                 scalpel-agent--context-readonly-files))))
  (:then (should (member (file-truename file-kept)
                         (mapcar #'file-truename
                                 scalpel-agent--context-files))))
  (:cleanup (setq scalpel-agent--context-files nil
                  scalpel-agent--context-readonly-files nil)
            (delete-directory temp-dir t)))

(provide 'scalpel-story-context-test)

;;; scalpel-story-context-test.el ends here
