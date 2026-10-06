;;; scalpel-story-ignore-test.el --- Ignore-file stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT story for .scalpelignore handling: I add a
;; directory that carries no .scalpelignore, and the files inside it
;; still reach the context.  The temporary root is freshly created, so
;; it holds no ignore file.  Read-capability stories live in
;; scalpel-story-read-test; every clause is a single form, because
;; ert-gwt accepts no clause labels; every THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-agent)

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-ignore-" t))
           (file (expand-file-name "kept.el" root))
           (_ (with-temp-file file (insert "x\n")))
           (_ (setq scalpel-agent--context-files nil))))
  (:when (scalpel-agent-context-add root))
  (:then (should (member (file-truename file)
                         (mapcar #'file-truename
                                 scalpel-agent--context-files))))
  (:cleanup (setq scalpel-agent--context-files nil)
            (delete-directory root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-ignore-" t))
           (kept (expand-file-name "kept.el" root))
           (skipped (expand-file-name "skipped.el" root))
           (ignore-file (expand-file-name ".scalpelignore" root))
           (_ (with-temp-file kept (insert "x\n")))
           (_ (with-temp-file skipped (insert "y\n")))
           (_ (with-temp-file ignore-file (insert "skipped.el\n")))
           (_ (setq scalpel-agent--context-files nil))))
  (:when (scalpel-agent-context-add root))
  (:then (should (not (member (file-truename skipped)
                              (mapcar #'file-truename
                                      scalpel-agent--context-files)))))
  (:then (should (member (file-truename kept)
                         (mapcar #'file-truename
                                 scalpel-agent--context-files))))
  (:cleanup (setq scalpel-agent--context-files nil)
            (delete-directory root t)))

(provide 'scalpel-story-ignore-test)

;;; scalpel-story-ignore-test.el ends here
