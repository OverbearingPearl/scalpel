;;; scalpel-story-files-test.el --- User-visible file-action stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the agent's file-level actions:
;; renaming a file succeeds, renaming onto an existing file is
;; refused, and deleting a context file removes it from disk and
;; context.  Written with `ert-gwt-deftest': no test name, no
;; argument list, one binding list per :given, one action in :when,
;; one assertion per :then, cleanup in :cleanup.

;;; Code:

(require 'ert)
(require 'ert-gwt)
(require 'scalpel-agent)

;; Story A: renaming old.el to new.el renames the file on disk.
(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-files-" t))
           (scalpel-agent--context-files nil)
           (old (expand-file-name "old.el" root))
           (new (expand-file-name "new.el" root))
           (result nil))
          (with-temp-file old (insert "old\n")))
  (:when (setq result (scalpel-agent-file-rename old new)))
  (:then (should (string-match-p "\\`.*Renamed.*\\'" result)))
  (:then (should (file-exists-p new)))
  (:then (should (string= "old\n"
                          (with-temp-buffer
                            (insert-file-contents new)
                            (buffer-string)))))
  (:then (should (not (file-exists-p old))))
  (:cleanup (condition-case nil
                (delete-directory root t)
              (error nil))))

;; Story B: renaming onto an existing file is refused.
(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-files-" t))
           (scalpel-agent--context-files nil)
           (old (expand-file-name "old.el" root))
           (other (expand-file-name "other.el" root))
           (refusal nil))
          (with-temp-file old (insert "old\n"))
          (with-temp-file other (insert "other\n")))
  (:when (setq refusal
               (condition-case err
                   (progn (scalpel-agent-file-rename old other) nil)
                 (user-error (error-message-string err)))))
  (:then (should (string-search "already exists"
                                (or refusal ""))))
  (:then (should (file-exists-p old)))
  (:then (should (file-exists-p other)))
  (:then (should (string= "old\n"
                          (with-temp-buffer
                            (insert-file-contents old)
                            (buffer-string)))))
  (:then (should (string= "other\n"
                          (with-temp-buffer
                            (insert-file-contents other)
                            (buffer-string)))))
  (:cleanup (condition-case nil
                (delete-directory root t)
              (error nil))))

;; Story C: deleting a context file removes it from disk and context.
(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-files-" t))
           (scalpel-agent--context-files nil)
           (doomed (expand-file-name "doomed.el" root))
           (truename nil)
           (report nil))
          (with-temp-file doomed (insert "bye\n"))
          (setq truename (file-truename doomed))
          (setq scalpel-agent--context-files (list truename)))
  (:when (setq report (scalpel-agent-file-delete truename)))
  (:then (should (string-match-p "\\`.*Deleted file.*\\'" report)))
  (:then (should (not (file-exists-p doomed))))
  (:then (should (not (member truename scalpel-agent--context-files))))
  (:cleanup (condition-case nil
                (delete-directory root t)
              (error nil))
            (setq scalpel-agent--context-files nil)))

(provide 'scalpel-story-files-test)

;;; scalpel-story-files-test.el ends here
