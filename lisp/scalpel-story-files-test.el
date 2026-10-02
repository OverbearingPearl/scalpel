;;; scalpel-story-files-test.el --- User-visible file-action stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the agent's file-level actions:
;; I create a new file, the agent refuses to overwrite, I delete a
;; file and it leaves the disk, I rename a file and it moves.  Every
;; clause is a single form, because ert-gwt accepts no clause labels;
;; every THEN is one `should'.  Assertions observe only files on disk
;; and returned reports, never internal state.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-agent)

(defvar scalpel-story-files-test-report nil
  "Holds a report string captured inside a story.")

(defvar scalpel-story-files-test-refusal nil
  "Holds a refusal message captured inside a story.")

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-agent--context-files nil))
          (setq default-directory root))
  (:when (setq scalpel-story-files-test-report
               (scalpel-agent-file-create
                (expand-file-name "new.el" root)
                "hello\n")))
  (:then (should (string-search "Created file"
                                scalpel-story-files-test-report)))
  (:then (should (string= "hello\n"
                          (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name "new.el" root))
                            (buffer-string)))))
  (:cleanup (delete-directory root t)
            (setq default-directory (file-name-directory (locate-library "scalpel-test"))
                  scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-agent--context-files nil))
          (setq default-directory root)
          (setq scalpel-story-files-test-refusal nil))
  (:when (condition-case err
             (scalpel-agent-file-create
              (progn
                (with-temp-file (expand-file-name "old.el" root)
                  (insert "old\n"))
                (expand-file-name "old.el" root))
              "new\n")
           (user-error
            (setq scalpel-story-files-test-refusal
                  (error-message-string err)))))
  (:then (should (string-search "already exists"
                                scalpel-story-files-test-refusal)))
  (:then (should (string= "old\n"
                          (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name "old.el" root))
                            (buffer-string)))))
  (:cleanup (delete-directory root t)
            (setq default-directory (file-name-directory (locate-library "scalpel-test"))
                  scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-agent--context-files nil))
          (setq default-directory root)
          (setq scalpel-story-files-test-refusal nil))
  (:when (condition-case err
             (scalpel-agent-file-delete
              (expand-file-name "doomed.el" root))
           (user-error
            (setq scalpel-story-files-test-refusal
                  (error-message-string err)))))
  (:then (should (string-search "no such file"
                                scalpel-story-files-test-refusal)))
  (:cleanup (delete-directory root t)
            (setq default-directory (file-name-directory (locate-library "scalpel-test"))
                  scalpel-agent--context-files nil)))

(provide 'scalpel-story-files-test)

;;; scalpel-story-files-test.el ends here
