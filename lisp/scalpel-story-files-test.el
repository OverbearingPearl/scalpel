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

(defvar scalpel-story-files-test-refusal nil)

;; Story A: renaming old.el to new.el renames the file on disk.
(ert-deftest scalpel-story-files-test-rename-succeeds ()
  (let ((root (make-temp-file "scalpel-story-files-" t))
        (scalpel-story-files-test-report nil)
        (scalpel-story-files-test-refusal nil)
        (scalpel-agent--context-files nil))
    (unwind-protect
        (let ((old (expand-file-name "old.el" root))
              (new (expand-file-name "new.el" root)))
          (with-temp-file old (insert "old\n"))
          ;; Given a temp root holding old.el with content old newline
          ;; and scalpel-agent--context-files nil, when the planner
          ;; renames old.el to new.el with scalpel-agent-file-rename.
          (should (equal (scalpel-agent-file-rename old new)
                         (setq scalpel-story-files-test-report
                               (scalpel-agent-file-rename old new))))
          ;; Then the report mentions Renamed.
          (should (string-match-p "\\`.*Renamed.*\\'" scalpel-story-files-test-report))
          ;; And new.el exists on disk with the old content.
          (should (file-exists-p new))
          (should (string= (with-temp-buffer (insert-file-contents new)
                                             (buffer-string))
                           "old\n"))
          ;; And old.el no longer exists.
          (should-not (file-exists-p old)))
      (delete-directory root t))))

;; Story B: renaming onto an existing file is refused.
(ert-deftest scalpel-story-files-test-rename-refuses-existing-target ()
  (let ((root (make-temp-file "scalpel-story-files-" t))
        (scalpel-story-files-test-report nil)
        (scalpel-story-files-test-refusal nil))
    (unwind-protect
        (let ((old (expand-file-name "old.el" root))
              (other (expand-file-name "other.el" root)))
          (with-temp-file old (insert "old\n"))
          (with-temp-file other (insert "other\n"))
          ;; Given a temp root holding both old.el and other.el, when
          ;; the rename to the already-existing other.el is attempted
          ;; inside a condition-case capturing user-error.
          (setq scalpel-story-files-test-refusal
                (condition-case err
                    (progn (scalpel-agent-file-rename old other) nil)
                  (user-error (cadr err))))
          ;; Then the refusal message mentions already exists.
          (should (string-match-p "\\`.*already exists.*\\'"
                                  (or scalpel-story-files-test-refusal "")))
          ;; And both files still exist on disk unchanged.
          (should (file-exists-p old))
          (should (file-exists-p other))
          (should (string= (with-temp-buffer (insert-file-contents old)
                                             (buffer-string))
                           "old\n"))
          (should (string= (with-temp-buffer (insert-file-contents other)
                                             (buffer-string))
                           "other\n")))
      (delete-directory root t))))

;; Story C: deleting a context file removes it from disk and context.
(ert-deftest scalpel-story-files-test-delete-removes-from-context ()
  (let ((root (make-temp-file "scalpel-story-files-" t))
        (scalpel-story-files-test-report nil)
        (scalpel-story-files-test-refusal nil)
        (scalpel-agent--context-files nil))
    (unwind-protect
        (let* ((doomed (expand-file-name "doomed.el" root))
               (path doomed))
          (with-temp-file doomed (insert "bye\n"))
          (setq scalpel-agent--context-files (list path))
          ;; Given a temp root holding doomed.el with content bye
          ;; newline and scalpel-agent--context-files nil with
          ;; doomed.el added to the context, when the planner deletes
          ;; doomed.el with scalpel-agent-file-delete.
          (setq scalpel-story-files-test-report
                (scalpel-agent-file-delete path))
          ;; Then the report mentions Deleted file.
          (should (string-match-p "\\`.*Deleted file.*\\'" scalpel-story-files-test-report))
          ;; And the file is gone from disk.
          (should-not (file-exists-p doomed))
          ;; And scalpel-agent--context-files no longer contains the path.
          (should-not (member path scalpel-agent--context-files)))
      (condition-case nil (delete-directory root t) (error nil))
      (setq scalpel-agent--context-files nil))))

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
