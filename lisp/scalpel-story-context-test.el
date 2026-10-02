;;; scalpel-story-context-test.el --- User-visible context stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the agent's context and read
;; capability: I add a file to the session, I ask to read it whole or
;; as one symbol, and a file outside the session is refused loudly.
;; Every clause is a single form, because ert-gwt accepts no clause
;; labels; every THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-agent)

(defvar scalpel-story-context-test-read-report nil
  "Holds the read report captured inside a story.")

(defvar scalpel-story-context-test-refusal nil
  "Holds the refusal message captured inside a story.")

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "notes.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path (insert "hello notes\n"))
          (scalpel-agent-context-add path))
  (:when (setq scalpel-story-context-test-read-report
               (scalpel-agent-file-read path nil)))
  (:then (should (string-search "hello notes"
                                scalpel-story-context-test-read-report)))
  (:cleanup (delete-directory root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "greet.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path
            (insert "(defun greet () \"hi\")\n"))
          (scalpel-agent-context-add path))
  (:when (setq scalpel-story-context-test-read-report
               (scalpel-agent-file-read path "greet")))
  (:then (should (string-search "(defun greet"
                                scalpel-story-context-test-read-report)))
  (:cleanup (delete-directory root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "secret.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path (insert "private\n")))
  (:when (condition-case err
             (scalpel-agent-file-read path nil)
           (user-error
            (setq scalpel-story-context-test-refusal
                  (error-message-string err)))))
  (:then (should (string-search "not in the context"
                                scalpel-story-context-test-refusal)))
  (:cleanup (delete-directory root t)))

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
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-agent--context-files nil))
          (make-directory (expand-file-name "sub" root) t)
          (with-temp-file (expand-file-name "sub/a.el" root)
            (insert "a\n"))
          (with-temp-file (expand-file-name "sub/b.el" root)
            (insert "b\n")))
  (:when (scalpel-agent-context-add (expand-file-name "sub" root)))
  (:then (should (= (length scalpel-agent--context-files) 2)))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(provide 'scalpel-story-context-test)

;;; scalpel-story-context-test.el ends here
