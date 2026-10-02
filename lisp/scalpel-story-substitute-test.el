;;; scalpel-story-substitute-test.el --- User-visible substitute stories;; -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the batch substitute capability:
;; I ask for one mechanical change across several files, it lands
;; everywhere; a pattern matching nothing is refused whole, so I learn
;; immediately instead of finding a half-applied change; and a preview
;; shows what would change without touching the files.  Every clause
;; is a single form; every THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-agent)

(defvar scalpel-story-substitute-test-report nil
  "Holds a substitute report captured inside a story.")

(defvar scalpel-story-substitute-test-refusal nil
  "Holds a substitute refusal captured inside a story.")

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (a (expand-file-name "a.el" root))
           (b (expand-file-name "b.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file a (insert "(defun old-name () 1)\n"))
          (with-temp-file b (insert "(defun old-name () 2)\n"))
          (scalpel-agent-context-add a)
          (scalpel-agent-context-add b))
  (:when (setq scalpel-story-substitute-test-report
               (scalpel-agent-file-substitute
                (list a b) "old-name" "new-name")))
  (:then (should (string-search
                  "new-name"
                  (with-temp-buffer
                    (insert-file-contents a)
                    (buffer-string)))))
  (:then (should (string-search
                  "new-name"
                  (with-temp-buffer
                    (insert-file-contents b)
                    (buffer-string)))))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (a (expand-file-name "a.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file a (insert "(defun stays () 1)\n"))
          (scalpel-agent-context-add a)
          (setq scalpel-story-substitute-test-refusal nil))
  (:when (condition-case err
             (scalpel-agent-file-substitute
              (list a) "absent-symbol" "x")
           (error
            (setq scalpel-story-substitute-test-refusal
                  (error-message-string err)))))
  (:then (should scalpel-story-substitute-test-refusal))
  (:then (should (string-search
                  "stays"
                  (with-temp-buffer
                    (insert-file-contents a)
                    (buffer-string)))))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (a (expand-file-name "a.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file a (insert "(defun alpha () 1)\n"))
          (scalpel-agent-context-add a))
  (:when (setq scalpel-story-substitute-test-report
               (scalpel-agent--perl-substitute-preview
                "alpha" "beta" (list a))))
  (:then (should (string-search "Match count: 1"
                                scalpel-story-substitute-test-report)))
  (:then (should (string-search
                  "alpha"
                  (with-temp-buffer
                    (insert-file-contents a)
                    (buffer-string)))))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(provide 'scalpel-story-substitute-test)

;;; scalpel-story-substitute-test.el ends here
