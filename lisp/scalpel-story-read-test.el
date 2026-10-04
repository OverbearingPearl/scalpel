;;; scalpel-story-read-test.el --- User-visible read stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the read capability: a definition
;; too large to read is refused with its true size instead of returned
;; truncated, a whole-file read over the limit is truncated with an
;; honest marker, and binary content is withheld rather than shown.
;; Every clause is a single form; every THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-agent)

(defvar scalpel-story-read-test-report nil
  "Holds a read report captured inside a story.")

(defvar scalpel-story-read-test-refusal nil
  "Holds a read refusal captured inside a story.")

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "big.el" root))
           (scalpel-agent--context-files nil)
           (scalpel-agent-file-read-max-bytes 256))
          (with-temp-file path
            (insert "(defun big () " (make-string 4096 ?x) ")\n"))
          (scalpel-agent-context-add path)
          (setq scalpel-story-read-test-refusal nil))
  (:when (condition-case err
             (scalpel-agent-file-read path "big")
           (user-error
            (setq scalpel-story-read-test-refusal
                  (error-message-string err)))))
  (:then (should (string-search "over the"
                                scalpel-story-read-test-refusal)))
  (:then (should (string-search "read the whole file"
                                scalpel-story-read-test-refusal)))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "wide.el" root))
           (scalpel-agent--context-files nil)
           (scalpel-agent-file-read-max-bytes 8))
          (with-temp-file path (insert "aaaa\nbbbb\ncccc\n"))
          (scalpel-agent-context-add path)
          (setq scalpel-story-read-test-report nil))
  (:when (setq scalpel-story-read-test-report
               (scalpel-agent-file-read path nil)))
  (:then (should (string-search
                  "truncated: showing first 8 of 15 bytes"
                  scalpel-story-read-test-report)))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "blob.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path (insert "a\0b\n"))
          (scalpel-agent-context-add path)
          (setq scalpel-story-read-test-report nil))
  (:when (setq scalpel-story-read-test-report
               (scalpel-agent-file-read path nil)))
  (:then (should (string-search "binary file withheld"
                                scalpel-story-read-test-report)))
  (:then (should (not (string-search "a\0b"
                                     scalpel-story-read-test-report))))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "notes.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path (insert "hello notes\n"))
          (scalpel-agent-context-add path)
          (setq scalpel-story-read-test-report nil))
  (:when (setq scalpel-story-read-test-report
               (scalpel-agent-file-read path nil)))
  (:then (should (string-search "hello notes"
                                scalpel-story-read-test-report)))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "greet.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path
            (insert "(defun greet () \"hi\")\n"))
          (scalpel-agent-context-add path))
  (:when (setq scalpel-story-read-test-report
               (scalpel-agent-file-read path "greet")))
  (:then (should (string-search "(defun greet"
                                scalpel-story-read-test-report)))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (path (expand-file-name "secret.el" root))
           (scalpel-agent--context-files nil))
          (with-temp-file path (insert "private\n")))
  (:when (condition-case err
             (scalpel-agent-file-read path nil)
           (user-error
            (setq scalpel-story-read-test-refusal
                  (error-message-string err)))))
  (:then (should (string-search "not in the context"
                                scalpel-story-read-test-refusal)))
  (:cleanup (delete-directory root t)
            (setq scalpel-agent--context-files nil)))

(provide 'scalpel-story-read-test)

;;; scalpel-story-read-test.el ends here
