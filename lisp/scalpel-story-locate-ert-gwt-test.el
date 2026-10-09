;;; scalpel-story-locate-ert-gwt-test.el --- ert-gwt adapter stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the ert-gwt locator adapter.  I
;; read a source file through the adapter and get the generated test
;; names in source order; when the file holds duplicated blocks the
;; names carry the suffix numbering; when the file holds no
;; ert-gwt-deftest I get nil; and the adapter is registered for
;; ert-gwt-deftest in the anonymous-definer registry.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-locate-elisp-ert-gwt)

(defvar scalpel-story-locate-ert-gwt-test--file nil
  "Holds the temp source file path across clauses.")

(ert-gwt-deftest
  (:given ((_ (let ((file (make-temp-file "gwt-story-" nil ".el")))
                (with-temp-file file
                  (insert "(ert-gwt-deftest (:given ((x 1)))\n"
                          " (:when (progn (setq x (+ x 1))))\n"
                          " (:then (should (eq x 2))))\n"))
                (setq scalpel-story-locate-ert-gwt-test--file file)))
           (names nil)))
  (:when (progn
           (setq names
                 (scalpel-locate-elisp-ert-gwt--names
                  scalpel-story-locate-ert-gwt-test--file))))
  (:then (should (equal names (list "ert-gwt-deftest-69ca97a0"))))
  (:cleanup (when (and scalpel-story-locate-ert-gwt-test--file
                       (file-exists-p
                        scalpel-story-locate-ert-gwt-test--file))
              (delete-file scalpel-story-locate-ert-gwt-test--file))
            (setq scalpel-story-locate-ert-gwt-test--file nil)))

(ert-gwt-deftest
  (:given ((_ (let ((file (make-temp-file "gwt-story-" nil ".el")))
                (with-temp-file file
                  (insert "(defun unrelated () 1)\n"))
                (setq scalpel-story-locate-ert-gwt-test--file file)))
           (names nil)))
  (:when (progn
           (setq names
                 (scalpel-locate-elisp-ert-gwt--names
                  scalpel-story-locate-ert-gwt-test--file))))
  (:then (should (null names)))
  (:cleanup (when (and scalpel-story-locate-ert-gwt-test--file
                       (file-exists-p
                        scalpel-story-locate-ert-gwt-test--file))
              (delete-file scalpel-story-locate-ert-gwt-test--file))
            (setq scalpel-story-locate-ert-gwt-test--file nil)))

(ert-gwt-deftest
  (:given ((_ (let ((file (make-temp-file "gwt-story-" nil ".el")))
                (with-temp-file file
                  (insert "(ert-gwt-deftest (:given ((x 1)))\n"
                          " (:when (progn (setq x (+ x 1))))\n"
                          " (:then (should (eq x 2))))\n"
                          "(ert-gwt-deftest (:given ((x 1)))\n"
                          " (:when (progn (setq x (+ x 1))))\n"
                          " (:then (should (eq x 2))))\n"))
                (setq scalpel-story-locate-ert-gwt-test--file file)))
           (names nil)))
  (:when (progn
           (setq names
                 (scalpel-locate-elisp-ert-gwt--names
                  scalpel-story-locate-ert-gwt-test--file))))
  (:then (should (equal names
                        (list "ert-gwt-deftest-69ca97a0"
                              "ert-gwt-deftest-69ca97a0-2"))))
  (:cleanup (when (and scalpel-story-locate-ert-gwt-test--file
                       (file-exists-p
                        scalpel-story-locate-ert-gwt-test--file))
              (delete-file scalpel-story-locate-ert-gwt-test--file))
            (setq scalpel-story-locate-ert-gwt-test--file nil)))
(ert-gwt-deftest
  (:given ((entry nil)))
  (:when (progn
           (setq entry
                 (assq 'ert-gwt-deftest
                       scalpel-locate-elisp--anonymous-definer-registry))))
  (:then (should (functionp (cdr entry)))))

(provide 'scalpel-story-locate-ert-gwt-test)

;;; scalpel-story-locate-ert-gwt-test.el ends here
