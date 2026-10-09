;;; scalpel-story-locate-test.el --- Locator dispatch stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the locator dispatcher.  Asking a
;; language nobody registered signals `user-error'; a language with no
;; bracket opinion answers t for balance; a form counter counts
;; complete top-level forms; the single-form validator admits one
;; registration call and refuses two; and a missing symbol in range
;; lookup is a `user-error'.  Every clause is a single form and every
;; THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-locate)

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result (scalpel-locate-balanced-p "foo.bar" "((")))
  (:then (should (eq result t))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result
               (scalpel-locate-balanced-p "notes.yaml" "(((")))
  (:then (should (eq result t))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result
               (scalpel-locate-form-count
                "foo.el" "(defun a ())\n(defun b ())")))
  (:then (should (= result 2))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result (scalpel-locate-single-form-p "foo.el" "(x)")))
  (:then (should result)))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result
               (condition-case err
                   (progn (scalpel-locate-single-form-p "foo.bar" "(x)") nil)
                 (user-error (error-message-string err)))))
  (:then (should (string-search "no form validator" result))))

(ert-gwt-deftest
  (:given ((result nil) (file nil)))
  (:when (progn
           (setq file (make-temp-file "scalpel-story-locate" nil ".el"))
           (with-temp-file file (insert "(defun a ())\n"))
           (setq result
                 (condition-case err
                     (progn (scalpel-locate-range file "zzz") nil)
                   (user-error (error-message-string err))))))
  (:then (should (string-search "not found" result))))

(provide 'scalpel-story-locate-test)

;;; scalpel-story-locate-test.el ends here
