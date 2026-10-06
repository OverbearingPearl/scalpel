;;; scalpel-story-diagnose-test.el --- Diagnose blame stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for error classification.  When a
;; round fails because the model's reply broke the action contract I
;; am told the blame is the planner's and the failure is self-healable
;; with a retry; when the failure needs a file only the user can add
;; the blame is the context's and no retry burns tokens; anything else
;; is Scalpel's own bug.  A bracket-defect provider registered for an
;; extension answers for files of that language and is replaced by a
;; later registration of the same extension.  Every clause is a single
;; form and every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-diagnose)

(ert-gwt-deftest
  (:given ((err (list :type 'parse))))
  (:when (progn))
  (:then (should (eq (scalpel-diagnose-category
                      (plist-get err :type))
                     'planner))))

(ert-gwt-deftest
  (:given ((err (list :type 'file-outside-context))))
  (:when (progn))
  (:then (should (eq (scalpel-diagnose-category
                      (plist-get err :type))
                     'context))))

(ert-gwt-deftest
  (:given ((err (list :type 'some-internal-bug))))
  (:when (progn))
  (:then (should (eq (scalpel-diagnose-category
                      (plist-get err :type))
                     'executor))))

(ert-gwt-deftest
  (:given ((err (list :type 'pattern-no-match))))
  (:when (progn))
  (:then (should (scalpel-diagnose-planner-error-p err))))

(ert-gwt-deftest
  (:given ((err (list :type 'file-outside-context))))
  (:when (progn))
  (:then (should (not (scalpel-diagnose-planner-error-p err)))))

(ert-gwt-deftest
  (:given ((err (list :type 'malformed))))
  (:when (progn))
  (:then (should (scalpel-diagnose-self-heal-p err))))

(ert-gwt-deftest
  (:given ((err (list :type 'prose))))
  (:when (progn))
  (:then (should (not (scalpel-diagnose-self-heal-p err)))))

(ert-gwt-deftest
  (:given ((err (list :type 'file-outside-context))))
  (:when (progn))
  (:then (should (not (scalpel-diagnose-self-heal-p err)))))

(ert-gwt-deftest
  (:given ((err "just text")))
  (:when (progn))
  (:then (should (not (scalpel-diagnose-self-heal-p err)))))

(ert-gwt-deftest
  (:given ((_ (scalpel-diagnose-register-paren-provider
               '("xyz") (lambda (text) (list (list :text text)))))
           (defects nil)))
  (:when (progn
           (setq defects
                 (scalpel-diagnose-paren-defects
                  "/tmp/a.xyz" "payload"))))
  (:then (should (equal defects
                        (list (list :text "payload")))))
  (:cleanup (progn
              (setq scalpel-diagnose-paren-providers
                    (cl-remove-if
                     (lambda (e) (equal (car e) "xyz"))
                     scalpel-diagnose-paren-providers)))))

(ert-gwt-deftest
  (:given ((_ (scalpel-diagnose-register-paren-provider
               '("abc") (lambda (_) (list :old))))
           (_ (scalpel-diagnose-register-paren-provider
               '("abc") (lambda (_) (list :new))))
           (defects nil)))
  (:when (progn
           (setq defects
                 (scalpel-diagnose-paren-defects
                  "/tmp/a.abc" "x"))))
  (:then (should (equal defects (list :new))))
  (:cleanup (progn
              (setq scalpel-diagnose-paren-providers
                    (cl-remove-if
                     (lambda (e) (equal (car e) "abc"))
                     scalpel-diagnose-paren-providers)))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (setq result
                 (scalpel-diagnose-paren-defects
                  "/tmp/a.noprovider" "x"))))
  (:then (should (null result))))

(provide 'scalpel-story-diagnose-test)

;;; scalpel-story-diagnose-test.el ends here
