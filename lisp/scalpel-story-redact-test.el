;;; scalpel-story-redact-test.el --- Redaction boundary stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the redaction boundary.  I type my
;; real home path into a request and the model never sees my user
;; name; the model echoes the placeholder back and my real path comes
;; home.  A rule I register replaces any secret pattern, and the same
;; round trip applies in reverse.  When the switch is off the boundary
;; is a pass-through in both directions, so a placeholder I typed by
;; hand survives untouched.  A placeholder the model mistypes is
;; flagged by drift detection only when a rule's placeholder has the
;; double-brace shape, so the drift story registers such a rule first.
;; Placeholder literals are built with `concat' so the source file
;; never carries the contiguous built-in placeholder text, which the
;; restore step would otherwise rewrite back into the real home path.
;; Every clause is a single form and every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-redact)

(defconst scalpel-story-redact-test-placeholder
  (concat "{{SCALPE" "L_USER}}")
  "The built-in placeholder.
It is spelled so the source file never contains it contiguously.")

(ert-gwt-deftest
  (:given ((_ (setq scalpel-redact-rules nil))
           (_ (setq scalpel-redact-enabled t))
           (secret (concat "/Users/" (user-real-login-name)
                           "/Projects/secret.txt"))
           (result nil)))
  (:when (setq result (scalpel-redact-apply secret)))
  (:then (should (string-prefix-p
                  (concat scalpel-story-redact-test-placeholder
                          "/Projects/")
                  result)))
  (:cleanup (setq scalpel-redact-rules nil
                  scalpel-redact-enabled t)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-redact-rules nil))
           (_ (setq scalpel-redact-enabled t))
           (secret (concat "/Users/" (user-real-login-name)
                           "/Projects/secret.txt"))
           (result nil)))
  (:when (setq result (scalpel-redact-restore
                       (scalpel-redact-apply secret))))
  (:then (should (string= result secret)))
  (:cleanup (setq scalpel-redact-rules nil
                  scalpel-redact-enabled t)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-redact-rules nil))
           (_ (setq scalpel-redact-enabled t))
           (_ (scalpel-redact-install-defaults))
           (_ (scalpel-redact-register
               "s3cret" "{{SECRET_TOKEN}}" "s3cret"))
           (draft (concat "my password is s3cret and lives under /Users/"
                          (user-real-login-name) "/x\n"))
           (result nil)))
  (:when (setq result (scalpel-redact-apply draft)))
  (:then (should (not (string-search "s3cret" result))))
  (:then (should (not (string-search (user-real-login-name)
                                     result))))
  (:cleanup (setq scalpel-redact-rules nil
                  scalpel-redact-enabled t)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-redact-rules nil))
           (_ (setq scalpel-redact-enabled t))
           (draft "my password is s3cret\n")
           (result nil)))
  (:when (setq result (scalpel-redact-restore
                       (scalpel-redact-apply draft))))
  (:then (should (string= result draft)))
  (:cleanup (setq scalpel-redact-rules nil
                  scalpel-redact-enabled t)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-redact-rules nil))
           (_ (setq scalpel-redact-enabled nil))
           (typed (concat "send the log under "
                          scalpel-story-redact-test-placeholder
                          "/tmp\n"))
           (result nil)))
  (:when (setq result (list (scalpel-redact-apply typed)
                            (scalpel-redact-restore typed))))
  (:then (should (equal result (list typed typed))))
  (:cleanup (setq scalpel-redact-rules nil
                  scalpel-redact-enabled t)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-redact-rules nil))
           (_ (setq scalpel-redact-enabled t))
           (_ (scalpel-redact-register
               "realuser"
               (concat "{{SCALPE" "L_USER}}")
               "realuser"))
           (reply (concat "see {{SCCALPE" "L_USER}}/Projects/x.el\n"))
           (result nil)))
  (:when (setq result (scalpel-redact-placeholder-drift reply)))
  (:then (should (string-search
                  (concat "{{SCCALPE" "L_USER}}")
                  result)))
  (:cleanup (setq scalpel-redact-rules nil
                  scalpel-redact-enabled t)))

(ert-gwt-deftest
  (:given ((_ (setq scalpel-redact-rules nil))
           (_ (setq scalpel-redact-enabled t))
           (reply (concat "see "
                          scalpel-story-redact-test-placeholder
                          "/Projects/x.el\n"))
           (result 'unset)))
  (:when (setq result (scalpel-redact-placeholder-drift reply)))
  (:then (should (not result)))
  (:cleanup (setq scalpel-redact-rules nil
                  scalpel-redact-enabled t)))

(provide 'scalpel-story-redact-test)

;;; scalpel-story-redact-test.el ends here
