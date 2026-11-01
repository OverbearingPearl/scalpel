;;; scalpel-story-locate-org-test.el --- Org locator stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the Org locator: a user points
;; Scalpel at an Org document and asks what headline subtrees it holds
;; and where a subtree ends.  Real documents carry #+begin_src blocks
;; whose headline-looking lines must never be reported.  Every clause
;; is a single form; every THEN holds one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-locate-org)

(ert-gwt-deftest
  (:given ((content "* Intro\nbody\n\n* Usage\nrun it\n\n** Tips\nsmall\n")
           (result nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-org-list-symbols nil))))
  (:then (should (equal result '("Intro" "Usage" "Tips")))))

(ert-gwt-deftest
  (:given ((content "* Real\n#+begin_src sh\n* not a headline\n#+end_src\n* After\n")
           (result nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-org-list-symbols nil))))
  (:then (should (equal result '("Real" "After")))))

(ert-gwt-deftest
  (:given ((content "* Usage\nrun it\n\n** Tips\nsmall\n* Next\n")
           (range nil) (text nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq range (scalpel-locate-org-range nil "Usage"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal text "* Usage\nrun it\n\n** Tips\nsmall"))))

(ert-gwt-deftest
  (:given ((full nil) (partial nil)))
  (:when (setq full (scalpel-locate-org--single-definition-p
                     "* A\none\n")
              partial (scalpel-locate-org--single-definition-p
                       "* A\none\n* B\n")))
  (:then (should (and full (not partial)))))

(ert-gwt-deftest
  (:given ((provider nil)
           (locate-fn nil)
           (list-symbols-fn nil)))
  (:when (progn
           (require 'scalpel-locate)
           (setq provider (scalpel-locate-provider-for-file "x.org"))
           (setq locate-fn (plist-get provider :locate))
           (setq list-symbols-fn (plist-get provider :list-symbols))))
  (:then (should provider)
         (should (functionp locate-fn))
         (should (functionp list-symbols-fn))))

(provide 'scalpel-story-locate-org-test)

;;; scalpel-story-locate-org-test.el ends here
