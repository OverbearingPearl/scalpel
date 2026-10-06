;;; scalpel-story-locate-markdown-test.el --- Markdown locator stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the Markdown locator: a user
;; points Scalpel at a Markdown document and asks what sections it
;; holds and where a section ends.  Real documents carry fenced code
;; blocks whose hash lines must never be read as headings, YAML front
;; matter whose keys are not headings, and definer lines of the form
;; "- NAME: rest".  Every clause is a single form; every THEN holds
;; one `should'.  Result variables are declared directly in the
;; given bindings.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-locate-markdown)

(ert-gwt-deftest
  (:given ((content "# Intro\nbody\n\n# Usage\nrun it\n\n## Tips\nsmall\n")
           (result nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-markdown-list-symbols nil))))
  (:then (should (equal result '("Intro" "Usage" "Tips")))))

(ert-gwt-deftest
  (:given ((content "# Real\n```sh\n# not a heading\n```\n# After\n")
           (result nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-markdown-list-symbols nil))))
  (:then (should (equal result '("Real" "After")))))

(ert-gwt-deftest
  (:given ((content "---\nkey: # value\n---\n\n# Body\ntext\n")
           (result nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-markdown-list-symbols nil))))
  (:then (should (equal result '("Body")))))

(ert-gwt-deftest
  (:given ((content "# Usage\nrun it\n\n## Tips\nsmall\n# Next\n")
           (range nil) (text nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq range (scalpel-locate-markdown-range nil "Usage"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal text "# Usage\nrun it\n\n## Tips\nsmall"))))

(ert-gwt-deftest
  (:given ((full nil) (partial nil)))
  (:when (setq full (scalpel-locate-markdown--single-definition-p
                     "# A\none\n")
              partial (scalpel-locate-markdown--single-definition-p
                       "# A\none\n# B\n")))
  (:then (should (and full (not partial)))))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-md-" t))
           (file (expand-file-name "doc.md" dir))
           (_ (with-temp-file file
                (insert "# Doc\n- tool: hammer\n- tool: saw\n- note: fine\n")))
           (names nil)))
  (:when (setq names (scalpel-locate-markdown-definer-names file)))
  (:then (should (equal names '("tool" "note"))))
  (:cleanup (delete-directory dir t)))

(provide 'scalpel-story-locate-markdown-test)

;;; scalpel-story-locate-markdown-test.el ends here
