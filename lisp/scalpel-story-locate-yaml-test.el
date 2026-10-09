;;; scalpel-story-locate-yaml-test.el --- YAML locator stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the YAML locator: a user points
;; Scalpel at a YAML file and asks which top-level blocks exist and
;; where one block ends.  Stories cover listing keys, block extent,
;; single-block replacement check, a missing symbol, shell scripts
;; embedded in a run block never surfacing as keys, and comment or
;; plain-text lines never surfacing as keys.  Every clause is a
;; single form; every THEN holds one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-locate-yaml)

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (with-temp-buffer
           (insert "name: ci\non:\n  push:\n    branches: [main]\njobs:\n  build:\n    runs-on: ubuntu-latest")
           (setq result (scalpel-locate-yaml-list-symbols nil))))
  (:then (should (equal result '("name" "on" "jobs")))))

(ert-gwt-deftest
  (:given ((range nil) (text nil)))
  (:when (with-temp-buffer
           (insert "name: ci\njobs:\n  build:\n    runs-on: ubuntu-latest\nnext: x\n")
           (setq range (scalpel-locate-yaml-range nil "jobs"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal text
                        "jobs:\n  build:\n    runs-on: ubuntu-latest"))))

(ert-gwt-deftest
  (:given ((full nil) (partial nil)))
  (:when (setq full (scalpel-locate-yaml--single-definition-p "a: 1\n")
             partial (scalpel-locate-yaml--single-definition-p "a: 1\nb: 2\n")))
  (:then (should (and full (not partial)))))

(ert-gwt-deftest
  (:given ((range nil)))
  (:when (with-temp-buffer
           (insert "name: ci\njobs:\n  build:\n")
           (setq range (scalpel-locate-yaml-range nil "missing"))))
  (:then (should (null range))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (with-temp-buffer
           (insert "name: ci\njobs:\n  build:\n    runs-on: ubuntu-latest\n    run: |\n      echo \"name: decoy\"\n      echo done\nsummary: ok\n")
           (setq result (scalpel-locate-yaml-list-symbols nil))))
  (:then (should (equal result '("name" "jobs" "summary")))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (with-temp-buffer
           (insert "# a comment\n\nplain text\nkey: value\n")
           (setq result (scalpel-locate-yaml-list-symbols nil))))
  (:then (should (equal result '("key")))))

(ert-gwt-deftest
  (:given ((range nil) (text nil)))
  (:when (with-temp-buffer
           (insert "a: 1\nb: 2\n\nc: 3\n")
           (setq range (scalpel-locate-yaml-range nil "b"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal text "b: 2"))))

(ert-gwt-deftest
  (:given ((range nil) (text nil)))
  (:when (with-temp-buffer
           (insert "on:\n  push:\n    branches: [main]\njobs:\n  build:\n")
           (setq range (scalpel-locate-yaml-range nil "on"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal text "on:\n  push:\n    branches: [main]"))))

(ert-gwt-deftest
  (:given ((range nil) (text nil)))
  (:when (with-temp-buffer
           (insert "a: 1\njobs:\n  build:\n")
           (setq range (scalpel-locate-yaml-range nil "jobs"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal text "jobs:\n  build:"))))

(provide 'scalpel-story-locate-yaml-test)

;;; scalpel-story-locate-yaml-test.el ends here
