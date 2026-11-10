;;; scalpel-story-locate-nested-test.el --- Nested block note tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the nested-definition diagnostics:
;; `scalpel-locate-elisp--nested-definition-block' and
;; `scalpel-agent--nested-block-note'.

;;; Code:

(require 'ert)
(require 'ert-gwt)
(require 'scalpel-locate-elisp)
(require 'scalpel-agent)

(defconst scalpel-story-locate-nested-test-sample
  ";; sample
(require 'foo)
(use-package bar
  :config
  (defun bar-helper ()
    \"Doc.\"
    nil))
(defconst top-name 1)
(provide 'sample)
"
  "A file whose bar-helper sits in a `use-package' :config body.")

(ert-gwt-deftest
    (:given ((file (make-temp-file "scalpel-story-locate-when" nil ".el"))
             (_ (with-temp-file file
                  (insert "(when (require 'windmove nil :noerror)\n"
                          "  (defun my/smart-window-switch ()\n"
                          "    \"Switch windows in a smart order.\"\n"
                          "    nil))\n")))
             (buffer (find-file-noselect file))
             (symbols nil)))
    (:when (setq symbols
                 (with-current-buffer buffer
                   (scalpel-locate-elisp-list-symbols file))))
    (:then (ert-info ("A defun under a when wrapper is listed as the synthesized container name when-1")
             (should (member "when-1" symbols))))
    (:then (ert-info ("The nested defun is not listed; the whole when block is the addressable unit")
             (should-not (member "my/smart-window-switch" symbols))))
    (:cleanup (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (set-buffer-modified-p nil))
                (kill-buffer buffer))
              (when (file-exists-p file)
                (delete-file file))))

(ert-gwt-deftest
  (:given ((sample scalpel-story-locate-nested-test-sample)
           (file (make-temp-file "scalpel-story-nested-" nil ".el"))
           (_ (with-temp-file file (insert sample)))
           (buffer (find-file-noselect file))
           (reported nil)))
  (:when (setq reported
              (with-current-buffer buffer
                (scalpel-locate-elisp--nested-definition-block
                 "bar-helper"))))
  (:then (ert-info ("A defun inside use-package :config reports use-package bar")
           (should (equal reported "use-package bar"))))
  (:cleanup
   (progn
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (set-buffer-modified-p nil))
       (kill-buffer buffer))
     (when (file-exists-p file)
       (delete-file file)))))

(ert-gwt-deftest
  (:given ((sample scalpel-story-locate-nested-test-sample)
           (file (make-temp-file "scalpel-story-nested-" nil ".el"))
           (_ (with-temp-file file (insert sample)))
           (buffer (find-file-noselect file))
           (reported 'unset)))
  (:when (setq reported
               (with-current-buffer buffer
                 (scalpel-locate-elisp--nested-definition-block
                  "top-name"))))
  (:then (ert-info ("A top-level definition reports nil, not a block")
           (should (null reported))))
  (:cleanup
   (progn
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (set-buffer-modified-p nil))
       (kill-buffer buffer))
     (when (file-exists-p file)
       (delete-file file)))))

(ert-gwt-deftest
  (:given ((sample scalpel-story-locate-nested-test-sample)
           (file (make-temp-file "scalpel-story-nested-" nil ".el"))
           (_ (with-temp-file file (insert sample)))
           (buffer (find-file-noselect file))
           (note nil)))
  (:when (setq note
               (with-current-buffer buffer
                 (scalpel-agent--nested-block-note
                  (buffer-file-name buffer) "bar-helper"))))
  (:then (ert-info ("The agent note names the block and both remedies")
           (should (string-match-p "use-package bar" note))
           (should (string-match-p "hoist" note))))
  (:cleanup
   (progn
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (set-buffer-modified-p nil))
       (kill-buffer buffer))
     (when (file-exists-p file)
       (delete-file file)))))

(ert-gwt-deftest
  (:given ((sample scalpel-story-locate-nested-test-sample)
           (file (make-temp-file "scalpel-story-nested-" nil ".el"))
           (_ (with-temp-file file (insert sample)))
           (buffer (find-file-noselect file))
           (note 'unset)))
  (:when (setq note
              (with-current-buffer buffer
                (scalpel-agent--nested-block-note
                 (buffer-file-name buffer) "top-name"))))
  (:then (ert-info ("A top-level name adds no note")
           (should (null note))))
  (:cleanup
   (progn
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (set-buffer-modified-p nil))
       (kill-buffer buffer))
     (when (file-exists-p file)
       (delete-file file)))))

(provide 'scalpel-story-locate-nested-test)

;;; scalpel-story-locate-nested-test.el ends here
