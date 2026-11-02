;;; scalpel-story-console-abort-test.el --- Abort recovery stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for aborting a busy console round:
;; an unanswered instruction comes back to the input area when that
;; area was empty, while typed-but-unsent input is discarded and the
;; sent instruction stays a user turn of the transcript.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-console)

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-abort-" t))
           (console (let ((default-directory root))
                      (scalpel-console-open)
                      (current-buffer)))
           (regions nil))
          (let ((inhibit-read-only t))
            (scalpel-console--insert-tagged "explain the parser" 'user)))
  (:when (let ((scalpel-console--busy t))
           (scalpel-console-abort)))
  (:then (ert-info ("Abort leaves exactly one pending-input region")
           (setq regions (scalpel-console--pending-input-regions))
           (should (= (length regions) 1))))
  (:then (ert-info ("The restored instruction is replayed verbatim")
           (should (equal (buffer-substring-no-properties
                           (car (car regions)) (cdr (car regions)))
                          "explain the parser"))))
  (:then (ert-info ("The restored input carries no face at all")
           (should (null (text-property-not-all
                          (car (car regions)) (cdr (car regions))
                          'face nil)))))
  (:then (ert-info ("The first character carries no role tag")
           (should (null (get-text-property (car (car regions))
                                            'scalpel-console-role)))))
  (:cleanup (progn
              (when (buffer-live-p console)
                (let ((proc (get-buffer-process console)))
                  (when proc
                    (set-process-query-on-exit-flag proc nil)))
                (kill-buffer console))
              (when (file-directory-p root)
                (delete-directory root t)))))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-abort-" t))
           (console (let ((default-directory root))
                      (scalpel-console-open)
                      (current-buffer))))
          (let ((inhibit-read-only t))
            (scalpel-console--insert-tagged "explain the parser" 'user)
            (goto-char (point-max))
            (insert "half-typed note")))
  (:when (let ((scalpel-console--busy t))
           (scalpel-console-abort)))
  (:then (ert-info ("Typed-but-unsent input is discarded, not restored")
           (should (null (scalpel-console--pending-input-regions)))))
  (:then (ert-info ("The discarded note is gone from the transcript")
           (should (null (string-search
                          "half-typed"
                          (buffer-substring-no-properties
                           (point-min) (point-max)))))))
  (:then (ert-info ("The abort is announced in the transcript")
           (should (string-search
                    "Mission aborted"
                    (buffer-substring-no-properties
                     (point-min) (point-max))))))
  (:cleanup (progn
              (when (buffer-live-p console)
                (let ((proc (get-buffer-process console)))
                  (when proc
                    (set-process-query-on-exit-flag proc nil)))
                (kill-buffer console))
              (when (file-directory-p root)
                (delete-directory root t)))))

(provide 'scalpel-story-console-abort-test)

;;; scalpel-story-console-abort-test.el ends here
