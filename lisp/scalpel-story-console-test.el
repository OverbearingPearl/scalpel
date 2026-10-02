;;; scalpel-story-console-test.el --- User-visible console stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the console as a user sees it:
;; point behaviour, folded report bodies, dimming, the Roger
;; acknowledgement line and pending input capture.  Every story
;; states a world, one user action and only user-observable
;; outcomes.  THEN clauses must evaluate truthy: ert-gwt wraps each
;; in `should', so negated expectations are written as
;; (should (not ...)) rather than (should-not ...), whose nil return
;; would fail the wrapping `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-console)

(defvar scalpel-story-console-test-captured-regions nil
  "Holds the pending input regions captured inside a story.")

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-console-collapse-output t)
           (scalpel-console-trim-consumed-output nil))
          (let ((default-directory root))
            (scalpel-console-open))
          (scalpel-console--append "First reply body." 'assistant))
  (:given "I scrolled up to read the beginning of the buffer"
          (goto-char (point-min)))
  (:when (scalpel-console--append "Second reply body." 'assistant))
  (:then (should (= (point) (point-min))))
  (:then (should (string-search "Second reply body"
                                (buffer-substring-no-properties
                                 (point-min) (point-max)))))
  (:cleanup (delete-directory root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-console-collapse-output t)
           (scalpel-console-trim-consumed-output nil))
          (let ((default-directory root))
            (scalpel-console-open))
          (scalpel-console--append "First reply body." 'assistant))
  (:given "I am reading at the end of the buffer"
          (goto-char (point-max)))
  (:when (scalpel-console--append "Second reply body." 'assistant))
  (:then (should (= (point) (point-max))))
  (:cleanup (delete-directory root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-console-collapse-output t)
           (scalpel-console-trim-consumed-output nil)
           (buf (progn
                  (let ((default-directory root))
                    (scalpel-console-open))
                  (current-buffer))))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "fix the bug in the parser\n")))
  (:when (with-current-buffer buf
           (cl-letf (((symbol-function 'scalpel-console--run-rounds)
                      (lambda (_instr _history)
                        (scalpel-console--append
                         "Here is the answer." 'assistant))))
             (scalpel-console-send-line))))
  (:then (with-current-buffer buf
           (should (not (string-search
                         "Roger. Working"
                         (buffer-substring-no-properties
                          (point-min) (point-max)))))))
  (:then (with-current-buffer buf
           (should (string-search
                    "Here is the answer"
                    (buffer-substring-no-properties
                     (point-min) (point-max))))))
  (:cleanup (delete-directory root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-console-collapse-output t)
           (scalpel-console-trim-consumed-output nil))
          (let ((default-directory root))
            (scalpel-console-open)))
  (:when (scalpel-console--append "Context refresh note." nil))
  (:then (should (text-property-any
                  (point-min) (point-max) 'face 'shadow)))
  (:cleanup (delete-directory root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-console-collapse-output t)
           (scalpel-console-trim-consumed-output nil))
          (let ((default-directory root))
            (scalpel-console-open)))
  (:when (scalpel-console--append
          (concat "Header line\n"
                  "Read: some-file.el\n"
                  "--- output ---\n"
                  "many lines of dump\n"
                  "--- end output ---\n")
          'assistant))
  (:then (should (catch 'collapsed
                   (let ((pos (point-min)))
                     (while (< pos (point-max))
                       (when (get-text-property pos
                                                'scalpel-console-collapsed)
                         (throw 'collapsed t))
                       (setq pos (next-single-property-change
                                  pos 'scalpel-console-collapsed
                                  nil (point-max))))
                     nil))))
  (:cleanup (delete-directory root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-console-collapse-output t)
           (scalpel-console-trim-consumed-output nil))
          (let ((default-directory root))
            (scalpel-console-open))
          (scalpel-console--append "A long earlier reply." 'assistant))
  (:given "I typed an instruction in the middle, above the buffer end"
          (goto-char (point-min))
          (insert "fix the bug in the parser\n"))
  (:when (setq scalpel-story-console-test-captured-regions
               (scalpel-console--pending-input-regions)))
  (:then (should (= (length scalpel-story-console-test-captured-regions) 1)))
  (:then (should (equal
                  (buffer-substring-no-properties
                   (car (car scalpel-story-console-test-captured-regions))
                   (cdr (car scalpel-story-console-test-captured-regions)))
                  "fix the bug in the parser\n")))
  (:cleanup (delete-directory root t)))

(ert-gwt-deftest
  (:given ((root (make-temp-file "scalpel-story-" t))
           (scalpel-console-collapse-output t)
           (scalpel-console-trim-consumed-output nil))
          (let ((default-directory root))
            (scalpel-console-open)))
  (:when (progn
           (scalpel-console--append "Round one." 'assistant)
           (scalpel-console--append "Round two." 'assistant)))
  (:then (should (= (length
                     (scalpel-console--assistant-report-regions))
                    2)))
  (:cleanup (delete-directory root t)))

(provide 'scalpel-story-console-test)

;;; scalpel-story-console-test.el ends here
