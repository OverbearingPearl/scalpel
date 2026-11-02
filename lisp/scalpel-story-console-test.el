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

(ert-deftest scalpel-story-console-test-parse-prefix-boundary
  ()
  (ert-info ("'? ' prefix yields consult with leading whitespace stripped")
    (should (equal (scalpel-console-parse-prefix "?  hello")
                   (cons 'consult "hello"))))
  (ert-info ("'! ' prefix yields shell-mode with leading whitespace stripped")
    (should (equal (scalpel-console-parse-prefix "!  ls -la")
                   (cons 'shell-mode "ls -la"))))
  (ert-info ("bare '?' at start without following whitespace is plain text")
    (should (equal (scalpel-console-parse-prefix "?hello")
                   (cons nil "?hello"))))
  (ert-info ("bare '!' at start without following whitespace is plain text")
    (should (equal (scalpel-console-parse-prefix "!ls")
                   (cons nil "!ls"))))
  (ert-info ("'?' not at position 0 does not trigger prefix parsing")
    (should (equal (scalpel-console-parse-prefix "what? yes")
                   (cons nil "what? yes")))
    (should (equal (scalpel-console-parse-prefix "why! no")
                   (cons nil "why! no")))))

(ert-deftest scalpel-story-console-test-model-tag-vendor-prefix ()
  "Lock vendor-prefix stripping in `scalpel-console--model-tag'.
A display tag built from a \"z-ai/\"-prefixed `gptel-model' must drop
the vendor namespace; a bare model name is rendered verbatim inside
the same angle brackets."
  (let ((old-bound (boundp 'gptel-model))
        (old-model (and (boundp 'gptel-model)
                        (symbol-value 'gptel-model))))
    (unwind-protect
        (progn
          (set 'gptel-model "z-ai/glm-4.6")
          (ert-info ("gptel-model \"z-ai/glm-4.6\" renders as <glm-4.6>")
            (should (equal "<glm-4.6>"
                           (scalpel-console--model-tag))))
          (set 'gptel-model "glm-4.6")
          (ert-info ("bare gptel-model \"glm-4.6\" is rendered verbatim inside the same angle brackets")
            (should (equal "<glm-4.6>"
                           (scalpel-console--model-tag)))))
      ;; Rewrite the saved value or drop the binding we created, so the
      ;; prior binding state survives even on assertion failure.
      (if old-bound
          (set 'gptel-model old-model)
        (makunbound 'gptel-model)))))

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

(ert-gwt-deftest
  (:given "the console keymap bindings are installed"
          (should (keymapp scalpel-console-mode-map)))
  (:when (lookup-key scalpel-console-mode-map (kbd "C-c C-a")))
  (:then (should (eq (lookup-key scalpel-console-mode-map (kbd "C-c C-a"))
                       #'scalpel-console-add-file)))
  (:then (should (eq (lookup-key scalpel-console-mode-map (kbd "C-c C-v"))
                       #'scalpel-console-add-file-readonly))))

(ert-deftest scalpel-story-console-test-abort-restores-previous-instruction-without-face ()
  "Aborting over an empty input area restores the prior instruction verbatim.
This pins the regression where the restore carried the echoed highlight
face and its role tag into the input area, which content-only assertions
let slip through.  Expected to turn green once `scalpel-console-abort'
drops the echoed properties on restore."
  (let* ((workdir (make-temp-file "scalpel-console-abort" t))
         (console (let ((default-directory workdir))
                    (scalpel-console-open)
                    (current-buffer))))
    (unwind-protect
        (progn
          (ert-info ((format "Opening the console in %S must yield a live console buffer"
                             workdir))
            (should (buffer-live-p console)))
          (with-current-buffer console
            ;; Previous round is finished: its user instruction sits
            ;; in the transcript, replayed through the console's own
            ;; tagging path so the echo carries the highlight face.
            ;; A new round is running and the input area is still
            ;; empty.
            (let ((inhibit-read-only t))
              (scalpel-console--insert-tagged "explain the parser" 'user))
            (ert-info ("Input area must be empty before the abort is triggered")
              (should (null (scalpel-console--pending-input-regions))))
            (let ((scalpel-console--busy t))
              (scalpel-console-abort))
            ;; The input area was empty before the abort, so the
            ;; pending input found afterwards is exactly the restored
            ;; unsent instruction.
            (let* ((regions (scalpel-console--pending-input-regions))
                   (region (car regions)))
              (ert-info ((format "Abort must leave exactly one pending-input region, but found %S"
                                 (length regions)))
                (should (= (length regions) 1)))
              (let* ((start (car region))
                     (end (cdr region))
                     (restored (buffer-substring start end))
                     (pos start)
                     faced-pos)
                (while (and (not faced-pos) (< pos end))
                  (when (get-text-property pos 'face)
                    (setq faced-pos pos))
                  (setq pos (or (next-single-property-change pos 'face nil end)
                                end)))
                (ert-info ((format "Abort must replay the instruction verbatim into the sole pending-input region: expected %S, got %S"
                                   "explain the parser"
                                   restored))
                  (should (string= restored "explain the parser")))
                (ert-info ((format "Restored input must carry no face at all, but found %S at offset %S"
                                   (and faced-pos (get-text-property faced-pos 'face))
                                   (and faced-pos (- faced-pos start))))
                  (should-not faced-pos))
                (ert-info ((format "First character of the restored input must carry no role tag, but found %S"
                                   (get-text-property start 'scalpel-console-role)))
                  (should-not (get-text-property start 'scalpel-console-role)))))))
      (when (buffer-live-p console)
        (let ((proc (get-buffer-process console)))
          (when proc
            (set-process-query-on-exit-flag proc nil)))
        (kill-buffer console))
      (delete-directory workdir t))))

(provide 'scalpel-story-console-test)

;;; scalpel-story-console-test.el ends here
