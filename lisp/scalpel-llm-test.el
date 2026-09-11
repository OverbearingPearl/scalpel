;;; scalpel-llm-test.el --- Tests for scalpel-llm -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-llm.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-llm)

(ert-deftest scalpel-llm-test-api-key-error-p ()
  "Recognize gptel's missing-API-key setup error."
  (should (scalpel-llm--api-key-error-p "‘gptel-api-key’ is not valid"))
  (should-not (scalpel-llm--api-key-error-p "Scalpel: LLM request idle for more than 30 seconds")))

(ert-deftest scalpel-llm-test-fails-loudly-without-gptel ()
  "Requiring `scalpel-llm' must fail loudly when gptel is absent.
gptel is a hard dependency, so loading must signal `file-missing'
rather than silently deferring.  Runs a child Emacs with every
gptel directory removed from `load-path' and loads the source file
directly, so a stale byte-compiled copy cannot mask the result."
  (let* ((emacs (let ((path (and invocation-directory
                                 (expand-file-name invocation-name
                                                   invocation-directory))))
                  (or (and path (file-executable-p path) path)
                      (executable-find "emacs"))))
         (lib (locate-library "scalpel-llm"))
         (lisp-dir (and lib (file-name-directory lib))))
    (skip-unless (and emacs lisp-dir))
    (with-temp-buffer
      (let* ((code (format (concat "(progn"
                                   " (require 'cl-lib)"
                                   " (setq load-path (cl-remove-if"
                                   " (lambda (d) (string-match-p \"gptel\" d))"
                                   " load-path))"
                                   " (condition-case err"
                                   "     (progn (load-file"
                                   " (expand-file-name \"scalpel-llm.el\" %s))"
                                   " (princ \"SCALPEL-LLM-LOADED\"))"
                                   " (file-missing (princ \"SCALPEL-LLM-FAILED-LOUDLY\"))))")
                           (prin1-to-string lisp-dir)))
             (status (call-process emacs nil t nil
                                   "-Q" "-batch" "-L" lisp-dir
                                   "--eval" code))
             (output (buffer-string)))
        (unless (and (eq 0 status)
                     (string-match-p "SCALPEL-LLM-FAILED-LOUDLY" output)
                     (not (string-match-p "SCALPEL-LLM-LOADED" output)))
          (ert-fail
           (format (concat "scalpel-llm must fail with file-missing when "
                           "gptel is absent: status=%S output=%S")
                   status output)))))))

(ert-deftest scalpel-llm-test-streaming-accumulates-response-and-tokens ()
  "Streaming chunks accumulate into the response and update token counters.
Uses the real gptel streaming protocol: content chunks are strings,
the end of the stream is signaled by RESPONSE = t, and reasoning
chunks arrive as (reasoning . TEXT) conses."
  (cl-letf (((symbol-function 'gptel-request)
             (lambda (_prompt &rest args)
               (let ((cb (plist-get args :callback)))
                 (funcall cb "Hello" nil)
                 (funcall cb " world" nil)
                 (funcall cb t nil))
               'fake-fsm)))
    (let ((result (scalpel-llm-request "test prompt")))
      (should (string= result "Hello world"))
      (should (> scalpel-llm--tokens-received 0))
      (should (> scalpel-llm--tokens-uploaded 0)))))

(ert-deftest scalpel-llm-test-reasoning-buffer-collects-chunks ()
  "Reasoning chunks are collected in the reasoning buffer.
Uses the real gptel streaming protocol: reasoning arrives as
conses of the form (reasoning . TEXT) in the RESPONSE argument."
  (unwind-protect
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (let ((cb (plist-get args :callback)))
                     (funcall cb '(reasoning . "step 1 ") nil)
                     (funcall cb '(reasoning . "step 2") nil)
                     (funcall cb t nil))
                   'fake-fsm)))
        (scalpel-llm-request "test prompt")
        (with-current-buffer (get-buffer scalpel-llm-reasoning-buffer-name)
          (should (string= (buffer-string) "step 1 step 2"))))
    (when (get-buffer scalpel-llm-reasoning-buffer-name)
      (kill-buffer scalpel-llm-reasoning-buffer-name))))

(ert-deftest scalpel-llm-test-idle-timeout-abandons-request ()
  "A silent backend trips the idle timeout and its late callback is dropped.
The abandoned request must not mutate the shared token counters, or a
late response would corrupt the next request."
  (let ((scalpel-llm-timeout 0.2)
        (late-callback nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest args)
                 (setq late-callback (plist-get args :callback))
                 'fake-fsm)))
      (should-error (scalpel-llm-request "test prompt") :type 'user-error))
    (should late-callback)
    (let ((tokens scalpel-llm--tokens-received))
      (funcall late-callback "late chunk" nil)
      (should (= tokens scalpel-llm--tokens-received)))))

(ert-deftest scalpel-llm-test-active-stream-survives-idle-timeout ()
  "A stream that outlives the idle budget but keeps talking must succeed.
Total request time exceeds `scalpel-llm-timeout', so a wall-clock
deadline would kill it; the idle budget must not."
  (let* ((scalpel-llm-timeout 0.3)
         (saved-callback nil)
         (ticks 0))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_prompt &rest args)
                 (setq saved-callback (plist-get args :callback))
                 'fake-fsm))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _ignore)
                 ;; Each poll burns wall-clock time, then delivers a chunk,
                 ;; so the stream stays active across a span longer than a
                 ;; wall-clock deadline would allow.
                 (sit-for 0.2)
                 (setq ticks (1+ ticks))
                 (funcall saved-callback "." nil)
                 (when (>= ticks 3)
                   (funcall saved-callback t nil)))))
      (let ((result (scalpel-llm-request "test prompt")))
        (ert-info ((format "Result: %S (ticks=%d)" result ticks))
          (should (string= result "...")))))))

(provide 'scalpel-llm-test)

;;; scalpel-llm-test.el ends here
