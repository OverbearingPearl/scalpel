;;; scalpel-llm-test.el --- Tests for scalpel-llm -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-llm.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-llm)

(defmacro scalpel-llm-test-with-clean-reasoning-buffer (&rest body)
  "Run BODY, then kill the reasoning buffer it may have created.
Every request recreates `scalpel-llm-reasoning-buffer-name' through
`get-buffer-create', so a test that does not clean it up leaves a
buffer behind."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (when (get-buffer scalpel-llm-reasoning-buffer-name)
       (kill-buffer scalpel-llm-reasoning-buffer-name))))

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

(ert-deftest scalpel-llm-test-async-idle-timer-drops-late-callbacks ()
  "A silent backend trips the idle timer and its late callback is dropped.
The abandoned request must not mutate the shared token counters, or a
late response would corrupt the next request."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 0.2)
          saved-callback
          error)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (setq saved-callback (plist-get args :callback))
                   'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (_response) (ert-fail "a silent backend must not succeed"))
         (lambda (err) (setq error err)))
        ;; The timer runs from the event loop, not from the request, so
        ;; this waits for it instead of calling it.  The deadline keeps a
        ;; timer that never fires from hanging.
        (let ((deadline (+ (float-time) 2)))
          (while (and (null error) (< (float-time) deadline))
            (sit-for 0.05))))
      (should (eq (plist-get error :type) 'idle))
      (should saved-callback)
      (let ((tokens scalpel-llm--tokens-received))
        (funcall saved-callback "late chunk" nil)
        (ert-info ((format "tokens=%d after a late chunk"
                           scalpel-llm--tokens-received))
          (should (= tokens scalpel-llm--tokens-received)))))))

(ert-deftest scalpel-llm-test-async-active-stream-survives-idle-timeout ()
  "A stream that outlives the idle budget but keeps talking must succeed.
The whole request lasts longer than `scalpel-llm-timeout', so a
wall-clock deadline would kill it; every callback re-arms the idle
timer, so the idle budget must not."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 0.3)
          saved-callback
          delivered
          error)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (setq saved-callback (plist-get args :callback))
                   'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (response) (setq delivered response))
         (lambda (err) (setq error err)))
        ;; Three chunks, each after a pause shorter than the budget, for a
        ;; total span longer than it.  `sit-for' drives the event loop,
        ;; so a timer that was not re-armed fires in the middle of this.
        (dotimes (_ 3)
          (sit-for 0.2)
          (funcall saved-callback "." nil))
        (funcall saved-callback t nil))
      (ert-info ((format "Delivered: %S, error: %S" delivered error))
        (should-not error)
        (should (string= delivered "..."))))))

(ert-deftest scalpel-llm-test-async-returns-before-the-callback ()
  "The call returns while the backend is still thinking.
This is the property the console relies on: the command loop must
regain control before the response arrives, which is exactly what
the synchronous entry point prevented."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          saved-callback
          delivered)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (setq saved-callback (plist-get args :callback))
                   'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (response) (setq delivered response))
         (lambda (err) (ert-fail (plist-get err :message)))))
      (ert-info ("control must be back with the request outstanding")
        (should saved-callback)
        (should-not delivered))
      ;; Settle the request so its idle timer does not outlive the test.
      (funcall saved-callback "done" nil)
      (funcall saved-callback t nil)
      (should (string= delivered "done")))))

(ert-deftest scalpel-llm-test-async-delivers-the-whole-stream-once ()
  "Chunks accumulate; ON-SUCCESS runs once, at the end of the stream."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          calls)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (let ((cb (plist-get args :callback)))
                     (funcall cb "Hello" nil)
                     (funcall cb " world" nil)
                     (funcall cb t nil))
                   'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (response) (push response calls))
         (lambda (err) (ert-fail (plist-get err :message)))))
      (ert-info ((format "Delivered: %S" calls))
        (should (equal calls '("Hello world")))
        (should (> scalpel-llm--tokens-received 0))
        (should (> scalpel-llm--tokens-uploaded 0))))))

(ert-deftest scalpel-llm-test-async-collects-reasoning-chunks ()
  "Reasoning chunks go to the reasoning buffer, not into the response."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          delivered)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (let ((cb (plist-get args :callback)))
                     (funcall cb '(reasoning . "step 1 ") nil)
                     (funcall cb "answer" nil)
                     (funcall cb '(reasoning . "step 2") nil)
                     (funcall cb t nil))
                   'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (response) (setq delivered response))
         (lambda (err) (ert-fail (plist-get err :message)))))
      (should (string= delivered "answer"))
      (with-current-buffer (get-buffer scalpel-llm-reasoning-buffer-name)
        (should (string= (buffer-string) "step 1 step 2"))))))

(ert-deftest scalpel-llm-test-async-reports-backend-error ()
  "A nil RESPONSE from gptel is reported through ON-ERROR as `api'."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          error
          delivered)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (funcall (plist-get args :callback) nil '(:status 500))
                   'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (response) (setq delivered response))
         (lambda (err) (setq error err))))
      (ert-info ((format "Error: %S" error))
        (should (eq (plist-get error :type) 'api))
        (should (string-match-p "500" (plist-get error :message)))
        (should-not delivered)))))

(ert-deftest scalpel-llm-test-async-reports-missing-api-key ()
  "A synchronous failure naming the API key is reported as `api-key'."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          error)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _ignore)
                   (error "\u2018gptel-api-key\u2019 is not valid"))))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (_response) (ert-fail "a missing API key must not succeed"))
         (lambda (err) (setq error err))))
      (should (eq (plist-get error :type) 'api-key)))))

(ert-deftest scalpel-llm-test-async-reports-setup-failure ()
  "Any other synchronous failure is reported as `setup'."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          error)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _ignore)
                   (error "Scalpel test: backend not configured"))))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (_response) (ert-fail "a failed setup must not succeed"))
         (lambda (err) (setq error err))))
      (ert-info ((format "Error: %S" error))
        (should (eq (plist-get error :type) 'setup))
        (should (string-match-p "not configured" (plist-get error :message)))))))

(ert-deftest scalpel-llm-test-async-cancels-its-idle-timer ()
  "A settled request leaves its idle timer cancelled.
Regression: the timer was armed after dispatch and only cancelled
by abandon; a success that forgot to cancel it would fire later and
report an idle error for a request that finished long ago."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 0.1)
          error
          delivered)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (funcall (plist-get args :callback) "done" nil)
                   (funcall (plist-get args :callback) t nil)
                   'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (response) (setq delivered response))
         (lambda (err) (setq error err))))
      (should (string= delivered "done"))
      ;; Outlive the idle budget: a leftover timer would fire here.
      (sit-for 0.3)
      (ert-info ((format "Error after settling: %S" error))
        (should-not error)))))

(provide 'scalpel-llm-test)

;;; scalpel-llm-test.el ends here
