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

(defun scalpel-llm-test--mock-backend ()
  "A backend that passes preflight without network access."
  (gptel-make-openai "Scalpel-test" :key "test-key"))

(ert-deftest scalpel-llm-test-api-key-error-p ()
  "Recognize gptel's missing-API-key setup error."
  (should (scalpel-llm--api-key-error-p "‘gptel-api-key’ is not valid"))
  (should-not (scalpel-llm--api-key-error-p "Scalpel: LLM request idle for more than 30 seconds")))

(ert-deftest scalpel-llm-test-count-tokens-prices-cjk-per-character ()
  "CJK characters cost about one token each, not a quarter of one.
Regression: the 4-characters-per-token heuristic priced Chinese
text at a quarter of its real cost, so a prompt dominated by
Chinese was under-reported fourfold."
  (ert-info ("Input: 8 ASCII chars; expect 2 tokens")
    (should (= (scalpel-llm--count-tokens "abcdefgh") 2)))
  (ert-info ("Input: 8 CJK chars; expect 8 tokens, not 2")
    (should (= (scalpel-llm--count-tokens "你好世界，测试。") 8)))
  (ert-info ("Input: mixed; expect CJK per char plus ASCII per 4")
    (should (= (scalpel-llm--count-tokens "你好abcd") 3)))
  (ert-info ("Input: accented Latin is not CJK; expect the ASCII ratio")
    (should (= (scalpel-llm--count-tokens "café") 1))))

(ert-deftest scalpel-llm-test-token-counters-are-defined ()
  "The cumulative token counters must be defined at load time.
Regression: `scalpel-llm-request-async' updates
`scalpel-llm--total-uploaded' and `scalpel-llm--total-received`;
when their `defvar' forms are missing, every async test fails with
an indirect void-variable error instead of naming the cause."
  (should (boundp 'scalpel-llm--total-uploaded))
  (should (boundp 'scalpel-llm--total-received)))

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
          (gptel-backend (scalpel-llm-test--mock-backend))
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
          (gptel-backend (scalpel-llm-test--mock-backend))
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
          (gptel-backend (scalpel-llm-test--mock-backend))
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
          (gptel-backend (scalpel-llm-test--mock-backend))
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
          (gptel-backend (scalpel-llm-test--mock-backend))
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

(ert-deftest scalpel-llm-test-async-counts-reasoning-tokens ()
  "Reasoning chunks count toward the received-token total.
The console's down counter tracks all downstream data, so a model
that streams a long reasoning pass before its reply must show
progress during that pass, not stall at zero."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let* ((scalpel-llm-timeout 5)
           (gptel-backend (scalpel-llm-test--mock-backend))
           (reasoning "step 1 ")
           (reasoning-tokens (scalpel-llm--count-tokens reasoning))
           tokens-after-reasoning
           delivered)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest args)
                   (let ((cb (plist-get args :callback)))
                     (funcall cb (cons 'reasoning reasoning) nil)
                     ;; Snapshot before any content arrives: at this point
                     ;; only reasoning has been streamed, so the down
                     ;; counter must already be past its reset value.
                     (setq tokens-after-reasoning scalpel-llm--tokens-received)
                     (funcall cb "answer" nil)
                     (funcall cb t nil))
                   'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (response) (setq delivered response))
         (lambda (err) (ert-fail (plist-get err :message)))))
      (ert-info ((format "tokens-received=%S after reasoning only (expected %d)"
                         tokens-after-reasoning reasoning-tokens))
        (should (= tokens-after-reasoning reasoning-tokens)))
      (should (string= delivered "answer")))))

(ert-deftest scalpel-llm-test-async-reports-backend-error ()
  "A nil RESPONSE from gptel is reported through ON-ERROR as `api'."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          (gptel-backend (scalpel-llm-test--mock-backend))
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
          (gptel-backend (scalpel-llm-test--mock-backend))
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
          (gptel-backend (scalpel-llm-test--mock-backend))
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
          (gptel-backend (scalpel-llm-test--mock-backend))
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

(ert-deftest scalpel-llm-test-async-reports-keyless-backend-immediately ()
  "A keyless backend is reported as `api-key', never an idle timeout.
Regression: a keyless backend -- including a stub leaked into the
session by a test -- sent the request into a real network call that
never called back, and the round died on the idle timer with a
misleading `idle' error naming no cause.  The nil-backend case is
guarded by the preflight; a keyless backend passes it and is
caught by the synchronous error `gptel-request' raises for a
missing key."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          (gptel-backend (gptel-make-openai "Scalpel-test-keyless"))
          error)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _ignore)
                   (error "\u2018gptel-api-key\u2019 is not valid"))))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (_r) (ert-fail "a keyless backend must not succeed"))
         (lambda (err) (setq error err))))
      (ert-info ((format "Error: %S" error))
        (should (eq (plist-get error :type) 'api-key))
        (should (string-match-p "scalpel-set-backend"
                                (plist-get error :message))))
      (should (null scalpel-llm--cancel-current)))))

(ert-deftest scalpel-llm-test-async-reports-unconfigured-backend-immediately ()
  "No backend at all fails fast as `api-key', not after idle timeout.
Regression: `gptel-backend' nil made `gptel-request' hang silently
until the idle timer reported an `idle' error."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          (gptel-backend nil)
          error
          requested)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (setq requested t) 'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (_r) (ert-fail "an unconfigured backend must not succeed"))
         (lambda (err) (setq error err))))
      (ert-info ((format "Error: %S" error))
        (should (eq (plist-get error :type) 'api-key)))
      (should-not requested))))

(ert-deftest scalpel-llm-test-select-backend-delegates-to-gptel-menu ()
  "Backend selection delegates to `gptel-menu'."
  (let ((called 0)
        (orig (symbol-function 'gptel-menu)))
    ;; `scalpel-llm-select-backend' dispatches through
    ;; `call-interactively', which requires a command: the stub carries
    ;; an `(interactive)' form so `commandp' accepts it.
    (unwind-protect
        (progn
          (fset 'gptel-menu
                (lambda (&rest _)
                  (setq called (1+ called))
                  t))
          (put 'gptel-menu 'commandp t)
          (put 'gptel-menu 'interactive-form '(interactive))
          (scalpel-llm-select-backend)
          (should (= called 1)))
      (fset 'gptel-menu orig)
      (put 'gptel-menu 'commandp nil)
      (put 'gptel-menu 'interactive-form nil))))

(ert-deftest scalpel-llm-test-async-explicit-cancel-reports-cancelled ()
  "Cancelling a request in flight reports :type `cancelled' once.
Regression: the cancel closure existed but nothing exercised it, so
a broken cancellation path would only surface in interactive use."
  (scalpel-llm-test-with-clean-reasoning-buffer
    (let ((scalpel-llm-timeout 5)
          (gptel-backend (scalpel-llm-test--mock-backend))
          error
          delivered)
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_prompt &rest _args)
                   'fake-fsm)))
        (scalpel-llm-request-async
         "test prompt"
         (lambda (response) (setq delivered response))
         (lambda (err) (setq error err)))
        (should scalpel-llm--cancel-current)
        (funcall scalpel-llm--cancel-current)
        (ert-info ((format "Error: %S" error))
          (should (eq (plist-get error :type) 'cancelled)))
        ;; A late callback after the cancel is dropped.
        (should-not delivered)
        (should (null scalpel-llm--cancel-current))))))

(provide 'scalpel-llm-test)

;;; scalpel-llm-test.el ends here
