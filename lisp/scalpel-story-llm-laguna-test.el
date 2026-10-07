;;; scalpel-story-llm-laguna-test.el --- Laguna dialect stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the Laguna reply dialect: a backend
;; that answers with text tool calls instead of the TOML action
;; document.  A complete call is converted one action per block, a
;; reply the backend cut off is refused whole rather than executed in
;; part, markup the reply merely mentions is refused, and a reply in
;; plain prose reaches the loud refusal instead of acting.

;;; Code:

(require 'ert)
(require 'scalpel-llm-laguna)

(defun scalpel-story-llm-laguna-test--call (tool &rest pairs)
  "Render TOOL with argument PAIRS as one Laguna text call."
  (concat "<tool_call>" tool "\n"
          (mapconcat
           (lambda (pair)
             (concat "<arg_key>" (car pair) "</arg_key>"
                     "<arg_value>" (cdr pair) "</arg_value>"))
           pairs
           "\n")
          "</tool_call>"))

;; Story A: a complete text call becomes one action plist.
(ert-deftest scalpel-story-llm-laguna-test-complete-call-parses ()
  (let ((providers scalpel-llm-dialect-providers))
    (unwind-protect
        (progn
          ;; Given a reply holding one complete call with two arguments.
          (let* ((raw (scalpel-story-llm-laguna-test--call
                       "block-edit"
                       '("file" . "/tmp/a.el")
                       '("symbol" . "foo")))
                 ;; Then it parses to exactly one action.
                 (actions (scalpel-llm-laguna-parse-reply raw)))
            (should (= (length actions) 1))
            (should (string= (plist-get (car actions) :tool) "block-edit"))
            (should (string= (plist-get (car actions) :file)
                             "/tmp/a.el"))
            (should (string= (plist-get (car actions) :symbol) "foo"))))
      (setq scalpel-llm-dialect-providers providers))))

;; Story B: several calls convert in the order they were written.
(ert-deftest scalpel-story-llm-laguna-test-multiple-calls-in-order ()
  (let ((providers scalpel-llm-dialect-providers))
    (unwind-protect
        (progn
          ;; Given a reply with a shell action first and a reply action
          ;; second.
          (let* ((raw (concat
                       (scalpel-story-llm-laguna-test--call
                        "shell"
                        '("command" . "echo hi"))
                       "\n"
                       (scalpel-story-llm-laguna-test--call
                        "reply"
                        '("text" . "done"))))
                 (actions (scalpel-llm-laguna-parse-reply raw)))
            ;; Then both actions come back in written order.
            (should (= (length actions) 2))
            (should (string= (plist-get (nth 0 actions) :tool) "shell"))
            (should (string= (plist-get (nth 1 actions) :text) "done"))))
      (setq scalpel-llm-dialect-providers providers))))

;; Story C: a reply cut off mid-call is refused, not partly executed.
(ert-deftest scalpel-story-llm-laguna-test-cut-off-refused ()
  (let ((providers scalpel-llm-dialect-providers)
        (echo-buf (get-buffer-create "*Scalpel Raw TOML*")))
    (unwind-protect
        (progn
          ;; Given a call whose closing marker never arrives, the
          ;; finished prefix must not be executed.
          (let ((raw "<tool_call>block-edit\n<arg_key>file</arg_key><arg_value>/tmp/a.el"))
            (should-error
             (scalpel-llm-laguna-parse-reply raw)
             :type 'scalpel-llm-dialect-tool-call-error)))
      (when (buffer-live-p echo-buf) (kill-buffer echo-buf))
      (setq scalpel-llm-dialect-providers providers))))

;; Story D: markup the reply only mentions is refused, not converted.
(ert-deftest scalpel-story-llm-laguna-test-mentioned-markup-refused ()
  (let ((providers scalpel-llm-dialect-providers)
        (echo-buf (get-buffer-create "*Scalpel Raw TOML*")))
    (unwind-protect
        (progn
          ;; Given prose that merely quotes a tool-call tag without a
          ;; complete call.
          (let ((raw "I would write a <tool_call> block here."))
            ;; Then the reply is refused loudly rather than parsed.
            (should-error
             (scalpel-llm-laguna-parse-reply raw)
             :type 'scalpel-llm-dialect-tool-call-error)))
      (when (buffer-live-p echo-buf) (kill-buffer echo-buf))
      (setq scalpel-llm-dialect-providers providers))))

;; Story E: an incomplete argument makes the whole reply refused.
(ert-deftest scalpel-story-llm-laguna-test-incomplete-argument-refused ()
  (let ((providers scalpel-llm-dialect-providers)
        (echo-buf (get-buffer-create "*Scalpel Raw TOML*")))
    (unwind-protect
        (progn
          ;; Given a call whose last argument value never closes.
          (let ((raw (concat
                      "<tool_call>block-edit\n"
                      "<arg_key>file</arg_key><arg_value>/tmp/a.el</arg_value>\n"
                      "<arg_key>symbol</arg_key><arg_value>")))
            ;; Then no action comes back; the reply is refused.
            (should-error
             (scalpel-llm-laguna-parse-reply raw)
             :type 'scalpel-llm-dialect-tool-call-error)))
      (when (buffer-live-p echo-buf) (kill-buffer echo-buf))
      (setq scalpel-llm-dialect-providers providers))))

;; Story F: plain prose reaches the default parser's loud refusal.
(ert-deftest scalpel-story-llm-laguna-test-prose-reply-refused ()
  (let ((providers scalpel-llm-dialect-providers)
        (echo-buf (get-buffer-create "*Scalpel Raw TOML*")))
    (unwind-protect
        (progn
          ;; Given a reply with neither a call nor a TOML document.
          (let ((raw "I think the refactor should start with the parser."))
            ;; Then the dialect falls through to the default parser,
            ;; which refuses prose with its own error.
            (should-error
             (scalpel-llm-laguna-parse-reply raw)
             :type 'scalpel-llm-dialect-prose-reply-error)))
      (when (buffer-live-p echo-buf) (kill-buffer echo-buf))
      (setq scalpel-llm-dialect-providers providers))))

(provide 'scalpel-story-llm-laguna-test)

;;; scalpel-story-llm-laguna-test.el ends here
