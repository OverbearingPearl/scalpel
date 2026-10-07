;;; scalpel-story-llm-laguna-test.el --- Laguna dialect stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the Laguna tool-call dialect: a
;; complete call parses to one action, several calls keep their
;; order, and incomplete or merely-mentioned markup is refused
;; loudly instead of partly executed.

;;; Code:

(require 'ert)
(require 'ert-gwt)
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
(ert-gwt-deftest
  (:given ((providers scalpel-llm-dialect-providers)
           (actions nil)
           (raw nil))
          (setq raw (scalpel-story-llm-laguna-test--call
                     "block-edit"
                     '("file" . "/tmp/a.el")
                     '("symbol" . "foo"))))
  (:when (setq actions (scalpel-llm-laguna-parse-reply raw)))
  (:then (should (= (length actions) 1)))
  (:then (should (string= (plist-get (car actions) :tool)
                          "block-edit")))
  (:then (should (string= (plist-get (car actions) :file)
                          "/tmp/a.el")))
  (:then (should (string= (plist-get (car actions) :symbol) "foo")))
  (:cleanup (setq scalpel-llm-dialect-providers providers)))

;; Story B: several calls convert in the order they were written.
(ert-gwt-deftest
  (:given ((providers scalpel-llm-dialect-providers)
           (actions nil)
           (raw nil))
          (setq raw (concat
                     (scalpel-story-llm-laguna-test--call
                      "shell" '("command" . "echo hi"))
                     "\n"
                     (scalpel-story-llm-laguna-test--call
                      "reply" '("text" . "done")))))
  (:when (setq actions (scalpel-llm-laguna-parse-reply raw)))
  (:then (should (= (length actions) 2)))
  (:then (should (string= (plist-get (nth 0 actions) :tool) "shell")))
  (:then (should (string= (plist-get (nth 1 actions) :text) "done")))
  (:cleanup (setq scalpel-llm-dialect-providers providers)))

;; Story C: a reply cut off mid-call is refused, not partly executed.
(ert-gwt-deftest
  (:given ((providers scalpel-llm-dialect-providers)
           (err nil)
           (raw nil))
          (setq raw "<tool_call>block-edit\n<arg_key>file</arg_key><arg_value>/tmp/a.el"))
  (:when (setq err (condition-case e
                       (progn (scalpel-llm-laguna-parse-reply raw)
                              nil)
                     (scalpel-llm-dialect-tool-call-error e))))
  (:then (should err))
  (:cleanup (setq scalpel-llm-dialect-providers providers)))

;; Story D: markup the reply only mentions is refused, not converted.
(ert-gwt-deftest
  (:given ((providers scalpel-llm-dialect-providers)
           (err nil)
           (raw nil))
          (setq raw "I would write a <tool_call> block here."))
  (:when (setq err (condition-case e
                       (progn (scalpel-llm-laguna-parse-reply raw)
                              nil)
                     (scalpel-llm-dialect-tool-call-error e))))
  (:then (should err))
  (:cleanup (setq scalpel-llm-dialect-providers providers)))

;; Story E: an incomplete argument makes the whole reply refused.
(ert-gwt-deftest
  (:given ((providers scalpel-llm-dialect-providers)
           (err nil)
           (raw nil))
          (setq raw (concat
                     "<tool_call>block-edit\n"
                     "<arg_key>file</arg_key><arg_value>/tmp/a.el</arg_value>\n"
                     "<arg_key>symbol</arg_key><arg_value>")))
  (:when (setq err (condition-case e
                       (progn (scalpel-llm-laguna-parse-reply raw)
                              nil)
                     (scalpel-llm-dialect-tool-call-error e))))
  (:then (should err))
  (:cleanup (setq scalpel-llm-dialect-providers providers)))

;; Story F: plain prose reaches the default parser's loud refusal.
(ert-gwt-deftest
  (:given ((providers scalpel-llm-dialect-providers)
           (err nil)
           (raw nil))
          (setq raw "I think the refactor should start with the parser."))
  (:when (setq err (condition-case e
                       (progn (scalpel-llm-laguna-parse-reply raw)
                              nil)
                     (scalpel-llm-dialect-prose-reply-error e))))
  (:then (should err))
  (:cleanup (setq scalpel-llm-dialect-providers providers)))

(provide 'scalpel-story-llm-laguna-test)

;;; scalpel-story-llm-laguna-test.el ends here
