;;; scalpel-story-llm-deepseek-test.el --- DeepSeek dialect stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the DeepSeek reply dialect.  When
;; the API layer drops the tool-call channel the model leaks its DSML
;; delimiters as literal text; I am told loud that nothing was executed
;; and that the backend must change, not retried.  A normal TOML reply
;; goes through the default parser unchanged.  Every clause is a
;; single form and every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-llm-deepseek)

(defun scalpel-story-llm-deepseek-test-dsml ()
  "Return a leaked DSML delimiter string without non-ASCII in source."
  (concat "<" (string #xFF5C) "DSML" (string #xFF5C)))

(ert-gwt-deftest
  (:given ((raw (concat "Scalpel plans:\n"
                        (scalpel-story-llm-deepseek-test-dsml)
                        "tool=reply\n"))
           (kind nil)))
  (:when (progn
           (setq kind
                 (condition-case nil
                     (progn (scalpel-llm-deepseek-parse-reply raw) nil)
                   (scalpel-llm-dialect-tool-call-error :tool-call)
                   (error :other)))))
  (:then (should (eq kind :tool-call))))

(ert-gwt-deftest
  (:given ((raw (concat "prefix " (scalpel-story-llm-deepseek-test-dsml)))
           (result nil)))
  (:when (progn
           (setq result
                 (condition-case err
                     (progn (scalpel-llm-deepseek-parse-reply raw) nil)
                   (error (error-message-string err))))))
  (:then (should (string-search "DSML" result)))
  (:then (should (string-search "nothing was executed" result))))

(ert-gwt-deftest
  (:given ((raw (concat "tool = 'reply'\ntext = 'hello'\n"))
           (actions nil)))
  (:when (progn
           (setq actions (scalpel-llm-deepseek-parse-reply raw))))
  (:then (should (equal actions
                        (list (list :tool "reply"
                                    :text "hello")))))
  (:cleanup (when (get-buffer "*Scalpel Raw TOML*")
              (kill-buffer "*Scalpel Raw TOML*"))))

(provide 'scalpel-story-llm-deepseek-test)

;;; scalpel-story-llm-deepseek-test.el ends here
