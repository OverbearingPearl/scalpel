;;; scalpel-llm-deepseek-test.el --- Tests for scalpel-llm-deepseek -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the DeepSeek reply dialect.  No LLM is contacted.

;;; Code:

(require 'ert)
(require 'scalpel-llm-deepseek)

(ert-deftest scalpel-llm-deepseek-test-parse-refuses-dsml ()
  "A reply carrying DSML tool-call delimiters signals `user-error'."
  (ert-info ("Input: DSML invoke markup with U+FF5C delimiters; expect user-error naming DSML")
    (let ((raw (concat "<" (string #xFF5C) "DSML" (string #xFF5C) "calls>\n"
                       "<" (string #xFF5C) "DSML" (string #xFF5C)
                       "invoke name=\"read\">\n"
                       "</" (string #xFF5C) "DSML" (string #xFF5C) "invoke>\n"
                       "</" (string #xFF5C) "DSML" (string #xFF5C) "calls>")))
      (should-error (scalpel-llm-deepseek-parse-reply raw)
                    :type 'user-error))))

(ert-deftest scalpel-llm-deepseek-test-parse-delegates-valid-json ()
  "A plain JSON reply parses through the default parser unchanged."
  (ert-info ("Input: bare action array; expect the action parsed")
    (let ((result (scalpel-llm-deepseek-parse-reply
                   "[{\"tool\":\"reply\",\"text\":\"hi\"}]")))
      (should (equal (plist-get (car result) :tool) "reply")))))

(ert-deftest scalpel-llm-deepseek-test-parse-ascii-tool-call-still-refused ()
  "ASCII tool-call markup without DSML delimiters still fails loudly."
  (ert-info ("Input: <invoke> markup; expect user-error from the default parser")
    (should-error
     (scalpel-llm-deepseek-parse-reply
      "<invoke name=\"read\">\n<parameter name=\"file\">/tmp/a.el</parameter>\n</invoke>")
     :type 'user-error)))

(provide 'scalpel-llm-deepseek-test)

;;; scalpel-llm-deepseek-test.el ends here
