;;; scalpel-llm-test.el --- Tests for scalpel-llm -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for scalpel-llm.

;;; Code:

(require 'ert)
(require 'scalpel-llm)

(ert-deftest scalpel-llm-test-api-key-error-p ()
  "Recognize gptel's missing-API-key setup error."
  (should (scalpel-llm--api-key-error-p "‘gptel-api-key’ is not valid"))
  (should-not (scalpel-llm--api-key-error-p "Scalpel: LLM request timed out")))

(provide 'scalpel-llm-test)

;;; scalpel-llm-test.el ends here
