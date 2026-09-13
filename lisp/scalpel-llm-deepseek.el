;;; scalpel-llm-deepseek.el --- DeepSeek reply dialect for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Reply dialect for DeepSeek backends.  DeepSeek models emit their
;; internal tool-call delimiters (U+FF5C fullwidth vertical bars around
;; DSML tags) as literal text when the API layer does not carry the
;; tool-call channel; such a reply is not a JSON action array, and
;; translating it here would paper over a planner that ignored the
;; output contract.  The dialect therefore detects the markers and
;; fails loud with a readable error; every other reply goes through
;; the default parser unchanged.

;;; Code:

(require 'scalpel-llm-dialect)

(defconst scalpel-llm-deepseek--dsml-regexp
  (concat "<" (string #xFF5C) "DSML" (string #xFF5C))
  "Regexp matching a leaked DeepSeek DSML tool-call delimiter.
The delimiters are U+FF5C (fullwidth vertical bar), which the
tokenizer emits around its internal tool-call tags; as literal
response text they mean the planner answered in tool-call syntax
instead of the JSON action array.")

(defun scalpel-llm-deepseek-parse-reply (raw)
  "Parse a DeepSeek raw reply RAW, refusing DSML tool-call syntax.
Signal `scalpel-llm-dialect-tool-call-error' when RAW carries the
DSML delimiters: nothing was executed, and the type tells the
console that switching backend, not retrying, is the remedy.
Otherwise delegate to the default parser."
  (when (string-match-p scalpel-llm-deepseek--dsml-regexp raw)
    (signal 'scalpel-llm-dialect-tool-call-error
            (list
             (format (concat "Scalpel: DeepSeek planner emitted DSML "
                             "tool-call syntax instead of the JSON action "
                             "array; nothing was executed.  Reply was: %s")
                     (scalpel-llm-dialect--visible-raw raw)))))
  (scalpel-llm-dialect--default-parse raw))

(scalpel-llm-dialect-register
 "deepseek"
 (list :parse-reply #'scalpel-llm-deepseek-parse-reply))

(provide 'scalpel-llm-deepseek)

;;; scalpel-llm-deepseek.el ends here
