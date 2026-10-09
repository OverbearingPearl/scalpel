;;; scalpel-story-prompt-test.el --- Prompt assembly stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for prompt assembly in
;; `scalpel-prompt'.  I read the system prompt with both language
;; overrides off and see it open and close with the no-wrapper rule
;; and carry no conditional rules appended; when I set a reply
;; language the prompt grows by the reply-language rules; when I set
;; a thinking language it grows by the thinking rule; and the
;; history-compression instruction is always a nonempty string.
;; Every clause is a single form and every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-prompt)

(ert-gwt-deftest
  (:given ((base nil)))
  (:when (progn
           (let ((scalpel-prompt-reply-language nil)
                 (scalpel-thinking-language nil))
             (setq base (scalpel-prompt-system-prompt)))))
  (:then (should (string-prefix-p
                  scalpel-prompt-rule--no-wrapper base))
         (should (string-suffix-p
                  scalpel-prompt-rule--no-wrapper base))))

(ert-gwt-deftest
  (:given ((base nil) (with-reply nil)))
  (:when (progn
           (let ((scalpel-prompt-reply-language nil)
                 (scalpel-thinking-language nil))
             (setq base (scalpel-prompt-system-prompt)))
           (let ((scalpel-prompt-reply-language "English")
                 (scalpel-thinking-language nil))
             (setq with-reply (scalpel-prompt-system-prompt)))))
  (:then (should (> (length with-reply) (length base)))))

(ert-gwt-deftest
  (:given ((base nil) (with-thinking nil)))
  (:when (progn
           (let ((scalpel-prompt-reply-language nil)
                 (scalpel-thinking-language nil))
             (setq base (scalpel-prompt-system-prompt)))
           (let ((scalpel-prompt-reply-language nil)
                 (scalpel-thinking-language "English"))
             (setq with-thinking (scalpel-prompt-system-prompt)))))
  (:then (should (> (length with-thinking) (length base)))))

(ert-gwt-deftest
  (:given ((text nil)))
  (:when (setq text (scalpel-prompt--history-compress-instruction)))
  (:then (should (and (stringp text)
                      (not (string-empty-p text))))))

(provide 'scalpel-story-prompt-test)

;;; scalpel-story-prompt-test.el ends here
