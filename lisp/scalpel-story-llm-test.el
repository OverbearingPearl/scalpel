;;; scalpel-story-llm-test.el --- LLM gateway stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the offline parts of the LLM
;; gateway: CJK-heavy text is priced near one token per character,
;; ASCII near four per token, a console buffer reuses its own
;; reasoning buffer while a plain buffer keeps the base name, and
;; the reasoning buffer resets and appends as documented.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-llm)

(ert-gwt-deftest
  (:given ((n nil)))
  (:when (setq n (scalpel-llm--count-tokens "abcdefgh")))
  (:then (should (= n 2))))

(ert-gwt-deftest
  (:given ((n nil)
           (_ (ignore "一二三四五六七八九十 counted as one each"))))
  (:when (setq n (scalpel-llm--count-tokens "一二三四五六七八九十")))
  (:then (should (= n 10))))

(ert-gwt-deftest
  (:given ((mixed nil)))
  (:when (setq mixed (scalpel-llm--count-tokens "abcd一二三四")))
  (:then (should (= mixed 5))))

(ert-gwt-deftest
  (:given ((plain (generate-new-buffer " *llm-story-plain*"))
           (name nil)))
  (:when (setq name (scalpel-llm--reasoning-buffer-name plain)))
  (:then (should (string= name scalpel-llm-reasoning-buffer-name)))
  (:cleanup (when (buffer-live-p plain) (kill-buffer plain))))

(ert-gwt-deftest
  (:given ((console (generate-new-buffer " *llm-story-console*"))
           (name nil))
          (with-current-buffer console
            (set (make-local-variable 'scalpel-console--root) (current-buffer))))
  (:when (setq name (scalpel-llm--reasoning-buffer-name console)))
  (:then (should (string= name
                          (format "*scalpel-thinking<%s>*"
                                  (buffer-name console)))))
  (:cleanup (when (buffer-live-p console) (kill-buffer console))))

(ert-gwt-deftest
  (:given ((name "*llm-story-reasoning*")
           (len nil))
          (scalpel-llm--reset-reasoning-buffer name))
  (:when (progn
           (scalpel-llm--append-reasoning "thought" name)
           (setq len
                 (buffer-size (get-buffer name)))))
  (:then (should (= len 7)))
  (:cleanup (let ((buffer (get-buffer name)))
              (when buffer (kill-buffer buffer)))))

(ert-gwt-deftest
  (:given ((key-error nil)
           (api-error nil)))
  (:when (progn
           (setq key-error
                 (scalpel-llm--api-key-error-p
                  "gptel-api-key is not valid"))
           (setq api-error
                 (scalpel-llm--api-key-error-p
                  "connection refused"))))
  (:then (should key-error))
  (:then (should (null api-error))))

(provide 'scalpel-story-llm-test)

;;; scalpel-story-llm-test.el ends here
