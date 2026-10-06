;;; scalpel-story-llm-dialect-test.el --- Dialect parser stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the default reply dialect.  I send
;; a TOML action document and get back parsed action plists; a
;; document written as one top-level tool table is accepted like an
;; action array; a broken string delimiter, an unterminated string and
;; an empty reply each tell me exactly what went wrong; and a reply
;; written in prose or in tool-call markup is named as such, so I know
;; the model's habit rather than a parse accident.  The triple-single
;; TOML delimiter is built at runtime with a helper so the source never
;; carries a quote run a TOML literal string cannot hold.  Every clause
;; is a single form and every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-llm-dialect)

(defun scalpel-story-llm-dialect-test-q3 ()
  "Return the triple-single-quote TOML delimiter as a string."
  (make-string 3 ?'))

(defun scalpel-story-llm-dialect-test-doc (pairs)
  "Build a TOML document from PAIRS of (key . value) with q3 values."
  (let ((q3 (scalpel-story-llm-dialect-test-q3)))
    (concat "[[action]]\n"
            (mapconcat
             (lambda (pair)
               (concat (car pair) " = " q3 (cdr pair) q3 "\n"))
             pairs ""))))

(defun scalpel-story-llm-dialect-test-cleanup ()
  "Remove the raw-TOML echo buffer left behind by parsing."
  (when (get-buffer "*Scalpel Raw TOML*")
    (kill-buffer "*Scalpel Raw TOML*")))

(ert-gwt-deftest
  (:given ((doc (scalpel-story-llm-dialect-test-doc
                 (list (cons "tool" "file-peek")
                       (cons "file" "/tmp/x.el"))))
           (actions nil)))
  (:when (progn
           (setq actions
                 (scalpel-llm-dialect--default-parse doc))))
  (:then (should (equal actions
                        (list (list :tool "file-peek"
                                    :file "/tmp/x.el")))))
  (:cleanup (scalpel-story-llm-dialect-test-cleanup)))

(ert-gwt-deftest
  (:given ((doc "tool = 'reply'\ntext = 'hello'\n")
           (actions nil)))
  (:when (progn
           (setq actions
                 (scalpel-llm-dialect--default-parse doc))))
  (:then (should (equal actions
                        (list (list :tool "reply"
                                    :text "hello")))))
  (:cleanup (scalpel-story-llm-dialect-test-cleanup)))

(ert-gwt-deftest
  (:given ((doc (concat "[[action]]\n"
                        "tool = 'file-peek'\n"
                        "files = ['/tmp/a.el', '/tmp/b.el']\n"))
           (actions nil)))
  (:when (progn
           (setq actions
                 (scalpel-llm-dialect--default-parse doc))))
  (:then (should (equal (plist-get (car actions) :files)
                        (list "/tmp/a.el" "/tmp/b.el"))))
  (:cleanup (scalpel-story-llm-dialect-test-cleanup)))

(ert-gwt-deftest
  (:given ((doc "  \n") (result nil)))
  (:when (progn
           (setq result
                 (condition-case err
                     (progn (scalpel-llm-dialect--default-parse doc)
                            nil)
                   (user-error (error-message-string err))))))
  (:then (should (string-search "empty reply" result)))
  (:cleanup (scalpel-story-llm-dialect-test-cleanup)))

(ert-gwt-deftest
  (:given ((doc "key = \"a\"\n") (result nil)))
  (:when (progn
           (setq result
                 (condition-case err
                     (progn
                       (scalpel-llm-dialect--validate-string-delimiters doc)
                       nil)
                   (error (error-message-string err))))))
  (:then (should (string-search "Invalid TOML string delimiter" result)))
  (:cleanup (scalpel-story-llm-dialect-test-cleanup)))

(ert-gwt-deftest
  (:given ((doc (concat "key = " (make-string 2 ?')
                        (make-string 1 ?') "abc"))
           (result nil)))
  (:when (progn
           (setq result
                 (condition-case err
                     (progn
                       (scalpel-llm-dialect--validate-string-delimiters doc)
                       nil)
                   (error (error-message-string err))))))
  (:then (should (string-search "Unterminated TOML string" result)))
  (:cleanup (scalpel-story-llm-dialect-test-cleanup)))

(ert-gwt-deftest
  (:given ((doc "tool = 'reply'\ntext = 'hi'\n") (result nil)))
  (:when (progn
           (setq result
                 (condition-case nil
                     (progn
                       (scalpel-llm-dialect--validate-string-delimiters doc)
                       :valid)
                   (error nil)))))
  (:then (should (eq result :valid)))
  (:cleanup (scalpel-story-llm-dialect-test-cleanup)))

(ert-gwt-deftest
  (:given ((doc "I think we should refactor this module.") (kind nil)))
  (:when (progn
           (setq kind
                 (condition-case nil
                     (progn (scalpel-llm-dialect--parse-error doc) nil)
                   (scalpel-llm-dialect-prose-reply-error :prose)
                   (user-error :other)))))
  (:then (should (eq kind :prose)))
  (:cleanup (scalpel-story-llm-dialect-test-cleanup)))

(ert-gwt-deftest
  (:given ((doc "<invoke type=\"shell\">\n<arg_key>command</arg_key>\n</invoke>")
           (kind nil)))
  (:when (progn
           (setq kind
                 (condition-case nil
                     (progn (scalpel-llm-dialect--parse-error doc) nil)
                   (scalpel-llm-dialect-tool-call-error :tool-call)
                   (user-error :other)))))
  (:then (should (eq kind :tool-call)))
  (:cleanup (scalpel-story-llm-dialect-test-cleanup)))

(provide 'scalpel-story-llm-dialect-test)

;;; scalpel-story-llm-dialect-test.el ends here
