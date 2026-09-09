;;; scalpel-agent-test.el --- Tests for scalpel-agent -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'scalpel-agent)
(require 'scalpel-execute)
(require 'scalpel-locate)
(require 'scalpel-locate-elisp)

(ert-deftest scalpel-agent-test-parse-json ()
  "Parse a valid JSON array of actions into a list of plists."
  (let ((raw "[{\"tool\":\"edit\",\"file\":\"/tmp/foo.el\",\"symbol\":\"bar\",\"instruction\":\"do something\"}]"))
    (let ((actions (scalpel-agent--parse-json raw)))
      (should (= (length actions) 1))
      (should (equal (plist-get (car actions) :tool) "edit"))
      (should (equal (plist-get (car actions) :file) "/tmp/foo.el"))
      (should (equal (plist-get (car actions) :symbol) "bar"))
      (should (equal (plist-get (car actions) :instruction) "do something")))))

(ert-deftest scalpel-agent-test-parse-json-invalid ()
  "Invalid JSON should signal a user-error."
  (should-error
   (scalpel-agent--parse-json "not json")
   :type 'error))

(ert-deftest scalpel-agent-test-apply-if-unchanged ()
  "Apply replacement when body is unchanged; abort when it changed."
  (let ((file (make-temp-file "scalpel-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun foo (x)\n  (+ x 1))\n"))
          (let* ((range (with-current-buffer (find-file-noselect file)
                          (scalpel-locate-elisp--top-definition-range "foo")))
                 (beg (car range))
                 (end (cdr range))
                 (body (with-current-buffer (find-file-noselect file)
                         (buffer-substring-no-properties beg end))))
            ;; Unchanged body: should apply
            (let ((report (scalpel-agent--apply-if-unchanged file "foo" body "(defun foo (x)\n  (+ x 2))")))
              (should (string-match "Edited foo" report))
              (with-current-buffer (find-file-noselect file)
                (should (string= (buffer-string) "(defun foo (x)\n  (+ x 2))\n"))))
            ;; Changed body: should signal
            (let ((modified-body (concat body " ;; modified")))
              (should-error
               (scalpel-agent--apply-if-unchanged file "foo" modified-body "(defun foo (x)\n  (+ x 3))")
               :type 'error))))
      (when (get-file-buffer file)
        (with-current-buffer (get-file-buffer file)
          (set-buffer-modified-p nil))
        (kill-buffer (get-file-buffer file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest scalpel-agent-test-execute-action-edit ()
  "Execute an edit action by mocking the LLM request."
  (let ((file (make-temp-file "scalpel-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun foo (x)\n  (+ x 1))\n"))
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (_prompt &optional _system)
                       "(defun foo (x)\n  (+ x 2))")))
            (let ((action (list :tool "edit"
                                :file file
                                :symbol "foo"
                                :instruction "increment x"
                                :text nil)))
              (let ((report (scalpel-agent-execute-action action)))
                (should (string-match "Edited foo" report))
                (with-current-buffer (find-file-noselect file)
                  (should (string= (buffer-string) "(defun foo (x)\n  (+ x 2))\n")))))))
      (when (get-file-buffer file)
        (with-current-buffer (get-file-buffer file)
          (set-buffer-modified-p nil))
        (kill-buffer (get-file-buffer file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest scalpel-agent-test-execute-action-reply ()
  "Execute a reply action and return its text."
  (let ((action (list :tool "reply"
                      :file nil
                      :symbol nil
                      :instruction nil
                      :text "Hello, world!")))
    (should (string= (scalpel-agent-execute-action action) "Hello, world!"))))

(ert-deftest scalpel-agent-test-execute-action-unknown ()
  "Unknown action tool returns a descriptive message."
  (let ((action (list :tool "unknown"
                      :file nil
                      :symbol nil
                      :instruction nil
                      :text nil)))
    (should (string-match "Unknown action" (scalpel-agent-execute-action action)))))

(ert-deftest scalpel-agent-test-edit-malformed ()
  "Malformed edit action (missing fields) signals user-error."
  (should-error
   (scalpel-agent-edit nil nil nil)
   :type 'error))

(ert-deftest scalpel-agent-test-edit-rejects-prose-response ()
  "When LLM returns prose instead of code, no edit is applied."
  (let ((file (make-temp-file "scalpel-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(defun foo (x)\n  (+ x 1))\n"))
          (cl-letf (((symbol-function 'scalpel-llm-request)
                     (lambda (&rest _ignore)
                       "There are no occurrences of `(+ x 1)` in the body.")))
            (should-error
             (scalpel-agent-edit file "foo" "replace x with y")
             :type 'user-error)
            (with-current-buffer (find-file-noselect file)
              (should (string= (buffer-string)
                               "(defun foo (x)\n  (+ x 1))\n")))))
      (when (get-file-buffer file)
        (with-current-buffer (get-file-buffer file)
          (set-buffer-modified-p nil))
        (kill-buffer (get-file-buffer file)))
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'scalpel-agent-test)
;;; scalpel-agent-test.el ends here
