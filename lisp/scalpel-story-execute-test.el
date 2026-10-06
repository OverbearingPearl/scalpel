;;; scalpel-story-execute-test.el --- Block execution stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for block replacement.  I replace a
;; region and the balanced text lands exactly there; an unbalanced
;; replacement is refused outright; an insertion after a definition
;; sits on its own lines with one blank line on each side; deleting a
;; block joins its neighbours without leaving stacked blank lines; and
;; an Emacs Lisp deletion carries the autoload cookie above the form
;; with it.  Every clause is a single form and every THEN one
;; `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-execute)
(require 'scalpel-execute-elisp)

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (setq result
                 (condition-case err
                     (progn
                       (with-temp-buffer
                         (insert "abc")
                         (scalpel-execute-replace 1 4 "(("))
                       nil)
                   (user-error (error-message-string err))))))
  (:then (should (string-search "unbalanced" result))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (setq result
                 (with-temp-buffer
                   (insert "hello world")
                   (scalpel-execute-replace 7 12 "there")
                   (buffer-string)))))
  (:then (should (string= result "hello there"))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (setq result
                 (with-temp-buffer
                   (insert "aaa\nbbb\n")
                   (scalpel-execute-insert-after 4 "ccc")
                   (buffer-string)))))
  (:then (should (string= result "aaa\n\nccc\n\nbbb\n"))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (setq result
                 (with-temp-buffer
                   (insert "a1\na2\n")
                   (scalpel-execute-delete 1 3)
                   (buffer-string)))))
  (:then (should (string= result "a2\n"))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (setq result
                 (with-temp-buffer
                   (insert ";;;###autoload\n(defun f ())\n(x)\n")
                   (scalpel-execute-elisp--deletion-start 16)))))
  (:then (should (= result 1))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result (scalpel-execute-provider-for-file "foo.el")))
  (:then (should (functionp (plist-get result :deletion-start)))))

(provide 'scalpel-story-execute-test)

;;; scalpel-story-execute-test.el ends here
