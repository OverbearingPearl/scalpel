;;; scalpel-execute-elisp-test.el --- Tests for scalpel-execute-elisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Emacs Lisp execute provider.

;;; Code:

(require 'ert)
(require 'scalpel-execute-elisp)

(defun scalpel-execute-elisp-test--beg-of (text)
  "Return the position at which TEXT begins its line.
Used to hand the provider the start of a definition's own range."
  (goto-char (point-min))
  (unless (search-forward text nil t)
    (ert-fail (format "Text %S not found in:\n%S" text (buffer-string))))
  (line-beginning-position))

(ert-deftest scalpel-execute-elisp-test-deletion-start-takes-the-cookie ()
  "The deletion range carries its autoload cookie upward."
  (with-temp-buffer
    (insert ";;;###autoload\n(defun foo ())\n")
    (let ((beg (scalpel-execute-elisp-test--beg-of "(defun foo ())")))
      (ert-info ((format "Start: %d"
                         (scalpel-execute-elisp--deletion-start beg)))
        (should (= (scalpel-execute-elisp--deletion-start beg) 1))))))

(ert-deftest scalpel-execute-elisp-test-deletion-start-takes-every-cookie ()
  "Every consecutive cookie line above the definition is carried."
  (with-temp-buffer
    (insert ";;;###autoload\n;;;###autoload\n(defun foo ())\n")
    (let ((beg (scalpel-execute-elisp-test--beg-of "(defun foo ())")))
      (should (= (scalpel-execute-elisp--deletion-start beg) 1)))))

(ert-deftest scalpel-execute-elisp-test-deletion-start-stops-at-code ()
  "A line that is not a cookie ends the run."
  (with-temp-buffer
    (insert "(defun previous ())\n;;;###autoload\n(defun foo ())\n")
    (let ((beg (scalpel-execute-elisp-test--beg-of "(defun foo ())")))
      (should (= (scalpel-execute-elisp--deletion-start beg)
                 (save-excursion
                   (goto-char beg)
                   (forward-line -1)
                   (point)))))))

(ert-deftest scalpel-execute-elisp-test-deletion-start-stops-at-blank-line ()
  "A blank line between the cookie and the definition ends the run."
  (with-temp-buffer
    (insert ";;;###autoload\n\n(defun foo ())\n")
    (let ((beg (scalpel-execute-elisp-test--beg-of "(defun foo ())")))
      (should (= (scalpel-execute-elisp--deletion-start beg) beg)))))

(ert-deftest scalpel-execute-elisp-test-deletion-start-keeps-mid-line ()
  "A range that does not begin its own line is returned unchanged."
  (with-temp-buffer
    (insert ";;;###autoload\n  (defun foo ())\n")
    (goto-char (point-min))
    (search-forward "(defun foo ())")
    (let ((beg (match-beginning 0)))
      (ert-info ((format "beg=%d" beg))
        (should-not (save-excursion (goto-char beg) (bolp)))
        (should (= (scalpel-execute-elisp--deletion-start beg) beg))))))

(ert-deftest scalpel-execute-elisp-test-deletion-start-without-cookie ()
  "A definition with no cookie above it comes back at its own start."
  (with-temp-buffer
    (insert "(defun foo ())\n")
    (let ((beg (scalpel-execute-elisp-test--beg-of "(defun foo ())")))
      (should (= (scalpel-execute-elisp--deletion-start beg) beg)))))

(provide 'scalpel-execute-elisp-test)

;;; scalpel-execute-elisp-test.el ends here
