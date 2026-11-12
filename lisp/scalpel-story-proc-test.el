;;; scalpel-story-proc-test.el --- User-visible subprocess stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the subprocess runner.  The property
;; under test is that a child process can never take Emacs with it: a
;; command that answers returns its output, one that never answers is
;; killed at the deadline and reported with a `timeout' status, one that
;; floods its output is killed at the byte cap, and the journal holds a
;; paired spawn and terminal line for each run so a force-killed Emacs
;; still names the culprit.  The excerpt scan behind the substitute
;; report is covered here too, because a planner-written pattern that
;; matches the empty string used to make that scan loop forever.
;;
;; Every story redirects `scalpel-proc-log-file' and
;; `scalpel-proc-diagnostics-directory' into its own temporary directory
;; through plain `:given' bindings, so no story reads or writes the
;; journal a real freeze investigation depends on.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-proc)
(require 'scalpel-agent)

(defvar scalpel-story-proc-test-result nil
  "Plist produced by the most recent runner story.")

(defvar scalpel-story-proc-test-lines nil
  "Excerpt lines produced by the most recent scan story.")

(defun scalpel-story-proc-test--journal-lines ()
  "Return the journal written by the current story as a list of lines."
  (when (file-exists-p scalpel-proc-log-file)
    (with-temp-buffer
      (insert-file-contents scalpel-proc-log-file)
      (split-string (buffer-string) "\n" t))))

(defun scalpel-story-proc-test--journal-holds-p (needle)
  "Return non-nil when NEEDLE appears in the current story's journal."
  (cl-some (lambda (line) (string-search needle line))
           (scalpel-story-proc-test--journal-lines)))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-proc-" t))
           (scalpel-proc-log-file (expand-file-name "journal.log" dir))))
  (:when (setq scalpel-story-proc-test-result
               (scalpel-proc-run "sh" '("-c" "printf out")
                                 :timeout 10 :label "story-exit")))
  (:then (should (equal (plist-get scalpel-story-proc-test-result :status) 0)))
  (:then (should (equal (plist-get scalpel-story-proc-test-result :output)
                        "out")))
  (:then (should (scalpel-story-proc-test--journal-holds-p "proc spawn #")))
  (:then (should (scalpel-story-proc-test--journal-holds-p "proc exit #")))
  (:cleanup (delete-directory dir t)))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-proc-" t))
           (scalpel-proc-log-file (expand-file-name "journal.log" dir))
           (scalpel-proc-diagnostics-directory
            (expand-file-name "diag" dir))))
  (:when (setq scalpel-story-proc-test-result
               (scalpel-proc-run "sleep" '("60")
                                 :timeout 1 :label "story-hang")))
  (:then (should (scalpel-proc-timeout-p scalpel-story-proc-test-result)))
  (:then (should (< (plist-get scalpel-story-proc-test-result :elapsed)
                    10)))
  (:then (should (scalpel-story-proc-test--journal-holds-p
                  "proc verdict #")))
  (:then (should (scalpel-story-proc-test--journal-holds-p
                  "advice #: while it is still stuck")))
  (:cleanup (delete-directory dir t)))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-proc-" t))
           (scalpel-proc-log-file (expand-file-name "journal.log" dir))
           (scalpel-proc-max-output-bytes 1000)))
  (:when (setq scalpel-story-proc-test-result
               (scalpel-proc-run "sh"
                                 '("-c" "yes aaaaaaaaaa | head -c 200000")
                                 :timeout 10 :label "story-flood")))
  (:then (should (scalpel-proc-capped-p scalpel-story-proc-test-result)))
  (:then (should (<= (length (plist-get scalpel-story-proc-test-result
                                        :output))
                     1000)))
  (:cleanup (delete-directory dir t)))

(ert-gwt-deftest
  (:given ((text "foo bar\nbaz foo")))
  (:when (setq scalpel-story-proc-test-lines
               (scalpel-agent--match-lines text "\\b" 5)))
  (:then (should (<= (length scalpel-story-proc-test-lines) 5)))
  (:then (should (> (length scalpel-story-proc-test-lines) 0))))

(ert-gwt-deftest
  (:given ((text "foo bar\nbaz foo")))
  (:when (setq scalpel-story-proc-test-lines
               (scalpel-agent--match-lines text "^" 5)))
  (:then (should (= (length scalpel-story-proc-test-lines) 2))))

(provide 'scalpel-story-proc-test)

;;; scalpel-story-proc-test.el ends here
