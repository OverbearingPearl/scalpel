;;; scalpel-story-user-prompt-test.el --- User prompt stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the user prompt layer.  The base
;; prompt file is always attached, a language prompt is attached for a
;; matching extension, both land joined in a deterministic order, and
;; an empty directory contributes nothing.  A prompt file is found
;; under its plain name or its .md fallback, a missing one yields nil,
;; and the combined rule-plus-user text degrades to whichever piece
;; exists.  The prompt dir is bound to a temp directory so no real
;; ~/.scalpel content leaks in; git lookups in the temp dir simply
;; miss, which keeps the stories deterministic.
;; Every clause is a single form and every THEN one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-user-prompt)

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-" t))
           (_ (setq scalpel-user-prompt-dir dir))
           (_ (with-temp-file (expand-file-name "prompt" dir)
                (insert "base prompt")))
           (path (expand-file-name "note.el" dir))
           (result nil)))
  (:when (setq result (scalpel-user-prompt-for-file path)))
  (:then (should (string-search "base prompt" result)))
  (:cleanup (delete-directory dir t)
            (setq scalpel-user-prompt-dir
                  (expand-file-name "~/.scalpel/"))))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-" t))
           (_ (setq scalpel-user-prompt-dir dir))
           (path (expand-file-name "note.el" dir))
           (result 'unset)))
  (:when (setq result (scalpel-user-prompt-for-file path)))
  (:then (should (not result)))
  (:cleanup (delete-directory dir t)
            (setq scalpel-user-prompt-dir
                  (expand-file-name "~/.scalpel/"))))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-" t))
           (_ (setq scalpel-user-prompt-dir dir))
           (_ (with-temp-file (expand-file-name "prompt.elisp" dir)
                (insert "elisp rules")))
           (path (expand-file-name "note.el" dir))
           (result nil)))
  (:when (setq result (scalpel-user-prompt-for-file path)))
  (:then (should (string-search "elisp rules" result)))
  (:cleanup (delete-directory dir t)
            (setq scalpel-user-prompt-dir
                  (expand-file-name "~/.scalpel/"))))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-" t))
           (_ (setq scalpel-user-prompt-dir dir))
           (_ (with-temp-file (expand-file-name "prompt" dir)
                (insert "base")))
           (_ (with-temp-file (expand-file-name "prompt.elisp" dir)
                (insert "lang")))
           (path (expand-file-name "note.el" dir))
           (result nil)))
  (:when (setq result (scalpel-user-prompt-for-file path)))
  (:then (should (string-search "base" result)))
  (:then (should (string-search "lang" result)))
  (:cleanup (delete-directory dir t)
            (setq scalpel-user-prompt-dir
                  (expand-file-name "~/.scalpel/"))))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-" t))
           (_ (setq scalpel-user-prompt-dir dir))
           (_ (with-temp-file (expand-file-name "prompt.zoo.md" dir)
                (insert "from md")))
           (result nil)))
  (:when (setq result (scalpel-user-prompt--read "prompt.zoo")))
  (:then (should (string-search "from md" result)))
  (:then (should (string-search "prompt.zoo.md" result)))
  (:cleanup (delete-directory dir t)
            (setq scalpel-user-prompt-dir
                  (expand-file-name "~/.scalpel/"))))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-" t))
           (_ (setq scalpel-user-prompt-dir dir))
           (result 'unset)))
  (:when (setq result (scalpel-user-prompt--read "prompt.absent")))
  (:then (should (not result)))
  (:cleanup (delete-directory dir t)
            (setq scalpel-user-prompt-dir
                  (expand-file-name "~/.scalpel/"))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result (list (scalpel-user-prompt--file-name nil)
                            (scalpel-user-prompt--file-name "")
                            (scalpel-user-prompt--file-name
                             "elisp"))))
  (:then (should (equal result
                        (list nil nil "prompt.elisp"))))
  (:cleanup nil))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result (list (scalpel-user-prompt--language "a.el")
                            (scalpel-user-prompt--language "a.YML")
                            (scalpel-user-prompt--language
                             "a.unknown"))))
  (:then (should (equal result (list "elisp" "yaml" nil))))
  (:cleanup nil))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-" t))
           (_ (setq scalpel-user-prompt-dir dir))
           (_ (with-temp-file (expand-file-name "prompt" dir)
                (insert "user text")))
           (path (expand-file-name "note.el" dir))
           (result nil)))
  (:when (setq result
               (scalpel-user-prompt-with-language-rule path)))
  (:then (should (string-search "user text" result)))
  (:cleanup (delete-directory dir t)
            (setq scalpel-user-prompt-dir
                  (expand-file-name "~/.scalpel/"))))

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-" t))
           (_ (setq scalpel-user-prompt-dir dir))
           (path (expand-file-name "note.unknown" dir))
           (result 'unset)))
  (:when (setq result
               (scalpel-user-prompt-with-language-rule path)))
  (:then (should (not result)))
  (:cleanup (delete-directory dir t)
            (setq scalpel-user-prompt-dir
                  (expand-file-name "~/.scalpel/"))))

(provide 'scalpel-story-user-prompt-test)

;;; scalpel-story-user-prompt-test.el ends here
