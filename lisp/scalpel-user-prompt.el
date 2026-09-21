;;; scalpel-user-prompt.el --- User prompts from ~/.scalpel for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: DeepSeek:deepseek-v4-flash, GLM:glm-5.3-flash, Laguna:laguna-s-2.1
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; User-supplied prompt fragments read from ~/.scalpel.  A fragment
;; is a file named prompt.VALUE; it is attached when VALUE matches
;; one of three dimensions of the file being worked on: the git
;; author name of the repo holding the file, the repository name,
;; or the file's programming language.  Everything resolves at the
;; granularity of a single file, because Scalpel's working directory
;; need not be a repo root and a round may touch several repos and
;; languages at once: every matching fragment applies, deduplicated,
;; each once.

;;; Code:

(require 'subr-x)
(require 'scalpel-prompt)

(defcustom scalpel-user-prompt-dir (expand-file-name "~/.scalpel/")
  "Directory holding user-supplied prompt files.
A file prompt.VALUE is attached when VALUE matches the git author,
the repository name or the language of the file an action targets."
  :type 'directory
  :group 'scalpel)

(defcustom scalpel-user-prompt-language-alist
  '(("el" . "elisp")
    ("md" . "markdown")
    ("yml" . "yaml")
    ("yaml" . "yaml")
    ("py" . "python")
    ("c" . "c")
    ("h" . "c")
    ("cc" . "c++")
    ("cpp" . "c++")
    ("rs" . "rust")
    ("go" . "go")
    ("js" . "javascript")
    ("ts" . "typescript")
    ("sh" . "shell")
    ("json" . "json")
    ("org" . "org"))
  "Map a file extension to the language name used in a prompt file name.
A user writes prompt.ELISP to cover Emacs Lisp files; the extension
of the target file is translated through this alist."
  :type '(alist :key-type string :value-type string)
  :group 'scalpel)

(defun scalpel-user-prompt--git-value (dir args)
  "Run git with ARGS in DIR; return the trimmed stdout, or nil.
Nil covers both a failing git call and a repo outside git, so
callers can treat an absent value and an error the same way."
  (with-temp-buffer
    (let ((default-directory dir))
      (and (zerop (apply #'call-process "git" nil t nil args))
           (not (string-empty-p (buffer-string)))
           (string-trim (buffer-string))))))

(defun scalpel-user-prompt--author (dir)
  "Return the git user name configured for DIR's repo, or nil."
  (scalpel-user-prompt--git-value dir '("config" "user.name")))

(defun scalpel-user-prompt--repo-name (dir)
  "Return the repository name for DIR, or nil outside a repo.
The name is taken from the origin remote when one exists, with a
.git suffix stripped, and falls back to the checkout directory's
base name."
  (or (let ((url (scalpel-user-prompt--git-value
                  dir '("remote" "get-url" "origin"))))
        (when url
          (file-name-nondirectory
           (replace-regexp-in-string "\\.git\\'" "" url))))
      (let ((top (scalpel-user-prompt--git-value
                  dir '("rev-parse" "--show-toplevel"))))
        (when top
          (file-name-nondirectory top)))))

(defun scalpel-user-prompt--language (file)
  "Return the language name for FILE's extension, or nil."
  (alist-get (downcase (or (file-name-extension file) ""))
             scalpel-user-prompt-language-alist
             nil nil #'string=))

(defun scalpel-user-prompt--file-name (value)
  "Return the prompt file base name for VALUE, or nil.
Nil VALUE and an empty one both yield nil, so an unmatched
dimension simply contributes no candidate."
  (and value (not (string-empty-p value))
       (concat "prompt." value)))

(defun scalpel-user-prompt--read (name)
  "Return the contents of the prompt file NAME under the prompt dir.
Return nil when it does not exist."
  (let ((file (expand-file-name name scalpel-user-prompt-dir)))
    (message "Scalpel: Reading prompt file: %s" file)
    (and (file-regular-p file)
         (file-readable-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string)))))

(defun scalpel-user-prompt--names-for-dir (dir)
  "Return the git-derived prompt file names matching DIR.
DIR need not be a repo root: git is asked directly in DIR, so the
lookup works from any working directory inside the repo."
  (delq nil
        (list (scalpel-user-prompt--file-name
               (scalpel-user-prompt--author dir))
              (scalpel-user-prompt--file-name
               (scalpel-user-prompt--repo-name dir)))))

(defun scalpel-user-prompt-for-file (file)
  "Return the user prompt text matching FILE, or nil.
All three dimensions are consulted: the author and repository of
the repo holding FILE, and FILE's language.  Matching contents are
joined with a blank line; each matching file contributes once even
when several dimensions name the same file."
  (when file
    (let ((names (delete-dups
                  (append
                   (scalpel-user-prompt--names-for-dir
                    (file-name-directory file))
                   (list (scalpel-user-prompt--file-name
                          (scalpel-user-prompt--language file)))))))
      (let ((joined (string-join
                     (delq nil (mapcar #'scalpel-user-prompt--read names))
                     "\n\n")))
        (and (not (string-empty-p joined)) joined)))))

(defun scalpel-user-prompt-for-files (files)
  "Return the union of user prompt text for FILES, or nil.
Git identity is resolved once per directory, so a round touching
several files of one repo runs git once.  A round spanning several
languages or several repos attaches every matching prompt, each
deduplicated to once -- that is the answer to a mixed edit: all
matching fragments apply together."
  (when files
    (let ((names
           (delete-dups
            (append
             (apply #'append
                    (delq nil
                          (mapcar #'scalpel-user-prompt--names-for-dir
                                  (delete-dups
                                   (delq nil (mapcar #'file-name-directory files))))))
             (apply #'append
                    (delq nil
                          (mapcar (lambda (file)
                                    (list (scalpel-user-prompt--file-name
                                           (scalpel-user-prompt--language file))))
                                  files)))))))
      (let ((joined (string-join
                     (delq nil (mapcar #'scalpel-user-prompt--read names))
                     "\n\n")))
        (and (not (string-empty-p joined)) joined)))))

(defun scalpel-user-prompt-with-language-rule (file)
  "Combine the registered language rule and the user prompt text for FILE.
This is the text appended to a per-file replacement or creation
round; either piece may be absent."
  (let ((rule (scalpel-prompt-language-rule-for-file file))
        (user (scalpel-user-prompt-for-file file)))
    (cond ((and rule user) (concat rule "\n\n" user))
          (rule)
          (user))))

(provide 'scalpel-user-prompt)

;;; scalpel-user-prompt.el ends here
