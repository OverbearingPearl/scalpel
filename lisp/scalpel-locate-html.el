;;; scalpel-locate-html.el --- HTML locator provider for Scalpel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Structural locator for HTML buffers.  A "definition" is one
;; element carrying an id attribute: the start tag plus everything
;; up to its matching end tag.  The symbol is the id value.  Void
;; elements and self-closing tags locate to the start tag itself.
;;
;; This file is loaded by `scalpel-locate', which registers its
;; provider at load time; it must not be loaded standalone, because
;; the registration call below needs `scalpel-locate-register-provider'
;; to already be defined.

;;; Code:

(require 'subr-x)

(defconst scalpel-locate-html--void-elements
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link"
    "meta" "param" "source" "track" "wbr")
  "HTML void elements: parsed without any end tag.")

(defconst scalpel-locate-html--tag-regexp
  "<\\([A-Za-z][A-Za-z0-9-]*\\)"
  "Regexp matching a start tag opener, capturing the tag name.
A closing tag never matches: `/' is not a letter, so the opener
regexp cannot run past a `</'.")

(defconst scalpel-locate-html--id-regexp
  "[ \t\n\r][Ii][Dd][ \t\n\r]*=[ \t\n\r]*\\(\"[^\"]*\"\\|'[^']*'\\)"
  "Regexp matching an id attribute with a quoted value.
The value is group 1 including its quotes; use
`scalpel-locate-html--tag-id' to read it without them.")

(defun scalpel-locate-html--tag-id (pos)
  "Return the id attribute value of the tag starting at POS, or nil.
POS is at the tag's `<'.  The attribute region is scanned up to
the tag's closing `>', which may lie on a later line."
  (save-excursion
    (goto-char pos)
    (let ((end (save-excursion
                 (forward-char 1)
                 (and (search-forward ">" (+ (point) 4096) t)
                      (1- (point))))))
      (when end
        (save-match-data
          (when (re-search-forward scalpel-locate-html--id-regexp end t)
            (let ((quoted (match-string 1)))
              (substring quoted 1 (1- (length quoted))))))))))

(defun scalpel-locate-html--skip-raw-text (name)
  "From just past a NAME start tag, skip its raw-text body.
Only script and style hold raw text in HTML; their content is not
markup, so tag-looking text inside them must never be matched.
Point moves to just before the matching end tag, or to the end of
the buffer when the element never closes."
  (when (member (downcase name) '("script" "style"))
    (let ((case-fold-search t))
      (if (re-search-forward
           (concat "</" (regexp-quote name)) nil t)
          (goto-char (match-beginning 0))
        (goto-char (point-max))))))

(defun scalpel-locate-html--element-end (tag-name)
  "From point at a start tag's `<', return position past TAG-NAME's end.
For void elements and self-closing tags this is the start tag's
own closing `>'.  Comments are skipped as units.  Returns a
position just past the end of the buffer when the element never
closes."
  (save-excursion
    (let* ((case-fold-search t)
           (void (member (downcase tag-name)
                         scalpel-locate-html--void-elements))
           (gt (save-excursion
                 (forward-char 1)
                 (search-forward ">" (+ (point) 4096) t))))
      (cond
       ((null gt) (point-max))
       ;; Self-closing or void: the start tag is the whole element.
       ((or void (eq (char-after (- gt 2)) ?/)) gt)
       (t
        (goto-char gt)
        (let ((depth 1) (end nil))
          (while (and (not end) (not (eobp)))
            (cond
             ((looking-at-p "<!--")
              (unless (search-forward "-->" nil t)
                (setq end (point-max))))
             ((looking-at-p "</")
              (if (re-search-forward
                   "</\\([A-Za-z][A-Za-z0-9-]*\\)[ \t\r\n]*>"
                   (+ (point) 4096) t)
                  (progn
                    (when (string= (downcase (match-string 1))
                                   (downcase tag-name))
                      (setq depth (1- depth))
                      (when (zerop depth)
                        (setq end (point)))))
                (setq end (point-max))))
             ((looking-at-p "<")
              (let ((gt2 (save-excursion
                           (forward-char 1)
                           (search-forward ">" (+ (point) 4096) t))))
                (cond
                 ((null gt2) (setq end (point-max)))
                 (t
                  (when (and (looking-at
                              scalpel-locate-html--tag-regexp)
                             (string= (downcase (match-string 1))
                                      (downcase tag-name))
                             (not (eq (char-after (- gt2 2)) ?/)))
                    (setq depth (1+ depth)))
                  (goto-char gt2)
                  (when (looking-at scalpel-locate-html--tag-regexp)
                    (scalpel-locate-html--skip-raw-text
                     (match-string 1)))))))
             (t
              (if (search-forward "<" nil t)
                  (backward-char 1)
                (setq end (point-max))))))
          (or end (point-max))))))))

(defun scalpel-locate-html--balanced-p (text)
  "Return non-nil when the replaced TEXT is balanced HTML.
Rules match the element-end locator: comments are skipped whole;
void and self-closing tags do not change depth; script/style
bodies are raw text, so no tag opens inside them and their
closing tag is consumed without affecting depth.  Depth must
never go negative, and the scan must end at zero depth with no
unclosed comment.  Closing tags are recognized by a single local
tag regexp that matches an optional slash before the tag name;
each match is anchored by requiring its start to equal the scan
position, so closing tags are never skipped as ordinary
characters.  Capture group 1 holds the optional slash (a closing
tag), group 2 the tag name."
  (catch 'unbalanced
    (let ((pos 0)
          (depth 0)
          (raw-end nil)
          (tag-regexp "<\\(/\\)?\\([a-zA-Z][a-zA-Z0-9-]*\\)"))
      (while (< pos (length text))
        (cond
         ;; Inside a raw-text element: skip to its closing tag only.
         (raw-end
          (let ((case-fold-search t))
            (if (string-match raw-end text pos)
                (setq pos (match-end 0)
                      raw-end nil)
              (throw 'unbalanced nil))))
         ;; Comment starting exactly at POS: skip the whole block,
         ;; fail on unclosed comment.
         ((and (eq (aref text pos) ?<)
               (equal (string-match "<!--" text pos) pos))
          (let ((end (string-match "-->" text (match-end 0))))
            (if end
                (setq pos (match-end 0))
              (throw 'unbalanced nil))))
         ;; Tag (open or closing) starting exactly at POS: anchored
         ;; by requiring the match start to equal POS; classify and
         ;; update depth.
         ((equal (string-match tag-regexp text pos) pos)
          (let* ((name (downcase (match-string 2 text)))
                 (closing (and (match-string 1 text) t))
                 (self-closing
                  (eq (aref text (match-end 0)) ?/))
                 (void (member name
                               scalpel-locate-html--void-elements))
                 (self-closing
                  (or self-closing
                      (progn
                        (string-match
                         "[ \t\n\r]*/>" text (match-end 0))
                        (eq (match-end 0) (1+ (match-end 0)))))))
            (cond
             (closing
              (setq depth (1- depth))
              (when (< depth 0)
                (throw 'unbalanced nil)))
             ((or void self-closing))     ; no depth change
             (t
              (setq depth (1+ depth))
              (when (member name '("script" "style"))
                (setq raw-end
                      (concat "</" (regexp-quote name)
                              "[ \t\n\r]*>")))))
            ;; Advance past "<name" plus any attributes; find the
            ;; tag end to skip attributes safely.
            (let ((end (string-match ">" text (match-end 0))))
              (unless end
                (throw 'unbalanced nil))
              (setq pos (1+ end)))))
         ;; Ordinary character: advance one position.
         (t
          (setq pos (1+ pos)))))
      (zerop depth))))

(defun scalpel-locate-html--top-definition-range (symbol)
  "Return (BEG . END) of the element whose id is SYMBOL.
BEG is the start tag's `<'; END is just past the matching end
tag.  Trailing blank lines are excluded.  Return nil when no
element in the buffer carries that id.
When SYMBOL is dotted as scriptId.jsName, first resolve the
element whose id is scriptId, then locate the JS declaration of
jsName inside that element's body and return its full JS
definition (BEG . END), matching the semantics of ranges
produced for .js files."
  (if (string-match "\\`\\([^.]+\\)\\.\\([^.]+\\)\\'" symbol)
      (let* ((script-id (match-string 1 symbol))
             (js-name (match-string 2 symbol))
             (range (scalpel-locate-html--top-definition-range script-id)))
        (when range
          (save-excursion
            (save-restriction
              (narrow-to-region (car range) (cdr range))
              (goto-char (point-min))
              (let (found)
                (while (and (not found)
                            (re-search-forward
                             scalpel-locate-js--decl-regexp nil t))
                  (let ((name (or (match-string 1) (match-string 2))))
                    (when (and name (string= name js-name))
                      (setq found
                            (cons (match-beginning 0)
                                  (scalpel-locate-js--definition-end))))))
                found)))))
    (save-excursion
      (goto-char (point-min))
      (let (beg name)
        (while (and (not beg)
                    (re-search-forward
                     scalpel-locate-html--tag-regexp nil t))
          (let* ((tag-beg (match-beginning 0))
                 (tag-name (match-string 1))
                 (id (scalpel-locate-html--tag-id tag-beg)))
            (when (and id (string= id symbol))
              (setq beg tag-beg name tag-name))
            (unless beg
              (scalpel-locate-html--skip-raw-text tag-name))))
        (when beg
          (goto-char beg)
          (let ((end (scalpel-locate-html--element-end name)))
            (goto-char end)
            (skip-chars-backward " \t\n\r")
            (if (< (point) beg)
                (cons beg (1+ beg))
              (cons beg (point)))))))))

(defun scalpel-locate-html-range (_file symbol)
  "Return the range of SYMBOL's element as (BEG . END).
Current buffer is the HTML file referenced by FILE."
  (scalpel-locate-html--top-definition-range symbol))

(defun scalpel-locate-html-list-symbols (_file)
  "Return the id attribute values of all elements in the buffer.
Script and style bodies are skipped so their raw text is never
scanned for ids, but the ids of the script or style tags
themselves are collected, making them locatable definitions.
For scripts carrying an id, the body is scanned by the JS
scanner and each JS name is emitted as scriptId.name."
  (save-excursion
    (goto-char (point-min))
    (require 'scalpel-locate-js)
    (let (ids)
      (while (re-search-forward
              scalpel-locate-html--tag-regexp nil t)
        (let* ((tag-beg (match-beginning 0))
               (tag-name (match-string 1))
               (id (scalpel-locate-html--tag-id tag-beg)))
          (if (member (downcase tag-name) '("script" "style"))
              (progn
                (when id (push id ids))
                (when (and id (string= (downcase tag-name) "script"))
                  (let ((body-beg (point)))
                    (scalpel-locate-html--skip-raw-text tag-name)
                    (let* ((body-end (point))
                           (body-text (and (> body-end body-beg)
                                           (buffer-substring
                                            body-beg body-end))))
                      (when body-text
                        (with-temp-buffer
                          (insert body-text)
                          (dolist (name (car (scalpel-locate-js--scan-region
                                              (point-min) (point-max))))
                            (push (concat id "." name) ids)))))))
                (unless (string= (downcase tag-name) "script")
                  (scalpel-locate-html--skip-raw-text tag-name)))
            (when id (push id ids)))))
      (nreverse ids))))

(defun scalpel-locate-html--single-definition-p (text)
  "Return non-nil when TEXT is exactly one element with one id.
TEXT must carry exactly one id attribute, and only whitespace may
follow the element's matching end tag, so a replacement can only
ever land on one element."
  (let ((count 0) (pos 0))
    (save-match-data
      (while (string-match scalpel-locate-html--id-regexp text pos)
        (setq count (1+ count)
              pos (match-end 0))))
    (and (= count 1)
         (with-temp-buffer
           (insert text)
           (goto-char (point-min))
           (when (re-search-forward
                  scalpel-locate-html--tag-regexp nil t)
             (let* ((beg (match-beginning 0))
                    (name (match-string 1)))
               (goto-char beg)
               (string-blank-p
                (buffer-substring-no-properties
                 (scalpel-locate-html--element-end name)
                 (point-max)))))))))

;; Register with `scalpel-locate' at load time; see the Commentary.
(scalpel-locate-register-provider
 "\\.x?html?\\'"
 (list :locate #'scalpel-locate-html-range
       :list-symbols #'scalpel-locate-html-list-symbols
       :single-definition-p #'scalpel-locate-html--single-definition-p
        :balanced-p #'scalpel-locate-html--balanced-p))

(provide 'scalpel-locate-html)

;;; scalpel-locate-html.el ends here
