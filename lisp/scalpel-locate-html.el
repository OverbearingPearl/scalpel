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

(defun scalpel-locate-html--top-definition-range (symbol)
  "Return (BEG . END) of the element whose id is SYMBOL.
BEG is the start tag's `<'; END is just past the matching end
tag.  Trailing blank lines are excluded.  Return nil when no
element in the buffer carries that id."
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
            (cons beg (point))))))))

(defun scalpel-locate-html-range (_file symbol)
  "Return the range of SYMBOL's element as (BEG . END).
Current buffer is the HTML file referenced by FILE."
  (scalpel-locate-html--top-definition-range symbol))

(defun scalpel-locate-html-list-symbols (_file)
  "Return the id attribute values of all elements in the buffer.
Elements named script or style are skipped together with their
raw-text bodies, so JavaScript or CSS text is never reported and
the ids of the script or style tags themselves are not collected."
  (save-excursion
    (goto-char (point-min))
    (let (ids)
      (while (re-search-forward
              scalpel-locate-html--tag-regexp nil t)
        (let* ((tag-beg (match-beginning 0))
               (tag-name (match-string 1))
               (id (scalpel-locate-html--tag-id tag-beg)))
          (if (member (downcase tag-name) '("script" "style"))
              (scalpel-locate-html--skip-raw-text tag-name)
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
       :single-definition-p #'scalpel-locate-html--single-definition-p))

(provide 'scalpel-locate-html)

;;; scalpel-locate-html.el ends here
