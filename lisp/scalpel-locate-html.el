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

(defun scalpel-locate-html--js-entry-range (symbol)
  "Return the range of entry KEY in the top-level JS object NAME.
SYMBOL is the dot form `NAME.key' in the current HTML buffer, and
may be given either as a symbol or as a string.
The object must be declared at top level of an inline <script>
element carrying no id attribute (and no src), as either
`const/let/var NAME = {' or a bare `NAME = {' assignment.  Raw
text of script and style elements outside such inline scripts is
never scanned, so `scriptId.jsName' style references cannot be
confused with object entries.
Returns (BEG . END), where BEG is the start of the declaration
head and END is the end of the located entry with trailing
whitespace removed; nil when the object declaration or the key is
not found."
  (require 'scalpel-locate-js)
  (let* ((raw (if (symbolp symbol) (symbol-name symbol) symbol))
         (parts (split-string raw "\\."))
         (name (string-join (butlast parts) "."))
         (key (car (last parts)))
         (decl-re
          (concat "\\<\\(?:\\(?:const\\|let\\|var\\)[ \t\r\n]+\\)?"
                  (regexp-quote name)
                  "[ \t\r\n]*=[ \t\r\n]*{")))
    (when (and (> (length parts) 1) (not (string-empty-p name)))
      (save-excursion
        (goto-char (point-min))
        (catch 'found
          (while (re-search-forward "<\\(script\\|style\\)\\b[^>]*>" nil t)
            (let* ((tag-name (downcase (match-string 1)))
                   (attrs (substring (match-string 0) 0 -1))
                   (body-beg (match-end 0))
                   (body-end
                    (save-excursion
                      (if (re-search-forward
                           (concat "</" (regexp-quote (match-string 1)))
                           nil t)
                          (match-beginning 0)
                        (point-max)))))
              (when (and (string= tag-name "script")
                         (not (string-match-p "\\bid[ \t\r\n]*=" (downcase attrs)))
                         (not (string-match-p "\\bsrc[ \t\r\n]*=" (downcase attrs))))
                (goto-char body-beg)
                (when (re-search-forward decl-re body-end t)
                  (let ((entry (save-excursion
                                 (goto-char (match-end 0))
                                 (scalpel-locate-js--object-entry-range
                                  name key))))
                    (when entry
                      (throw 'found
                             (cons (match-beginning 0)
                                   (save-excursion
                                     (goto-char (cdr entry))
                                     (skip-chars-backward " \t\r\n" (car entry))
                                     (point))))))))))
          nil)))))

(defun scalpel-locate-html--script-declaration-range (symbol)
  "Return the (BEG . END) range of dotted SYMBOL's JS declaration.
The declaration lives in the script element whose id is SYMBOL's
prefix.
The prefix names a script element carrying an id attribute; the
body is scanned with the JS locator, so function, class and
const/let/var declarations are all recognized.  Return nil when
the script element or the declaration cannot be found."
  (let* ((parts (split-string
                 (if (symbolp symbol) (symbol-name symbol) symbol)
                 "\\."))
         (script-id (nth 0 parts))
         (js-name (nth 1 parts)))
    (when (and script-id js-name)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               (concat "<script\\(?:[ \t\n]+[^<>]*\\)?"
                       "[ \t\n]id[ \t\n]*=[ \t\n]*\\(['\"]\\)"
                       (regexp-quote script-id) "\\1")
               nil t)
          (let ((body-beg (point))
                (body-end
                 (save-excursion
                   (scalpel-locate-html--skip-raw-text "script")
                   (point))))
            (when (> body-end body-beg)
              (let* ((body-text
                      (buffer-substring-no-properties
                       body-beg body-end))
                     (range
                      (with-temp-buffer
                        (insert body-text)
                        (scalpel-locate-js-range nil js-name))))
                (when range
                  (cons (+ body-beg (car range) -1)
                        (+ body-beg (cdr range) -1)))))))))))

(defun scalpel-locate-html--js--skip-noise ()
  "Skip over a comment, string, or template literal at point.
Return non-nil when such a token was consumed; the point then
sits right after it."
  (cond
   ((looking-at "//")
    (search-forward "\n" nil t))
   ((looking-at "/\\*")
    (search-forward "*/" nil t))
   ((looking-at "[\"']")
    (let ((quote (char-after))
          (closed nil))
      (forward-char)
      (while (and (not closed) (not (eobp)))
        (cond
         ((eq (char-after) ?\\) (forward-char 2))
         ((eq (char-after) quote) (forward-char) (setq closed t))
         ((eq (char-after) ?\n) (setq closed t))
         (t (forward-char))))
      t))
   ((eq (char-after) ?`)
    (forward-char)
    (while (and (not (eobp)) (not (eq (char-after) ?`)))
      (if (eq (char-after) ?\\)
          (forward-char 2)
        (forward-char)))
    (unless (eobp) (forward-char))
    t)
   (t nil)))

(defun scalpel-locate-html--js--skip-ws ()
  "Advance over whitespace at point."
  (while (and (not (eobp))
              (memq (char-after) '(?\s ?\t ?\n ?\r)))
    (forward-char)))

(defun scalpel-locate-html--js--scan-object-keys (name result)
  "Enumerate the keys of the object literal assigned to NAME.
Point sits before the opening brace; the keys declared at that
object's own depth, pushing NAME.key onto RESULT.  Keys are
recognized solely by string matching of identifiers or quoted
strings, so numeric positions never become key names; values are
skipped wholesale (strings, template literals and comments cannot
disturb the scan) and nested objects are not entered."
  (forward-char)                       ; enter the opening brace
  (let ((depth 1)
        (expect t)
        (key-re
         (concat
          "\\(?:\\([A-Za-z_$][A-Za-z0-9_$]*\\)"
          "\\|'\\([^']*\\)'"
          "\\|\"\\([^\"]*\\)\"\\)"
          "\\s-*[:=,}]")))
    (while (and (not (eobp)) (> depth 0))
      (cond
       ((scalpel-locate-html--js--skip-noise))
       ((memq (char-after) '(?\{ ?\[ ?\())
        (setq depth (1+ depth))
        (forward-char))
       ((memq (char-after) '(?\} ?\] ?\)))
        (setq depth (1- depth))
        (forward-char))
       ((and (= depth 1) expect (looking-at key-re))
        (push (concat name "."
                      (or (match-string 1)
                          (match-string 2)
                          (match-string 3)))
              result)
        (goto-char (match-end 0))
        ;; a key followed by `:` or `=` carries a value; the generic
        ;; branches below consume it, so stop expecting keys until the
        ;; next comma at the object's own depth.
        (setq expect nil))
       ((eq (char-after) ?\,)
        (setq expect (= depth 1))
        (forward-char))
       (t (forward-char))))
    result))

(defun scalpel-locate-html--js-inline-top-level-decls (text)
  "Return the top-level declared names in inline script TEXT.
For every top-level declaration whose initializer is an object
literal, the keys enumerated at that object's own depth are
appended as NAME.key entries.  Comments, strings and template
literals are skipped so they cannot produce spurious names; keys
are matched as strings only, never derived from numeric positions."
  (let ((result nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((depth 0)
            (decl-re
             (concat
              "\\_<\\(?:const\\|let\\|var\\|function\\|class\\)\\_>"
              "\\s-*\\*?\\s-*"
              "\\([A-Za-z_$][A-Za-z0-9_$]*\\)")))
        (while (not (eobp))
          (cond
           ((scalpel-locate-html--js--skip-noise))
           ((eq (char-after) ?\{)
            (setq depth (1+ depth))
            (forward-char))
           ((eq (char-after) ?\})
            (setq depth (1- depth))
            (forward-char))
           ((and (= depth 0) (looking-at decl-re))
            (let* ((name (match-string 1))
                   (plain-decl
                    (string-match-p
                     "\\`\\(?:const\\|let\\|var\\)"
                     (match-string 0))))
              (goto-char (match-end 0))
              (if (not plain-decl)
                  (push name result)
                ;; Variable declaration: walk its declarator list.
                (let ((more t) (pending t))
                  (while more
                    (cond
                     ((scalpel-locate-html--js--skip-noise))
                     ((looking-at "\\s-*=")
                      (goto-char (match-end 0))
                      (push name result)
                      (setq pending nil)
                      (scalpel-locate-html--js--skip-ws)
                      (if (eq (char-after) ?\{)
                          (setq result
                                (scalpel-locate-html--js--scan-object-keys
                                 name result))
                        ;; Skip the initializer up to a declarator
                        ;; separator, keeping brackets balanced.
                        (let ((vdepth 0) (stop nil))
                          (while (and (not stop) (not (eobp)))
                            (cond
                             ((scalpel-locate-html--js--skip-noise))
                             ((memq (char-after) '(?\{ ?\[ ?\())
                              (setq vdepth (1+ vdepth))
                              (forward-char))
                             ((memq (char-after) '(?\} ?\] ?\)))
                              (setq vdepth (1- vdepth))
                              (forward-char))
                             ((and (= vdepth 0)
                                   (memq (char-after) '(?\; ?\, ?\n)))
                              (setq stop t))
                             (t (forward-char)))))))
                     ((looking-at "\\s-*\\([,;]\\)")
                      (when pending (push name result))
                      (setq pending nil)
                      (if (string= (match-string 1) ",")
                          (progn
                            (goto-char (match-end 0))
                            (scalpel-locate-html--js--skip-noise)
                            (scalpel-locate-html--js--skip-ws)
                            (if (looking-at "[A-Za-z_$][A-Za-z0-9_$]*")
                                (setq name (match-string 0)
                                      pending t)
                              (setq more nil)))
                        (setq more nil)))
                     (t (setq more nil))))))))
           (t (forward-char))))))
    (nreverse result)))

(defun scalpel-locate-html--top-definition-range (symbol)
  "Locate the top-level definition range for SYMBOL in the current HTML buffer.
SYMBOL may be a symbol or a string.  A non-dotted SYMBOL resolves to the
element bearing that id.  A dotted SYMBOL first resolves its prefix: when
the prefix names a script element carrying an id, the original
scriptId.jsName declaration scan applies; otherwise
`scalpel-locate-html--js-entry-range' resolves the NAME.key object entry,
falling back to the script declaration scan when no object entry matches.
Return a cons (BEG . END) or nil when SYMBOL cannot be located."
  (let ((parts (split-string
                (if (symbolp symbol) (symbol-name symbol) symbol)
                "\\."))
        (case-fold-search nil))
    (save-excursion
      (save-restriction
        (widen)
        (cond
         ;; Non-dotted symbol: locate the element with matching id by
         ;; scanning tag by tag so void/img elements and raw-text
         ;; elements get exact boundaries.
         ((= (length parts) 1)
          (let ((id (regexp-quote (car parts))))
            (goto-char (point-min))
            (catch 'found
              (while (re-search-forward
                      scalpel-locate-html--tag-regexp nil t)
                (let* ((tag-beg (match-beginning 0))
                       (tag-name (save-excursion
                                   (goto-char tag-beg)
                                   (if (looking-at
                                        "<\\([[:alpha:]][[:alnum:]:_-]*\\)")
                                       (match-string 1)
                                     nil))))
                  (when tag-name
                    (if (equal (scalpel-locate-html--tag-id tag-beg) id)
                        (let ((end (scalpel-locate-html--element-end
                                    tag-name)))
                          (when end
                            (goto-char end)
                            (skip-chars-backward " \t\r\n")
                            (throw 'found
                                   (cons tag-beg (point)))))
                      (scalpel-locate-html--skip-raw-text
                       tag-name))))))))
         ;; Dotted symbol: resolve the prefix first.
         ((= (length parts) 2)
          (let ((prefix (nth 0 parts)))
            (goto-char (point-min))
            (if (re-search-forward
                 (concat "<script\\(?:[ \t\n]+[^<>]*\\)?"
                         "[ \t\n]id[ \t\n]*=[ \t\n]*\\(['\"]\\)"
                         (regexp-quote prefix) "\\1")
                 nil t)
                ;; Prefix is an id'd script element: scriptId.jsName scan.
                (scalpel-locate-html--script-declaration-range symbol)
              ;; Otherwise resolve NAME.key object entry, falling back to
              ;; the script declaration scan when no entry matches.
              (or (scalpel-locate-html--js-entry-range symbol)
                  (scalpel-locate-html--script-declaration-range symbol))))))))))

(defun scalpel-locate-html-range (_file symbol)
  "Return the range of SYMBOL's element as (BEG . END).
Current buffer is the HTML file referenced by FILE."
  (scalpel-locate-html--top-definition-range symbol))

(defun scalpel-locate-html-list-symbols (_file)
  "Return the id attribute values of all elements in the buffer.
The bodies of script and style elements are always skipped so
their raw text is never mistaken for ids, while the ids of the
script or style tags themselves are still collected, making them
locatable definitions.  For scripts carrying an id, the JS
scanner runs over the body and each JS name is emitted as
scriptId.name; scripts without an id contribute no JS names.

For inline scripts without an id attribute, the body is still
skipped as raw text, but its top-level declarations are scanned
via \`scalpel-locate-html--js-inline-top-level-decls', which yields
top-level const/let/var names together with each key of their
top-level object literal as NAME.key; nested objects are not
descended into.  Scripts with an id keep their existing behavior
and do not emit NAME.key symbols."
  (save-excursion
    (goto-char (point-min))
    (require 'scalpel-locate-js)
    (let (ids)
      (while (re-search-forward
              scalpel-locate-html--tag-regexp nil t)
        (let* ((tag-beg (match-beginning 0))
               (tag-name (match-string 1))
               (tag-type (downcase tag-name))
               (id (scalpel-locate-html--tag-id tag-beg)))
          (if (member tag-type '("script" "style"))
              (progn
                (when id (push id ids))
                (let ((body-beg (point)))
                  (scalpel-locate-html--skip-raw-text tag-name)
                  (let* ((body-end (point))
                         (body-text (and (> body-end body-beg)
                                         (buffer-substring
                                          body-beg body-end))))
                    (when body-text
                      (if (and id (string= tag-type "script"))
                          (with-temp-buffer
                            (insert body-text)
                            (dolist (name (car (scalpel-locate-js-scan-region
                                                (point-min) (point-max))))
                              (push (concat id "." name) ids)))
                        (when (string= tag-type "script")
                          (dolist (name
                                   (scalpel-locate-html--js-inline-top-level-decls
                                    body-text))
                            (push name ids))))))))
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
