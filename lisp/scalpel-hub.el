;;; scalpel-hub.el --- Multi-session hub for Scalpel consoles -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GPT:gpt-6-luna, GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; A management surface over every live Scalpel console buffer.  The
;; hub lists consoles in one tabulated view -- root, mnemonic, busy
;; state, cumulative token total and the most recent recorded user
;; instruction -- so that several sessions can be switched between,
;; inspected, renamed and killed without scrolling the buffer list.
;;
;; Keybindings are presented the way `scalpel-commit-mode' presents
;; them: as \\[...] substitutions inside the mode docstring, so they
;; always reflect the actual local map, plus a compact hint appended
;; to the tabulated header line.  No mode-line or process slot is
;; touched, so every theme renders the hub identically.
;;
;; Console buffers are recognised by name against
;; `scalpel-console-buffer-name-format', so no new state file is
;; needed.  The mnemonic column is exact only for sessions renamed
;; from the hub (stored buffer-locally on the console); the lineage
;; column degrades to a blank when `scalpel-lineage' is absent.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'scalpel-console)

(defconst scalpel-hub--key-hint
  "RET select  j/k/n/p line  f/b/h/l page  d kill  r label  g refresh  q quit"
  "Compact keybinding hint rendered after the tabulated header line.")
(defvar scalpel-hub-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'scalpel-hub-select)
    (define-key map "g" #'scalpel-hub-refresh)
    (define-key map "d" #'scalpel-hub-kill)
    (define-key map "r" #'scalpel-hub-set-mnemonic)
    (define-key map "q" #'quit-window)
    (define-key map "j" #'next-line)
    (define-key map "n" #'next-line)
    (define-key map "k" #'previous-line)
    (define-key map "p" #'previous-line)
    (define-key map "f" #'scroll-up-command)
    (define-key map "b" #'scroll-down-command)
    (define-key map "h" #'scroll-down-command)
    (define-key map "l" #'scroll-up-command)
    map))
(defvar scalpel-hub--buffer-name "*scalpel-hub*"
  "Name of the hub buffer opened by `scalpel-hub-open'.")

(defvar-local scalpel-hub-mnemonic nil
  "Mnemonic set on a console buffer via `scalpel-hub-set-mnemonic'.")

(defun scalpel-hub--console-buffer-p (name)
  "Return t when buffer NAME fits the console name format, nil otherwise.
The `%s' slot of `scalpel-console-buffer-name-format' is matched
wildly: everything before it must prefix NAME and everything
after it must suffix NAME.  Emacs's buffer-uniqueness suffix
\"<N>\" appended after the trailing part is also tolerated, so a
second console at the same root (e.g. \"*scalpel: ~/proj*<2>\")
is still recognized.  The return value is always t or nil so
callers can compare with `eq' against t."
  (let ((fmt scalpel-console-buffer-name-format))
    (and fmt
         (string-match (regexp-quote "%s") fmt)
         (and (string-match
               (concat "\\`"
                       (regexp-quote (substring fmt 0 (match-beginning 0)))
                       ".*"
                       (regexp-quote (substring fmt (match-end 0)))
                       "\\(?:<[0-9]+>\\)?"
                       "\\'")
               name)
              t))))

(defun scalpel-hub--consoles ()
  "Return the live console buffers, newest first by buffer order."
  (cl-remove-if-not
   (lambda (buf)
     (scalpel-hub--console-buffer-p (buffer-name buf)))
   (buffer-list)))

(defun scalpel-hub--busy (buf)
  "Return the busy state of console BUF.
The value is \"yes\" when the console is busy, \"err\" when the
buffer carries a scalpel-console-planner-error text property (a
persistent marker the console leaves on failed turns), \"no\" when
idle, or \"-\" when BUF is no longer live."
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (if (bound-and-true-p scalpel-console--busy)
            "yes"
          (if (text-property-any 1 (1+ (buffer-size))
                                 'scalpel-console-planner-error t)
              "err"
            "no")))
    "-"))

(defun scalpel-hub--tokens (buf)
  "Return the cumulative token total for console BUF, or a dash.
Zero is shown as 0; a missing token record or an unavailable
token module degrades to a dash rather than an error."
  (if (not (buffer-live-p buf))
      "-"
    (or (with-current-buffer buf
          (and (fboundp 'scalpel-console--cumulative-token-total)
               (boundp 'scalpel-token--console-totals)
               (number-to-string
                (scalpel-console--cumulative-token-total buf))))
        "-")))

(defun scalpel-hub--last-instruction (name)
  "Return the most recent recorded question for session NAME.
Scans the global lineage dialogue chain for entries whose :session
matches the console buffer name NAME; only entries carrying a
:question qualify.  When `scalpel-lineage' is not loadable, or no
matching entry exists, return nil so the column stays blank.
The full question text is returned without any length truncation;
overly wide cells simply extend off-screen, which truncated-lines
handling takes care of.  Multi-line questions are folded to a
single line so the tabulated-list row structure is preserved."
  (when (require 'scalpel-lineage nil t)
    (when (boundp 'scalpel-lineage--dialogues)
      (let ((last nil))
        (dolist (d scalpel-lineage--dialogues)
          (when (and (equal (plist-get d :session) name)
                     (plist-get d :question))
            (setq last (plist-get d :question))))
        (when last
          ;; Layout compromise: tabulated-list renders one physical row
          ;; per entry, so embedded newlines must be folded to spaces.
          (replace-regexp-in-string "\n" " " (string-trim last)))))))

(defun scalpel-hub--entries ()
  "Build the tabulated entries, one per live console buffer."
  (let* ((consoles (scalpel-hub--consoles))
         (root-counts
          (let ((counts (make-hash-table :test 'equal)))
            (dolist (buf consoles)
              (let ((root (with-current-buffer buf
                            (when (bound-and-true-p scalpel-console--root)
                              (abbreviate-file-name scalpel-console--root)))))
                (when root
                  (puthash root (1+ (gethash root counts 0)) counts))))
            counts))
         (root-duplicated
          (lambda (root)
            (> (gethash root root-counts 0) 1))))
    (mapcar
     (lambda (buf)
       (let* ((name (buffer-name buf))
              (root (with-current-buffer buf
                      (if (bound-and-true-p scalpel-console--root)
                          (abbreviate-file-name scalpel-console--root)
                        name)))
              (mnemonic (with-current-buffer buf
                          (or (bound-and-true-p scalpel-hub-mnemonic) "")))
              (busy (scalpel-hub--busy buf))
              (tokens (scalpel-hub--tokens buf))
              (last (or (scalpel-hub--last-instruction name) ""))
              (busy-cell (cond
                          ((string= busy "yes")
                           (propertize busy 'face 'bold 'font-lock-face 'error))
                          ((string= busy "no")
                           (propertize busy 'face 'shadow))
                          (t busy)))
              (root-cell (if (and (funcall root-duplicated root)
                                  (not (string= root name)))
                             name
                           root)))
         (list name
               (vector root-cell mnemonic busy-cell tokens last))))
     consoles)))

(defun scalpel-hub--refresh ()
  "Rebuild the hub listing from the live console buffers.

After recomputing the entries, widen the Root (first) column to
the widest displayed Root cell among the current entries, never
narrower than the base width 30, so first-column content is not
truncated by the column width.  The header is re-initialized before
printing so `tabulated-list-print' picks up the new width.

The entry id of the line under point is recorded before the
rebuild; after printing, if that id still exists among the fresh
entries, point is restored to its row, otherwise point falls back
to the first line."
  (when (buffer-live-p (get-buffer scalpel-hub--buffer-name))
    (with-current-buffer scalpel-hub--buffer-name
      (let ((current-id (tabulated-list-get-id)))
        (setq tabulated-list-entries (scalpel-hub--entries))
        (let* ((fmt (copy-sequence tabulated-list-format))
               (max-width 30))
          (dolist (entry tabulated-list-entries)
            ;; Entries are (NAME VECTOR) two-element lists: the row
            ;; vector is the second element, wrapped in the list.
            (let* ((row (cadr entry))
                   (root-cell (and (vectorp row) (aref row 0))))
              (when (stringp root-cell)
                (setq max-width
                      (max max-width (string-width root-cell))))))
          (catch 'found
            (dotimes (i (length fmt))
              (let ((col (aref fmt i)))
                (when (and (consp col)
                           (equal (car col) "Root"))
                  ;; Column entries are lists (NAME WIDTH ...): mutate
                  ;; the width slot in place via setcar on the cdr.
                  (setcar (cdr col) max-width)
                  (throw 'found nil)))))
          (setq tabulated-list-format fmt)
          (tabulated-list-init-header)
          (tabulated-list-print))
        (let ((target-line
               (when current-id
                 (cl-position current-id tabulated-list-entries
                              :key #'car :test #'equal))))
          (if target-line
              (progn
                (goto-char (point-min))
                (forward-line target-line))
            (goto-char (point-min))))))))

(defun scalpel-hub--refresh-on-event ()
  "Event-driven refresh entry point.
Refresh the hub buffer immediately when it exists; return
silently otherwise.  Event invocations never run while the
minibuffer is active, so no guard is needed."
  (interactive)
  (when (buffer-live-p (get-buffer scalpel-hub--buffer-name))
    (scalpel-hub--refresh)))

(defun scalpel-hub-refresh ()
  "Refresh the hub listing in place."
  (interactive)
  (scalpel-hub--refresh))

(defun scalpel-hub--console-at-point ()
  "Return the live console buffer named by the entry at point.
Signal a user error when the buffer has died since the listing
was built; the caller should refresh afterwards."
  (let ((name (tabulated-list-get-id)))
    (or (and name (get-buffer name))
        (user-error "Scalpel: console %s no longer exists" name))))

(defun scalpel-hub-select ()
  "Switch to the console buffer at point."
  (interactive)
  (pop-to-buffer (scalpel-hub--console-at-point)))

(defun scalpel-hub-kill ()
  "Kill the console buffer at point after its own confirmation.
The console's `scalpel-console--confirm-kill' decides, so the hub
adds no second prompt of its own."
  (interactive)
  (let ((buf (scalpel-hub--console-at-point)))
    (with-current-buffer buf
      (if (fboundp 'scalpel-console--confirm-kill)
          (scalpel-console--confirm-kill)
        (kill-buffer buf)))
    (scalpel-hub--refresh)))

(defun scalpel-hub-set-mnemonic (label)
  "Set a display-only mnemonic LABEL on the console at point.
The label is stored buffer-locally in `scalpel-hub-mnemonic'
and never touches the buffer name or any state keyed by
\"buffer-name\" (tokens, lineage, thinking buffer).  An empty
input clears the label; no buffer is renamed."
  (interactive "sMnemonic label: ")
  (with-current-buffer (scalpel-hub--console-at-point)
    (setq-local scalpel-hub-mnemonic (if (string-empty-p label) nil label)))
  (scalpel-hub-refresh))

(define-derived-mode scalpel-hub-mode tabulated-list-mode "Scalpel-Hub"
  "Major mode listing every live Scalpel console buffer.
Each row is one console session.  Columns: Root is the project
root the console serves; Mnemonic is a display-only short label
set with \\<scalpel-hub-mode-map>\\[scalpel-hub-set-mnemonic]; Busy
shows whether the session is currently running a request; Tokens
is the cumulative token count of the session; Last instruction is
the most recent recorded user instruction for that session.
\\[scalpel-hub-select] switches to the console at point,
\\[scalpel-hub-set-mnemonic] sets a display-only label for it,
\\[scalpel-hub-kill] kills it after confirmation,
\\[scalpel-hub-refresh] refreshes the listing and \\[quit-window]
quits the hub.  The same bindings are shown compactly at the
right edge of the header line.  This buffer never wraps lines by
default; long content extends past the edge of the screen and can
be toggled with `toggle-truncate-lines'."
  (require 'tabulated-list)
  (setq tabulated-list-format
        [("Root" 30 t) ("Mnemonic" 12 t) ("Busy" 6 t)
         ("Tokens" 8 nil) ("Last instruction" 40 nil)]
        tabulated-list-padding 2)
  (setq revert-buffer-function
        (lambda (&optional _ignore-auto _noconfirm) (scalpel-hub--refresh)))
  (setq truncate-lines t)
  (tabulated-list-init-header))

(defun scalpel-hub-open ()
  "Open the Scalpel session hub.
The hub lists every live console buffer with its root, mnemonic,
busy state, token total and most recent recorded instruction.
The key hint is appended to `header-line-format' after
`tabulated-list-init-header' and `scalpel-hub--refresh' have run,
because `tabulated-list-init-header' resets the format string
itself and would otherwise overwrite a hint set earlier.
The hint is rendered right-aligned via a `space' display spec so
it stays pinned to the header line's right edge instead of being
pushed off-window by the tabulated-list column widths.
Refreshes are event driven: consoles call `scalpel-hub-notify'
when a turn settles or output is written, and the hub holds no
timers of its own."
  (interactive)
  (let ((buf (get-buffer-create scalpel-hub--buffer-name)))
    (with-current-buffer buf
      (scalpel-hub-mode)
      (scalpel-hub--refresh)
      (setq header-line-format
            (append header-line-format
                    (list (propertize
                           " "
                           'display
                           `(space :align-to (- right
                                                (+ 1
                                                   ,(string-width
                                                     scalpel-hub--key-hint)))))
                          scalpel-hub--key-hint))))
    (pop-to-buffer buf)))

(provide 'scalpel-hub)

;;; scalpel-hub.el ends here
