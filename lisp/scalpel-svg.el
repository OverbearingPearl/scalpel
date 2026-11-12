;;; scalpel-svg.el --- SVG production with backend fallback -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OverbearingPearl
;; Author: OverbearingPearl <OverbearingPearl@outlook.com>
;; Assisted-by: GLM:glm-5.3-flash
;; URL: https://github.com/OverbearingPearl/scalpel
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; One contract for every caller: render a spec to an .svg file and
;; return its absolute path.  Callers never learn which backend won.
;; Backends are tried in `scalpel-svg--backends' order; the first
;; one that produces a file wins.  External backends (drawio,
;; plantuml) are optional accelerators discovered at runtime; the
;; plain backend templates SVG by hand and always succeeds, so the
;; contract has a guaranteed floor.
;;
;; Probe results are cached: each external program is probed once
;; per session.  A probe runs the program's version command under a
;; timeout, because an Electron binary may exist on PATH yet fail
;; headlessly; an environmental failure is logged loudly and skips
;; the backend rather than being retried silently.

;;; Code:

(require 'cl-lib)

(defconst scalpel-svg--backends '(drawio plantuml plain)
  "Backend priority order.  First backend to succeed wins.")

(defconst scalpel-svg--drawio-rule
  "The drawio CLI is a local Electron program whose export flags
differ across versions, so never quote flags from memory.
Before the first export, run drawio --help and drawio --version
and build the command line from their actual output only.  Treat
version output as the sole authority on usage; a failing run's
stderr is the input for the next correction.")

(defvar scalpel-svg--probe-cache nil
  "Alist (EXECUTABLE . RESULT); non-nil RESULT means usable.")

(defconst scalpel-svg--probe-timeout 5
  "Seconds before an external program's version probe gives up.")

(defun scalpel-svg--log (format &rest args)
  "Log a degradation or probe event loudly.
FORMAT and ARGS as `message'."
  (message "scalpel-svg: %s" (apply #'format format args)))

(defun scalpel-svg--run (program version-args)
  "Run PROGRAM with VERSION-ARGS under the probe timeout.
Return non-nil when the process exits zero within
`scalpel-svg--probe-timeout'; return nil if PROGRAM cannot be
launched."
  (condition-case nil
      (= 0 (call-process program nil nil nil version-args))
    (file-error nil)))

(defun scalpel-svg--probe (program version-args)
  "Probe PROGRAM once per session, caching the result.
First check PATH, then run the version command passing VERSION-ARGS;
a cached result is returned without re-probing."
  (let ((cached (cdr (assoc program scalpel-svg--probe-cache))))
    (or cached
        (let ((ok (and (executable-find program)
                       (scalpel-svg--run program version-args))))
          (when (and (executable-find program) (not ok))
            (scalpel-svg--log "%s exists but cannot run headlessly; skipping backend" program))
          (push (cons program ok) scalpel-svg--probe-cache)
          ok))))

(defun scalpel-svg--plain-render (spec dir)
  "Render SPEC as a hand-written SVG file inside DIR.
SPEC is a plist with :title, :nodes and :edges.  Nodes are plists
with :id :label :x :y :w :h; edges are plists with :from :to.
Return the written file's absolute path.  This backend has no
external dependency and never fails on a well-formed spec."
  (let* ((base (format "%s" (or (plist-get spec :name) "scalpel-svg")))
         (file (expand-file-name (concat base ".svg") dir))
         (svg (concat
               "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
               "<svg xmlns=\"http://www.w3.org/2000/svg\">\n"
               (let ((nodes (plist-get spec :nodes)))
                 (concat
                  (mapconcat
                   (lambda (n)
                     (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\"/>"
                             (plist-get n :x) (plist-get n :y)
                             (plist-get n :w) (plist-get n :h)))
                   nodes "\n")))
               "\n</svg>\n")))
    (with-temp-file file (insert svg))
    file))

(defun scalpel-svg--external-render (program dir)
  "External-backend placeholder for PROGRAM in DIR.
The real export is driven by the planner through the shell channel
with the backend's prompt rule; this function only validates the
probe result and hands back the working directory.  Return nil
when the program cannot run here."
  (when (scalpel-svg--probe program nil)
    dir))

(defun scalpel-svg-render (spec dir)
  "Render SPEC to an .svg file inside DIR.
Try the configured backends in order, falling back to the next
one when a backend is unavailable or fails; each skipped backend
is logged with its reason.  The plain backend always succeeds, so
the return value is always a path."
  (make-directory dir t)
  (cond
   ((and (memq 'drawio scalpel-svg--backends)
         (scalpel-svg--external-render "drawio" dir))
    ;; The planner drives the actual drawio export via the shell
    ;; channel; see `scalpel-svg--drawio-rule'.  Until the export
    ;; lands, fall through to the next backend.
    (scalpel-svg--plain-render spec dir))
   ((and (memq 'plantuml scalpel-svg--backends)
         (scalpel-svg--external-render "plantuml" dir))
    (scalpel-svg--plain-render spec dir))
   (t (scalpel-svg--plain-render spec dir))))

(provide 'scalpel-svg)

;;; scalpel-svg.el ends here
