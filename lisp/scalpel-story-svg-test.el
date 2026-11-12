;;; scalpel-story-svg-test.el --- User-visible SVG stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective stories for the SVG capability: I ask for a
;; diagram and receive an absolute path to a written .svg file even
;; when no external backend can run; a probe result is cached so
;; the same program is not re-probed; an environmental failure is
;; logged loudly instead of retried silently.  External processes
;; are stubbed as `:given' place bindings, so no story ever runs
;; drawio or plantuml for real.  Every clause is a single form;
;; every THEN is one `should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-svg)

(defvar scalpel-story-svg-test-path nil
  "Holds the path returned by a render story.")

(defvar scalpel-story-svg-test-log nil
  "Holds probe log lines captured inside a story.")

(ert-gwt-deftest
  (:given ((dir (make-temp-file "scalpel-story-" t))
           (spec '(:name diagram
                   :nodes ((:id a :label "A" :x 0 :y 0 :w 10 :h 10)))))
          (setq scalpel-story-svg-test-path nil))
  (:when (setq scalpel-story-svg-test-path
               (scalpel-svg--plain-render spec dir)))
  (:then (should (file-exists-p scalpel-story-svg-test-path)))
  (:then (should (string-suffix-p "diagram.svg"
                                  scalpel-story-svg-test-path)))
  (:then (should (string-search "<svg"
                                (with-temp-buffer
                                  (insert-file-contents
                                   scalpel-story-svg-test-path)
                                  (buffer-string)))))
  (:cleanup (delete-directory dir t)))

(ert-gwt-deftest
  (:given (((symbol-function 'scalpel-svg--run)
            (lambda (_program _args) nil))
           (scalpel-svg--probe-cache nil)
           (dir (make-temp-file "scalpel-story-" t))
           (spec '(:name plain :nodes nil)))
          (setq scalpel-story-svg-test-path nil
                scalpel-story-svg-test-log nil))
  (:when (cl-letf (((symbol-function 'message)
                    (lambda (format &rest args)
                      (push (apply #'format format args)
                            scalpel-story-svg-test-log))))
          (setq scalpel-story-svg-test-path
                (scalpel-svg-render spec dir))))
  (:then (should (string-suffix-p ".svg" scalpel-story-svg-test-path)))
  (:then (should (cl-some
                  (lambda (line)
                    (string-search "skipping backend" line))
                  scalpel-story-svg-test-log)))
  (:cleanup (delete-directory dir t)
            (setq scalpel-svg--probe-cache nil)))

(ert-gwt-deftest
  (:given ((scalpel-svg--probe-cache nil)
           (calls 0)
           ((symbol-function 'scalpel-svg--run)
            (lambda (_program _args)
              (cl-incf calls)
              t)))
          (setq scalpel-story-svg-test-log nil))
  (:when (progn
           (scalpel-svg--probe "drawio" "--version")
           (scalpel-svg--probe "drawio" "--version")))
  (:then (should (= 1 calls)))
  (:then (should (cdr (assoc "drawio" scalpel-svg--probe-cache))))
  (:cleanup (setq scalpel-svg--probe-cache nil)))

(provide 'scalpel-story-svg-test)

;;; scalpel-story-svg-test.el ends here
