;;; scalpel-test.el --- Test entry for Scalpel -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the test entry point for Scalpel.  Loading this file adds the
;; package root and the Lisp directory to `load-path' and provides
;; `scalpel-test-run' to run all ERT tests.

;;; Code:

(require 'cl-lib)
(require 'package)
(package-initialize)
(require 'ert)
(require 'scalpel)

(defvar scalpel-test--package-root
  (or (and load-file-name (file-name-directory load-file-name))
      default-directory)
  "Root directory of the Scalpel package source.")

(add-to-list 'load-path scalpel-test--package-root)
(add-to-list 'load-path (expand-file-name "lisp" scalpel-test--package-root))

(defun scalpel-test--lisp-files ()
  "Return Scalpel `lisp' directory file names (no directory)."
  (sort
   (directory-files (expand-file-name "lisp" scalpel-test--package-root)
                    nil "^[^.]+\\.el$")
   #'string<))

(defun scalpel-test--file-feature (file)
  "Return the feature symbol FILE provides.
FILE is a bare file name from the `lisp' directory, such as
\"scalpel-utils-test.el\".  A module is named after the feature it
provides, so the name alone identifies it."
  (intern (file-name-base file)))

(defun scalpel-test--module-features ()
  "Derive feature symbols of all Scalpel modules from lisp/ file names."
  (mapcar #'scalpel-test--file-feature (scalpel-test--lisp-files)))

(defun scalpel-test--test-files ()
  "Return the `*-test.el' file names under `lisp'."
  (cl-remove-if-not (lambda (file) (string-match-p "-test\\.el$" file))
                    (scalpel-test--lisp-files)))

(defun scalpel-test--skip-test-file-p (feature preloaded)
  "Return non-nil when the test file providing FEATURE is loaded already.
PRELOADED is the list of features provided before this run's load
pass began.  A feature in that list may still hold code from the
previous run -- an unload that did not take -- so its file is loaded
again and the run sees what is on disk.  Any other provided feature
was loaded by a sibling test file's `require' during this pass, and
loading that file again would hand ERT the same test twice."
  (and (featurep feature) (not (memq feature preloaded))))

(defun scalpel-test--kill-temp-file-buffers ()
  "Kill all file buffers visiting files under the temp directory.
The directory is the value of the variable `temporary-file-directory'.
Clear the modified flag first so killing never prompts.  This is a
safety net for tests interrupted before their own cleanup ran."
  (dolist (buf (buffer-list))
    (let ((file (buffer-file-name buf)))
      (when (and file
                 (file-name-absolute-p file)
                 (string-prefix-p (file-name-as-directory
                                   (expand-file-name temporary-file-directory))
                                  (expand-file-name file)))
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(defun scalpel-test-reload-modules ()
  "Reload all Scalpel modules so that ERT can exercise the latest code.

Not a user entry point: this touches the running session (unloads
modules, rebuilds keymaps).  Protection for a live console comes only
from the busy-console check and the session snapshot/restore below,
so callers must keep those invariants intact.

Unloading a module kills the buffer-local variables that hold a console's
root, its context file list and its diff baseline, so they are snapshotted
before the unload and put back afterwards.  The conversation itself needs
no snapshot because it lives in the buffer text, which unloading does not
touch.

Reloading is refused while any console has a round in flight, because
redefining the functions that round is running under would strand its
async callback on a mix of old and new definitions.  Wait for the round
to end or abort it with `scalpel-console-abort`."
  (let (busy-buffer)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (not busy-buffer)
                   (bound-and-true-p scalpel-console--busy))
          (setq busy-buffer buffer))))
    (when busy-buffer
      (user-error "Scalpel console %s has a round in flight; wait for it to finish or abort it with `scalpel-console-abort'"
                  (buffer-name busy-buffer))))
  ;; The guard reads `featurep', not `fboundp': the feature being loaded is
  ;; what says the module is here, and a snapshot function missing from a
  ;; loaded module is a defect that has to fail this run loudly.  A
  ;; `fboundp' guard skipped the snapshot silently instead, so every console
  ;; lost its session on the reload with nothing said -- which is exactly how
  ;; a defconst that had swallowed the whole function went unnoticed.
  ;; A snapshot that signals is reported and then dropped, and the reload
  ;; carries on without it.  The code being snapshotted is the code loaded
  ;; in this session, which is not always the code on disk: a session open
  ;; since before a fix holds the older function, and calling it here killed
  ;; the reload before the unload -- so the session could never pick the fix
  ;; up, and `M-x scalpel-test-run' repeated the old defect's own error
  ;; (`void-variable the', from a docstring that had ended early) on every
  ;; attempt until Emacs was restarted.  What is lost is the session state
  ;; below, which the message names; what is gained is that one reload
  ;; replaces the broken code with the code on disk, and the next run
  ;; snapshots normally.
  (let ((scalpel-console-session-snapshot
         (when (featurep 'scalpel-console)
           (condition-case err
               (scalpel-console--session-snapshot)
             (error
              (message
               (concat "Scalpel: could not snapshot console sessions (%s); "
                       "they are not carried across this reload -- the "
                       "session state of every open console is dropped, and "
                       "a console may need `scalpel-open' again")
               (error-message-string err))
              nil)))))
    ;; Unload every module feature derived from the lisp directory.
    (dolist (feat (scalpel-test--module-features))
      (when (featurep feat)
        (condition-case nil
            (unload-feature feat t)
          (error nil))))
    (when (featurep 'scalpel)
      (condition-case nil
          (unload-feature 'scalpel t)
        (error nil)))
    ;; Clear only keymaps: reloading re-creates them via defvar, and stale
    ;; bindings would otherwise survive.  User configuration (defcustom etc.)
    ;; is intentionally left untouched.
    (mapatoms (lambda (sym)
                (when (and (string-match-p "^scalpel-.*-map$" (symbol-name sym))
                           (boundp sym))
                  (makunbound sym))))
    ;; Load main module, then lisp sources (tests are excluded here; they are
    ;; loaded after all implementation modules).
    (let ((scalpel-el (expand-file-name "scalpel.el" scalpel-test--package-root)))
      (when (file-exists-p scalpel-el)
        (load-file scalpel-el)))
    (dolist (file (scalpel-test--lisp-files))
      (unless (string-match-p "-test\\.el$" file)
        (load-file (expand-file-name file
                                     (expand-file-name "lisp"
                                                       scalpel-test--package-root)))))
    (when scalpel-console-session-snapshot
      (scalpel-console--session-restore scalpel-console-session-snapshot)))
  (message "Scalpel modules reloaded."))

(defun scalpel-test--load-test-files ()
  "Load each `*-test.el' file under `lisp', once and from disk.
A test file may `require' a sibling: `scalpel-agent-test' needs the
`scalpel-utils-test-with-temp-file' macro at expansion time, so it
cannot wait for this loop -- which visits files in name order -- to
reach `scalpel-utils-test'.  ERT signals when it is handed a test it
already knows, so that `require' counts as the file's load and the
loop does not load it again.  Call this once per run, with the
previous run's test files already deleted."
  (let ((preloaded
         (cl-remove-if-not
          #'featurep
          (mapcar #'scalpel-test--file-feature
                  (scalpel-test--test-files)))))
    (dolist (file (scalpel-test--test-files))
      (let ((feature (scalpel-test--file-feature file)))
        (unless (scalpel-test--skip-test-file-p feature preloaded)
          (load-file (expand-file-name
                      file (expand-file-name "lisp"
                                             scalpel-test--package-root))))))))

(defun scalpel-test--load-entry-tests ()
  "Load this entry file again so that ERT registers what is written here.
`ert-delete-all-tests' is run before the load pass, so what was
registered before is gone.  That pass walks `lisp' alone while this
file sits at the package root, so what was written here never ran.
Reading the file back from disk is the same answer the pass gives
every other file, and it redefines the functions above exactly as
the pass redefines the modules under test: the call that started
this run keeps running under the definition it entered with."
  (let ((entry (expand-file-name "scalpel-test.el"
                                 scalpel-test--package-root)))
    (when (file-exists-p entry)
      (load-file entry))))

(defun scalpel-test-run-internal ()
  "Run the suite in this Emacs.
In batch mode, runs all tests and exits.  Interactively, discards any
existing ERT results buffer and then runs tests with `ert', so the
results buffer is recreated with the invoking directory as its
`default-directory'."
  (let ((dir default-directory))
    (ert-delete-all-tests)
    (scalpel-test--kill-temp-file-buffers)
    (scalpel-test-reload-modules)
    (scalpel-test--load-test-files)
    ;; The entry's own tests are read back here: the pass above visits
    ;; `lisp' files, and this file is not one of them.
    (scalpel-test--load-entry-tests)
    (scalpel-test--kill-temp-file-buffers)
    (let ((default-directory dir))
      (if noninteractive
          (ert-run-tests-batch-and-exit "scalpel-")
        ;; ERT's results buffer name is hard-coded as "*ert*".
        (when (get-buffer "*ert*")
          (kill-buffer "*ert*"))
        (ert "scalpel-")))))

(defun scalpel-test-run ()
  "Run every Scalpel ERT test.
In batch mode (`noninteractive'), run tests in this Emacs and exit.
Interactively (`M-x'), run the modules and the suite in this Emacs,
so results appear in the *ert* buffer.  A console with a round in
flight makes the reload signal a `user-error'."
  (interactive)
  (scalpel-test-run-internal))

(ert-deftest scalpel-test-run-runs-in-this-emacs ()
  "Interactive `scalpel-test-run' runs the suite here, not in a child.
Regression: it started a `--batch' child Emacs, so `M-x
scalpel-test-run' produced no *ert* buffer in the running editor."
  (let ((called 0)
        (spawned nil))
    (cl-letf (((symbol-function 'scalpel-test-run-internal)
               (lambda () (setq called (1+ called))))
              ((symbol-function 'make-process)
               (lambda (&rest _) (setq spawned t))))
      (call-interactively #'scalpel-test-run))
    (ert-info ((format "called=%d spawned=%S" called spawned))
      (should (= called 1))
      (should-not spawned))))

(ert-deftest scalpel-test-open-prompts-and-anchors-console ()
  "`scalpel-open' prompts for a directory and anchors the console there.
The console open is mocked out: this test covers the entry point's
own behavior -- the prompt with the current directory as default and
the `default-directory' binding -- not the console itself."
  (let ((seen-root nil)
        (asked nil))
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (prompt _dir &optional _default _mustmatch)
                 (setq asked prompt)
                 (expand-file-name "sub" temporary-file-directory)))
              ((symbol-function 'scalpel-console-open)
               (lambda ()
                 (setq seen-root default-directory))))
      (let ((default-directory (file-name-as-directory
                                (expand-file-name temporary-file-directory))))
        (call-interactively #'scalpel-open))
      (ert-info ((format "asked=%S seen-root=%S" asked seen-root))
        (should (string-match-p "Scalpel console root" asked))
        (should (string=
                 seen-root
                 (file-name-as-directory
                  (expand-file-name "sub" temporary-file-directory))))))))

(provide 'scalpel-test)

;;; scalpel-test.el ends here
