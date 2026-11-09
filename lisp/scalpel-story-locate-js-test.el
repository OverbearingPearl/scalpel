;;; scalpel-story-locate-js-test.el --- Tests for scalpel-locate-js -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the JavaScript locator: strings, template literals and
;; regex literals must never disturb bracket counting, and a
;; declaration range must cover its whole body.

;;; Code:

(require 'ert)
(require 'scalpel-locate-js)
(require 'ert-gwt)

(ert-gwt-deftest
  (:given ((results nil)))
  (:when (setq results
               (list (scalpel-locate-js-balanced-p "var a = \")}{(\";")
                     (scalpel-locate-js-balanced-p "var a = '))){'"))))
  (:then (should (nth 0 results)))
  (:then (should (nth 1 results))))

(ert-gwt-deftest
  (:given ((results nil)))
  (:when (setq results
               (list (scalpel-locate-js-balanced-p "const re = /[\\\\]){}]/g;")
                     (scalpel-locate-js-single-definition-p
                      "const re = /[\\\\]){}]/g;\n"))))
  (:then (should (nth 0 results)))
  (:then (should (nth 1 results))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result (scalpel-locate-js-balanced-p
                       "const tpl = `a ${ b ? \"{\" : \"}\" } c`;")))
  (:then (should result)))

(ert-gwt-deftest
  (:given ((symbols nil)))
  (:when (with-temp-buffer
           (insert "const tpl = `a ${ b ? \"{\" : \"}\" } c`;\nconst after = 1;\n")
           (setq symbols (scalpel-locate-js-list-symbols nil))))
  (:then (should (equal symbols (list "tpl" "after")))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (setq result (scalpel-locate-js-single-definition-p
                       "function f(x) { return x; }\n")))
  (:then (should result)))

(ert-gwt-deftest
  (:given nil)
  (:when nil)
  (:then (should-not (scalpel-locate-js-single-definition-p
                      "function a() {}\nfunction b() {}\n"))))

(ert-gwt-deftest
  (:given ((result nil)
           (end-char nil)))
  (:when (with-temp-buffer
           (insert "function f(a, b) { return a + b; }\nconst g = 1;\n")
           (setq result (scalpel-locate-js-range nil "f"))
           (setq end-char (and result (char-after (1- (cdr result)))))))
  (:then
   (should result)
   (should (= (car result) 1))
   (should (= end-char 125))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (with-temp-buffer
           (insert "function f(a = \"){\") { return a; }\n")
           (setq result (scalpel-locate-js-range nil "f"))))
  (:then (should result)))

(provide 'scalpel-story-locate-js-test)

;;; scalpel-story-locate-js-test.el ends here
