;;; scalpel-story-locate-js-test.el --- Tests for scalpel-locate-js -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the JavaScript locator: strings, template literals and
;; regex literals must never disturb bracket counting, and a
;; declaration range must cover its whole body.

;;; Code:

(require 'ert)
(require 'scalpel-locate-js)

(ert-deftest scalpel-story-locate-js-test-string-fake-braces ()
  "Brace and paren characters inside a string never disturb counts."
  (should (scalpel-locate-js-balanced-p "var a = \")}{(\";"))
  (should (scalpel-locate-js-balanced-p "var a = '))){'")))

(ert-deftest scalpel-story-locate-js-test-regex-char-class ()
  "A regex character class holding brackets still balances."
  (should (scalpel-locate-js-balanced-p "const re = /[\\\\]){}]/g;"))
  (should (scalpel-locate-js-single-definition-p "const re = /[\\\\]){}]/g;\\n")))

(ert-deftest scalpel-story-locate-js-test-template-interpolation ()
  "Interpolation braces, even holding quoted braces, keep balance."
  (should (scalpel-locate-js-balanced-p
           "const tpl = `a ${ b ? \"{\" : \"}\" } c`;")))

(ert-deftest scalpel-story-locate-js-test-template-then-declaration ()
  "A declaration after a template literal is still found."
  (with-temp-buffer
    (insert "const tpl = `a ${ b ? \"{\" : \"}\" } c`;\nconst after = 1;\n")
    (should (equal (scalpel-locate-js-list-symbols nil)
                   (list "tpl" "after")))))

(ert-deftest scalpel-story-locate-js-test-single-definition-positive ()
  "One complete function is exactly one definition."
  (should (scalpel-locate-js-single-definition-p
           "function f(x) { return x; }\\n")))

(ert-deftest scalpel-story-locate-js-test-single-definition-negative ()
  "Two functions are not one definition."
  (should-not (scalpel-locate-js-single-definition-p
               "function a() {}\nfunction b() {}\n")))

(ert-deftest scalpel-story-locate-js-test-range-covers-body ()
  "The range of a function covers its parameter list and body."
  (with-temp-buffer
    (insert "function f(a, b) { return a + b; }\\nconst g = 1;\\n")
    (let ((range (scalpel-locate-js-range nil "f")))
      (should range)
      (should (= (car range) 1))
      (should (= (char-after (1- (cdr range))) 125)))))

(ert-deftest scalpel-story-locate-js-test-parameters-with-strings ()
  "A parameter default holding brackets is skipped without hanging."
  (with-temp-buffer
    (insert "function f(a = \"){\") { return a; }\\n")
    (should (scalpel-locate-js-range nil "f"))))

(provide 'scalpel-story-locate-js-test)

;;; scalpel-story-locate-js-test.el ends here
