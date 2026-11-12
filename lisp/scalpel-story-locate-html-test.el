;;; scalpel-story-locate-html-test.el --- HTML locator stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the HTML locator: a user points
;; Scalpel at an HTML file and asks "which ids are here" and "where
;; does this element sit".  Elements without an id are never symbols;
;; script/style bodies are skipped.  Every clause is a single form;
;; every THEN holds one 'should'.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-locate-html)

(ert-gwt-deftest
  (:given ((content "<body>\n  <div id=\"main\">\n    <p>hi</p>\n  </div>\n  <img id=\"logo\" src=\"x.png\">\n</body>\n")
           (result nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-html-list-symbols nil))))
  (:then (should (equal result '("main" "logo")))))

(ert-gwt-deftest
  (:given ((content "<div id=\"a\">\n  <span>x</span>\n</div>\n<p>tail</p>\n")
           (range nil)
           (text nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq range (scalpel-locate-html-range nil "a"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal text "<div id=\"a\">\n  <span>x</span>\n</div>"))))

(ert-gwt-deftest
  (:given ((content "<img id=\"pic\" src=\"x.png\">\n<p>tail</p>\n")
           (range nil)
           (text nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq range (scalpel-locate-html-range nil "pic"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal text "<img id=\"pic\" src=\"x.png\">"))))

(ert-gwt-deftest
  (:given ((full nil)
           (partial nil)))
  (:when (setq full (scalpel-locate-html--single-definition-p
                     "<div id=\"a\">x</div>\n")
             partial (scalpel-locate-html--single-definition-p
                      "<div id=\"a\">x</div>\n<div id=\"b\">y</div>\n")))
  (:then (should (and full (not partial)))))

(ert-gwt-deftest
  (:given ((content "<div id=\"a\">x</div>\n")
           (range nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq range (scalpel-locate-html-range nil "missing"))))
  (:then (should (null range))))

(ert-gwt-deftest
  (:given ((content "<script id=\"s\">var x = \"<div id=\\\"nope\\\">\";</script>\n<p id=\"ok\"></p>\n")
           (result nil)
           (range nil)
           (text nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-html-list-symbols nil))
           (setq range (scalpel-locate-html-range nil "s"))
           (setq text (buffer-substring-no-properties
                       (car range) (cdr range)))))
  (:then (should (equal result '("s" "s.x" "ok")))
         (should (equal text "<script id=\"s\">var x = \"<div id=\\\"nope\\\">\";</script>"))))

(ert-gwt-deftest
  (:given ((content "<html><body>\n<script>\nconst D = { c2_2: {a:1}, c2_2b: {b:2} };\n</script>\n<p id=\"p1\">x</p>\n</body></html>\n")
           (range nil)
           (text nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq range (scalpel-locate-html-range nil "D.c2_2"))
           (setq text (and range (buffer-substring-no-properties (car range) (cdr range))))))
  (:then (should (and range text (string-match-p "c2_2" text)))))

(ert-gwt-deftest
  (:given ((content "<html><body><script>const f = (a) => a;</script><p id=\"p1\"></p></body></html>")
           (result nil)))
  (:when (with-temp-buffer
           (insert content)
           (setq result (scalpel-locate-html-list-symbols nil))))
  (:then (should (member "f" result)))
  (:then (should (member "p1" result))))

(provide 'scalpel-story-locate-html-test)

;;; scalpel-story-locate-html-test.el ends here
