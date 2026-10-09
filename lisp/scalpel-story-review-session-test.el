;;; scalpel-story-review-session-test.el --- Review session stories -*- lexical-binding: t; -*-

;;; Commentary:

;; User-perspective GWT stories for the session review coordinator.
;; I finish a dialogue in a console and expect only the records of
;; that closed dialogue to reach the review; records from earlier
;; dialogues drop out, while unattributable records stay visible.
;; When nothing changed, no review buffer appears.

;;; Code:

(require 'ert-gwt)
(require 'scalpel-review-session)

(ert-gwt-deftest
  (:given ((consoles nil) (dialogues nil) (result nil)))
  (:when (progn
           (setq consoles (list :s1)
                 dialogues (list (list :session :s1
                                       :start 1 :end 2)))
           (with-temp-buffer
             (setq scalpel-lineage--records
                   (list (list :session :s1 :index 1 :file "a")
                         (list :session :s1 :index 2 :file "b")
                         (list :session :s1 :index 3 :file "c")))
             (let ((scalpel-review-session--consoles consoles)
                   (scalpel-lineage--dialogues dialogues))
               (setq result
                     (scalpel-review-session--collect-records))))))
  (:then (should (equal result
                        (list (list :session :s1 :index 1 :file "a")
                              (list :session :s1 :index 2 :file "b"))))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (with-temp-buffer
             (setq scalpel-lineage--records
                   (list (list :session :guest :index 1 :file "a")
                         (list :index 2 :file "b")))
             (let ((scalpel-review-session--consoles nil)
                   (scalpel-lineage--dialogues nil))
               (setq result
                     (scalpel-review-session--collect-records))))))
  (:then (should (equal result
                        (list (list :session :guest :index 1 :file "a")
                              (list :index 2 :file "b"))))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (with-temp-buffer
             (setq scalpel-lineage--records
                   (list (list :session :s1 :index 1 :file "a")))
             (let ((scalpel-review-session--consoles (list :s1))
                   (scalpel-lineage--dialogues
                    (list (list :session :s1 :start 1 :end nil))))
               (setq result
                     (scalpel-review-session--collect-records))))))
  (:then (should (equal result
                        (list (list :session :s1 :index 1 :file "a"))))))

(ert-gwt-deftest
  (:given ((result nil)))
  (:when (progn
           (let ((scalpel-lineage--dialogues nil)
                 (scalpel-review-session--consoles nil))
             (setq result (scalpel-review-session-offer)))))
  (:then (should (null result)))
  (:then (should (null (get-buffer
                        scalpel-review-session--buffer-name)))))

(provide 'scalpel-story-review-session-test)

;;; scalpel-story-review-session-test.el ends here
