;;; subs.el --- Extracting subtitles from playing video -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'dash)
(require 'dash-functional)
(require 's)

(defvar testlist '("1"
                    "00:00:00 --> 00:00:02"
                    "This is some text."

                    "2"
                    "00:00:02 --> 00:00:05"
                    "Sometimes is is spread across"
                    "two strings,"

                    "3"
                    "00:00:00 --> 00:00:10"
                    "but not always."))

(defun create-alist (lis)
  "Return formated alist by modifying list of strings LIS."
  (let* ((group         (-partition-before-pred 's-numeric? lis))           ;; Break into sublist everytime you find a "[0-9]".
          (grab-last     (lambda (x) (cdr (cdr x))))                             ;; Step to find if last 2 elems are (string . nil) or (string . string)
          (is-length-2-p (lambda (x) (<= 2 (length (funcall grab-last x)))))
          (collapse      (lambda (x) (if (funcall is-length-2-p x)
                                  (cons (mapconcat 'identity
                                          (funcall grab-last x) " ") nil)   ;; Cons with nil this way we can replace (cdr (cdr x))
                                  (cons (car (funcall grab-last x)) nil)))) ;; so we can go from 4 elems to 3.
          (replist       (--map (funcall collapse it) group))               ;; Length of this list should be == (length group).
          (replace-last  (lambda (x y) (setf (cdr (cdr x)) y))))
    (--zip-with (funcall replace-last it other) group replist)              ;; One more mapping function so we can match step for each entry in the alist.
    group))

(provide 'subs)
;;; subs.el ends here
