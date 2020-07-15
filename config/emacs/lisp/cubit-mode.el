;;; cubit-mode.el --- Emacs Mode for Cubit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar cubit-mode-hook nil)

(defconst cubit-keywords
  (regexp-opt '("body" "volume" "surface" "curve" "vertex")))

(defconst cubit-comment-regex
  "^\\(\\(#+\\).*\\)")

(defconst cubit-comment2-regex
  "\\(\\(#+\\).*\\)")

(defconst cubit-font-lock-keywords
  (list
    (list cubit-keywords 'font-lock-keyword-face)
    (list cubit-comment-regex 1 'font-lock-comment-face)
    (list cubit-comment2-regex 1 'font-lock-comment-face)))

(define-derived-mode cubit-mode prog-mode "Cubit"
  (set (make-local-variable 'font-lock-defaults) '(cubit-font-lock-keywords t)))


(provide 'cubit-mode)
;;; cubit-mode.el ends here
