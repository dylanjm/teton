;;; cubit-mode.el --- Emacs Mode for Cubit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'comint)

(defvar cubit-mode-hook nil)

(defgroup cubit ()
  "Major mode for the Cubit language."
  :group 'languages
  :prefix "cubit-")

(defcustom cubit-program "cubitclx"
  "Path to program used by `inferior-cubit'."
  :type 'string
  :group 'cubit)

(defvar cubit-prompt-regexp "^\\w*> "
  "Regexp for matching `inferior-cubit' prompt.")

(defun cubit-eval-region (begin end)
  "Evaluate regino between BEGIN and END."
  (interactive "r")
  (progn
    (comint-send-string "*Cubit*" (buffer-substring-no-properties begin end))
    (comint-send-string "*Cubit*" "\n")))

(defvar inferior-cubit-mode-map
  (let ((map2 (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map2 (kbd "TAB") 'indent-for-tab-command)
    map2)
  "Basic mode map for `inferior-cubit-mode'.")

;;;###autoload
(defun inferior-cubit ()
  "Run an inferior instance of cubit inside Emacs."
  (interactive)
  (let ((cubit-program cubit-program))
    (when (not (comint-check-proc "*Cubit*"))
      (apply #'make-comint-in-buffer "Cubit" "*Cubit*"
        cubit-program nil nil))
    (pop-to-buffer-same-window "*Cubit*")
    (inferior-cubit-mode)))

(defun inferior-cubit--initialize ()
  "Helper function to initialize `inferior-cubit'."
  (setq comint-use-prompt-regexp t))

(define-derived-mode inferior-cubit-mode comint-mode "Cubit"
  "Major mode for `inferior-cubit'.

\\<inferior-cubit-mode-map>"
  nil "Cubit"
  (setq mode-line-process '(":%s"))
  (setq-local comint-inhibit-carriage-motion nil)
  (setq-local comint-process-echoes t)
  (setq-local comint-prompt-regexp cubit-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local paragraph-start cubit-prompt-regexp))

(add-hook 'inferior-cubit-mode-hook #'inferior-cubit--initialize)

;;;###autoload
(defalias 'run-cubit #'inferior-cubit
  "Run an inferior instance of cubit inside Emacs.")

(provide 'cubit-mode)
;;; cubit-mode.el ends here
