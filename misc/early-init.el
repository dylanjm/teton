;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar user-emacs-cache "~/.cache/emacs/")

(eval-and-compile
  (defun djm/emacs-path (path)
    (expand-file-name path user-emacs-directory)))

(eval-and-compile
    (defun djm/emacs-cache (path)
           (expand-file-name path user-emacs-cache)))

(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar default-gc-cons-threshold (if (display-graphic-p) 800000 800000))
(defvar extended-gc-cons-threshold (if (display-graphic-p) 400000000 100000000))

(setq-default  inhibit-compacting-font-caches t
               frame-inhibit-implied-resize t
               gc-cons-percentage 0.6
               auto-window-vscroll nil
               file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold default-gc-cons-threshold)
            (setq gc-cons-percentage 0.1)

            (defun djm/gc-on-lose-focus ()
              (unless (frame-focus-state)
                (garbage-collect)))

            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function #'djm/gc-on-lose-focus))

            (defun djm/minibuffer-setup-hook ()
              (setq gc-cons-threshold extended-gc-cons-threshold))

            (defun djm/minibuffer-exit-hook ()
              (setq gc-cons-threshold default-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'djm/minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'djm/minibuffer-exit-hook)))

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'display-startup-echo-area-message 'ignore)

(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(internal-border . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(font . "-*-Iosevka Nerd Font Mono-ultralight-normal-normal-*-18-*-*-*-m-0-iso10646-1") default-frame-alist)
(push '(variable-pitch . "-*-Fira Sans-light-normal-normal-*-18-*-*-*-p-0-iso10646-1") default-frame-alist)

(setq straight-check-for-modifications 'live-with-find
      use-package-enable-imenu-support t
      straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package use-package-chords)
(use-package use-package-ensure-system-package)
(use-package use-package-hydra)

;;; early-init.el ends here