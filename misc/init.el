;;; init.el --- Emacs main configuration file -*-
;;;
;;; Commentary:
;;; Emacs config by dylanjm
;;;
;;; Code:
(defconst emacs-start-time (current-time))

(defvar my--gc-cons-threshold gc-cons-threshold)
(defvar my--gc-cons-percentage gc-cons-percentage)
(defvar my--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      inhibit-compacting-font-caches t
      message-log-max 16384
      file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold my--gc-cons-threshold
                  gc-cons-percentage my--gc-cons-percentage
                  file-name-handler-alist my--file-name-handler-alist)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(defvar package--init-file-ensured)
(setq package-enable-at-startup nil
      package--init-file-ensured nil)

(setq-default ad-redefinition-action 'accept
              auto-save-file-name-transforms '((".*" "~/.cache/emacs-backups" t))
              auto-window-vscroll nil
              backup-by-copying t
              backup-directory-alist '(("." . "~/.cache/emacs-backups"))
              column-number-mode nil
              create-lockfiles nil
              display-time-default-load-average nil
              fill-column 80
              frame-title-format '("%b - Emacs")
              indent-tabs-mode nil
              inhibit-startup-screen t
              line-number-mode nil
              load-prefer-newer t
              mode-line-position nil
              mode-line-in-non-selected-windows nil
              ring-bell-function 'ignore
              scroll-conservatively most-positive-fixnum
              scroll-margin 10
              size-indication-mode nil
              select-enable-clipboard t
              tab-width 4
              vc-follow-symlinks t
              view-read-only t
              uniquify-buffer-name-style 'forward)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(blink-cursor-mode 0)
(tooltip-mode -1)
(menu-bar-mode -1)
(fringe-mode -1)
(fset 'menu-bar-open nil)
(fset 'yes-or-no-p 'y-or-n-p)

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(when window-system
  (setq-default cursor-type 'bar
                cursor-in-non-selected-windows nil))

(set-face-attribute 'default nil :font "Iosevka Nerd Font-14")

(defvar package-archives)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-always-verbose t))

(use-package exec-path-from-shell
  :defer 0.5
  :if (memq window-system '(mac ns nil))
  :hook (after-init . exec-path-from-shell-initialize))

(use-package all-the-icons :defer 0.5)

(use-package doom-themes
  :commands (doom-themes-org-config)
  :config
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :init (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :config
  (setq doom-modeline-bar-width 1
        doom-modeline-minor-modes t
        doom-modeline-percent-position nil
        find-file-visit-truename t)
  (doom-modeline-mode 1))

(use-package solaire-mode
  :custom (solaire-mode-remap-fringe t)
  :config
  (solaire-mode-swap-bg)
  (solaire-global-mode 1))

(use-package eyebrowse
  :defer 0.5
  :commands eyebrowse-mode
  :init (eyebrowse-mode t))

(use-package display-line-numbers
  :ensure nil
  :config
  (setq display-line-numbers-grow-only t
        display-line-numbers-width-start t))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . show-paren-mode)
         (prog-mode . display-line-numbers-mode)))

(use-package help
  :ensure nil
  :config (setq help-window-select t))

(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  (history-length t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-file (expand-file-name "~/.cache/emacs-history"))
  (savehist-save-minibuffer-history 1)
  :config (savehist-mode 1))

(use-package whitespace
  :ensure nil
  :hook (((prog-mode text-mode) . whitespace-turn-on)
         (before-save . whitespace-cleanup))
  :custom
  (whitespace-style '(face empty indentation::space tab trailing)))

(use-package minions
  :commands minions-mode
  :init (minions-mode 1))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :custom (aggressive-indent-comments-too))

(use-package electric-operator
  :hook (python-mode . electric-operator-mode))

(use-package hungry-delete
  :defer 0.7
  :config (global-hungry-delete-mode))

(use-package company
  :commands global-company-mode
  :hook (after-init . global-company-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package undo-tree
  :commands global-undo-tree-mode
  :init (global-undo-tree-mode 1))

(use-package gcmh
  :commands gcmh-mode
  :init (gcmh-mode 1))

(use-package ivy
  :demand t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H" . ivy-resume))
  :bind (:map ivy-minibuffer-map
              ("<tab>" . ivy-alt-done)
              ("C-i" . ivy-partial-or-done)
              ("C-r" . ivy-previous-line-or-history)
              ("M-r" . ivy-reverse-i-search))
  :custom
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-height 8)
  (ivy-case-fold-search-default t)
  (ivy-initial-inputs-alist nil t)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  :config
  (ivy-mode 1)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))

(use-package counsel
  :after ivy
  :demand t
  :bind (("C-x C-f" . counsel-find-file)))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)

(provide 'init)
;;; init.el ends here
