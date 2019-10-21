;;; init.el --- Emacs main configuration file -*-
;;;
;;; Commentary:
;;; Emacs config by dylanjm
;;;
;;; Code:
(defvar my--gc-cons-threshold gc-cons-threshold)
(defvar my--gc-cons-percentage gc-cons-percentage)
(defvar my--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      inhibit-compacting-font-caches t
      message-log-max 16384
      file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold my--gc-cons-threshold
                  gc-cons-percentage my--gc-cons-percentage
                  file-name-handler-alist my--file-name-handler-alist)))

(defvar package--init-file-ensured)
(setq package-enable-at-startup nil
      package--init-file-ensured nil)

(setq-default ad-redefinition-action 'accept
              auto-save-file-name-transforms '((".*" "~/.cache/emacs/emacs-backups" t))
              auto-window-vscroll nil
              backup-by-copying t
              backup-directory-alist '(("." . "~/.cache/emacs/emacs-backups"))
              column-number-mode nil
              create-lockfiles nil
              cursor-in-non-selected-windows nil
              cursor-type 'bar
              display-time-default-load-average nil
              fill-column 80
              frame-title-format '("%b - Emacs")
              help-window-select t
              indent-tabs-mode nil
              inhibit-compacting-font-caches t
              inhibit-default-init t
              inhibit-startup-screen t
              initial-scratch-message ""
              line-number-mode nil
              line-move-visual nil
              load-prefer-newer t
              mode-line-position nil
              mode-line-in-non-selected-windows nil
              redisplay-dont-pause t
              ring-bell-function #'ignore
              sentence-end-double-space nil
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

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(global-display-line-numbers-mode)

(blink-cursor-mode 0)
(tooltip-mode -1)
(menu-bar-mode -1)
(fringe-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(fset 'menu-bar-open nil)
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list
 'default-frame-alist
 '(font . "-*-Iosevka Nerd Font Mono-ultralight-normal-normal-*-18-*-*-*-m-0-iso10646-1"))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-verbose t))

;(use-package no-littering)
(use-package all-the-icons)
(use-package flx)
(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode))
(use-package use-package-ensure-system-package
  :commands (use-package-ensure-system-package-exists?))

(use-package exec-path-from-shell
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (progn
    (when (string-match-p "/zsh$" (getenv "SHELL"))
      (setq exec-path-from-shell-arguments '("-l")))
    (dolist (var '("PYTHONPATH"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

(use-package ns-win
  :ensure nil
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil
        mac-option-modifier 'meta
        mac-command-modifier 'meta
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none
        mac-function-modifier 'hyper))

(use-package darktooth-theme
  :config
  (load-theme 'darktooth t)
  (darktooth-modeline-three)
  (set-face-attribute 'mode-line nil :height 160)
  (set-face-attribute 'mode-line-inactive nil :height 160))

(use-package doom-themes
  :config
  ;;(doom-themes-treemacs-config)
  ;;(doom-themes-org-config)
  (setq doom-themes-enable-italic
        doom-themes-enable-bold))

;; (use-package doom-modeline
;;   :config
;;   (setq doom-modeline-project-detection 'project
;;         doom-modeline-buffer-file-name-style 'truncate-upto-project
;;         doom-modeline-icon (display-graphic-p)
;;         doom-modeline-major-mode-icon t
;;         doom-modeline-major-mode-color-icon t
;;         doom-modeline-buffer-state-icon t
;;         doom-modeline-minor-modes (featurep 'minions)
;;         doom-modeline-checker-simple-format t
;;         doom-modeline-vcs-max-length 80
;;         doom-modeline-github t
;;         doom-modeline-github-interval (* 30 60)
;;         doom-modeline-percent-position nil
;;         doom-modeline-buffer-encoding nil
;;         find-file-visit-truename t)
;;   :hook (after-init . doom-modeline-mode))

(use-package smart-mode-line
  :config
  (sml/setup))

(use-package eyebrowse
  :commands eyebrowse-mode
  :config (eyebrowse-mode t))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . show-paren-mode)))

(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  (history-length t)
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring))
  (savehist-file (expand-file-name "~/.cache/emacs-history"))
  (savehist-save-minibuffer-history 1)
  :config (savehist-mode 1))

(use-package whitespace
  :ensure nil
  :hook (((prog-mode text-mode) . whitespace-turn-on)
         (before-save . whitespace-cleanup))
  :custom
  (whitespace-style '(face empty indentation::space tab trailing)))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :custom (aggressive-indent-comments-too))

(use-package hungry-delete
  :defer 1
  :config (hungry-delete-mode 1))

(use-package minions
  :commands minions-mode
  :init (minions-mode 1))

(use-package company
  :commands global-company-mode
  :bind
  (:map company-active-map
        ("RET" . nil)
        ([return] . nil)
        ("TAB" . company-complete-selection)
        ([tab] . company-complete-selection)
        ("C-f" . company-complete-common)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :hook (after-init . global-company-mode)
  :init (setq company-require-match 'never
              company-async-timeout 5
              company-idle-delay 0.1
              company-minimum-prefix-length 2
              company-tooltip-align-annotations t)
  :config
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend
                            company-echo-metadata-frontend))
  (setq company-backends '(company-capf company-files)))

(use-package company-lsp :defer t)
(use-package company-irony :defer t)

(use-package company-statistics
  :after company
  :custom
  (company-statistics-file "~/.cache/emacs/company-statistics-cache.el")
  :config (company-statistics-mode))

(use-package company-flx
  :after company
  :config (company-flx-mode 1))

(use-package company-quickhelp
  :after company
  :config (company-quickhelp-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package undo-tree
  :defer 0.5
  :commands global-undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package gcmh
  :ensure nil
  :commands gcmh-mode
  :config (gcmh-mode 1))

(use-package ivy
  :demand
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-switch-buffer-kill))
  :custom
  (ivy-dynamic-exhibit-delay-ms 250)
  (ivy-case-fold-search-default t)
  (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1))

(use-package counsel
  :demand
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x C-d" . counsel-dired-jump)
         ("C-x C-l" . counsel-find-library)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-v" . counsel-set-variable)
         ("C-x C-u" . counsel-unicode-char))
  :config
  (counsel-mode 1))

(use-package swiper
  :bind (("C-s" . swiper)
         :map swiper-map
         ("M-%" . swiper-query-replace)))

(use-package all-the-icons-ivy
  :demand
  :commands (all-the-icons-ivy-setup)
  :preface
  (setq all-the-icons-ivy-file-commands '(counsel-find-file
                                          counsel-dired-jump
                                          counsel-recentf
                                          counsel-find-library))
  (setq all-the-icons-ivy-buffer-commands '(ivy-switch-buffer
                                            ivy-switch-buffer-other-window
                                            counsel-projectile-switch-to-buffer))
  :config
  (all-the-icons-ivy-setup))

(use-package flyspell
  :hook (((markdown-mode org-mode text-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_US")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcom-message nil))

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package ispell
  :defer 2
  :ensure-system-package (hunspell . "trizen -S hunspell")
  :custom
  (ispell-dictionary "en_US")
  (ispell-program-name (executable-find "hunspell"))
  (ispell-really-hunspell t)
  (ispell-silently-savep t)
  :config
  (flyspell-mode 1))

(use-package abbrev
  :ensure nil
  :hook ((text-mode org-mode latex-mode) . abbrev-mode)
  :custom (abbrev-file-name "~/.cache/emacs_abbrev")
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package recentf
  :init (recentf-mode)
  :custom
  (recentf-exclude (list "COMMIT_EDITMSG"
                         "~$"
                         "/scp:"
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 200)
  (recentf-save-file "~/.cache/emacs/emacs-recentf")
  :config (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package git-commit
  :after magit
  :hook (git-commit-mode . my/git-commit-auto-fill-everywhere)
  :custom (git-commit-summary-max-length 50)
  :preface
  (defun my/git-commmit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil)))

(use-package magit :defer 0.3)

(use-package gh
  :defer t)

(use-package git-gutter
  :defer 0.3
  :init (global-git-gutter-mode 1))

(use-package gitconfig-mode
  :defer t
  :hook (gitconfig-mode . (lambda () (setf indent-tabs-mode nil tab-width 4))))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package projectile
  :defer 1
  :custom
  (projectile-cache-file "~/.cache/emacs/projectile.cache")
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-known-projects-file "~/.cache/emacs/projectile-bookmarks.eld")
  (projectile-mode-line '(:eval (projectile-project-name)))
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))

(use-package server
  :init (server-mode))

(use-package julia-mode)
(use-package julia-repl
  :hook (julia-mode . julia-repl-mode))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package flycheck
  :config (global-flycheck-mode))

(provide 'init)
;;; init.el ends here
