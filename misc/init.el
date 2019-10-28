;;; init.el --- Emacs main configuration file -*- lexical-binding: t; coding: utf-8 -*-
;;;
;;; Commentary:
;;; Emacs config by dylanjm
;;;
;;; Code:
(use-package files
  :straight nil
  :custom
  (auto-save-file-name-transforms `((".*" ,(djm/emacs-cache "backups/") t)))
  (backup-directory-alist `(("." . ,(djm/emacs-cache "backups/")))))

(use-package cus-start
  :straight nil
  :custom
  (ad-redefinition-action 'accept)
  (auto-save-list-file-prefix nil)
  (auto-window-vscroll nil)
  (backup-by-copying t)
  (browse-urls-browser-function "firefox")
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (custom-file (make-temp-file "emacs-custom"))
  (debug-on-error t)
  (display-time-default-load-average nil)
  (echo-keystrokes 0.1)
  (enable-recursive-minibuffers t)
  (fill-column 80)
  (frame-inhibit-implied-resize t)
  (fast-but-imprecise-scrolling t)
  (frame-resize-pixelwise t)
  (frame-title-format '("%b - Emacs"))
  (help-window-select t)
  (icon-title-format frame-title-format)
  (indent-tabs-mode nil)
  (inhibit-compacting-font-caches t)
  (inhibit-default-init t)
  (inhibit-startup-screen t)
  (inhibit-startup-echo-area-message t)
  (initial-scratch-message "")
  (insert-directory-program "gls")
  (load-prefer-newer t)
  (message-log-max 10000)
  (mode-line-in-non-selected-windows nil)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1))
  (ns-use-thin-smoothing t)
  (ring-bell-function #'ignore)
  (set-horizontal-scroll-bar-mode nil)
  (scroll-conservatively most-positive-fixnum)
  (scroll-margin 5)
  (scroll-preserve-screen-position 'always)
  (scroll-step 1)
  (select-enable-clipboard t)
  (sentence-end-double-space nil)
  (tab-always-indent 'complete)
  (tab-width 4)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (vc-follow-symlinks t)
  (view-read-only t)
  (window-combination-resize t)
  :config
  (prefer-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")
  (global-hl-line-mode 1)
  (global-display-fill-column-indicator-mode 1)
  (global-display-line-numbers-mode)
  (blink-cursor-mode 0))

(use-package frame
  :straight nil
  :config (window-divider-mode 1)
  :custom
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  (global-unset-key (kbd "C-z")))

(use-package simple
  :straight nil
  :custom
  (column-number-mode nil)
  (line-number-mode nil)
  (line-move-visual nil)
  (track-eol t)
  (set-mark-command-repeat-pop t))

(use-package saveplace
  :straight nil
  :init (save-place-mode 1)
  :custom (save-place-file (djm/emacs-cache "places")))

(use-package savehist
  :straight nil
  :init (savehist-mode 1)
  :custom
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-autosave-interval 300)
  (savehist-file (djm/emacs-cache "emacs-history"))
  (savehist-save-minibuffer-history 1))

(use-package autorevert
  :straight nil
  :init (global-auto-revert-mode 1)
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-use-notify nil))

(use-package fringe
  :straight nil
  :custom
  (fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                                fringe-indicator-alist))
  :config (fringe-mode '(10 . 8)))

(use-package ns-win
  :straight nil
  :custom
  (ns-pop-up-frames nil)
  (ns-use-native-fullscreen nil)
  (mac-option-modifier 'meta)
  (mac-command-modifier 'meta)
  (mac-right-command-modifier 'left)
  (mac-right-option-modifier 'none)
  (mac-function-modifier 'hyper))

(use-package prog-mode
  :straight nil
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . show-paren-mode))
  :custom
  (prettify-symbols-unprettify-at-point 'right-edge))

(use-package recentf
  :straight nil
  :functions (recentf-save-list)
  :init (recentf-mode 1)
  :custom
  (recentf-save-file (djm/emacs-cache "recentf"))
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("\\.?cache"
                     ".cask"
                     "url"
                     "COMMIT_EDITMSG\\'"
                     "bookmarks"
                     "NEWS"
                     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
                     "^/tmp/"
                     "^/ssh:"
                     "\\.?ido\\.last$"
                     "\\.revive$"
                     "/TAGS$"
                     "^/var/folders/.+$"
                     (lambda (file)
                       (file-in-directory-p file package-user-dir))))
  :config
  (advice-add 'recentf-cleanup :around #'inhibit-message-in-minibuffer)
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (run-at-time nil (* 3 60) (lambda ()
                              (let ((save-silently t)) (recentf-save-list)))))

(use-package vscode-icon)
(use-package dired
  :straight nil
  :functions (dired wdired-change-to-wdired-mode)
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode)
              ("C-c C-r" . dired-rsync)
              ("TAB" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              (":" . dired-git-info-mode))
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-guess-shell-gnutar "tar")
  (dired-listing-switches "-alhF --group-directories-first -v")
  (dired-ls-F-marks-symlinks t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-use-ls-dired nil)
  :config
  (use-package dired-aux :straight nil)
  (use-package dired-x :straight nil)
  (use-package diredfl :init (diredfl-global-mode 1))
  (use-package dired-ranger)
  (use-package dired-git-info)
  (use-package dired-rsync)
  (use-package dired-subtree)
  (use-package fd-dired)
  (use-package dired-sidebar
    :bind ("M-\\" . dired-sidebar-toggle-sidebar)
    :custom (dired-sidebar-theme 'vscode)))

                                        ;(use-package ignoramus :config (ignoramus-setup))

(use-package osx-trash :init (osx-trash-setup))

(use-package focus-autosave-mode :init (focus-autosave-mode))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :custom
  (ibuffer-expert t))

(use-package whitespace
  :straight nil
  :bind (("C-c w" . whitespace-mode))
  :hook (((prog-mode text-mode conf-mode) . whitespace-mode)
         (before-save . delete-trailing-whitespace))
  :custom
  (whitespace-style '(face indentation space-after-tab space-before-tab
                           tab-mark empty trailing lines-tail))
  (whitespace-line-column nil))

(use-package windmove
  :bind (("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down))
  :custom (windmove-default-keybindings 'shift))

(use-package zop-to-char
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package async)
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-nord t)
  (dolist (face '(region hl-line secondary-selection))
    (set-face-attribute face nil :extend t))
  (set-face-attribute 'font-lock-comment-face nil :family "Iosevka Slab"
                      :height 180 :weight 'bold :slant 'italic))

  (use-package minions
    :hook (after-init . minions-mode)
    :custom
    (minions-mode-line-lighter "...")
    (minions-mode-line-delimiters '("" . "")))

(use-package tab-line
  :disabled t
  :straight nil
  :custom
  (tab-line-new-tab-choice nil)
  (tab-line-separator nil)
  (tab-line-close-button-show nil)
  :init (global-tab-line-mode))

(use-package eyebrowse
  :commands (eyebrowse-mode)
  :init (eyebrowse-mode 1))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package hungry-delete
  :init (global-hungry-delete-mode 1))

(use-package key-chord
  :custom (key-chord-two-keys-delay 0.05)
  :init (key-chord-mode 1))

(use-package prescient
  :custom
  (prescient-save-file (djm/emacs-cache "prescient-save.el"))
  :config (prescient-persist-mode))

(use-package company
  :demand t
  :commands global-company-mode
  :bind (:map company-active-map
              ("RET" . nil)
              ([return] . nil)
              ("TAB" . company-complete-selection)
              ([tab] . company-complete-selection)
              ("C-f" . company-complete-common)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :custom
  (company-require-match 'never)
  (company-async-timeout 5)
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-tng-frontend
                       company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  (company-backends '(company-capf
                      company-files))
  :config
  (global-company-mode 1)
  (use-package company-statistics
    :init (company-statistics-mode 1)
    :custom (company-statistics-file
             (djm/emacs-cache "company-statistics-cache.el")))

  (use-package company-math
    :config (add-to-list 'company-backends '(company-math-symbols-unicode
                                     company-math-symbols-latex)))
  (use-package company-flx :init (company-flx-mode 1))
  (use-package company-prescient :init (company-prescient-mode 1)))

(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

(use-package yasnippet
  :commands (yas-reload-all)
  :hook ((term-mode . (lambda () (yas-minor-mode -1)))
         (company-mode . yas-minor-mode))
  :config
  (use-package yasnippet-snippets)
  (use-package ivy-yasnippet
    :custom (ivy-yasnippet-new-snippet yas-new-snippet-default))
  (yas-reload-all)
  (yas-global-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom (rainbow-delimters-max-face-count 5))

(use-package undo-tree :init (global-undo-tree-mode 1))

(use-package posframe)

(use-package counsel
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))

  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("C-w" . ivy-yank-word)
         ("C-r" . ivy-previous-line)
         (:map ivy-switch-buffer-map
               ("C-x k" . ivy-switch-buffer-kill))

         (:map counsel-mode-map
               ([remap dired] . counsel-dired)
               ("M-x" . counsel-M-x)
               ("C-x C-f" . counsel-find-file)
               ("C-x C-d" . counsel-dired-jump)
               ("C-x C-l" . counsel-find-library)
               ("C-x C-r" . counsel-recentf)
               ("C-x C-v" . counsel-set-variable)
               ("C-x C-u" . counsel-unicode-char)
               ("C-x j" . counsel-mark-ring)
               ("C-c g" . counsel-grep)
               ("C-c h" . counsel-command-history)
               ("C-c j" . counsel-git)
               ("C-c j" . counsel-git-grep)
               ("C-c r" . counsel-rg)
               ("C-c z" . counsel-fzf)
               ("C-c c w" . counsel-colors-web)
               ("C-h F" . counsel-describe-face)
               ("C-h f" . counsel-describe-function)
               ("C-h v" . counsel-describe-variable))

         ("C-s" . swiper)
         ("C-c c s" . swiper-isearch)
         ("C-c c r" . swiper-isearch-backward)
         ("C-S-s" . swiper-all)
         :map swiper-map
         ("M-%" . swiper-query-replace)
         ("M-s" . swiper-isearch-toggle)
         :map isearch-mode-map
         ("M-s" . swiper-isearch-toggle))

  :custom
  (ivy-extra-directories nil)
  (ivy-dynamic-exhibit-delay-ms 250)
  (ivy-use-selectable-prompt t)
  (ivy-format-function #'ivy-format-function-arrow)
  (ivy-height 10)
  (ivy-initial-inputs-alist nil)
  (ivy-case-fold-search-default t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-count-format "(%d/%d) ")

  :config
  (with-eval-after-load 'ivy
    (push (cons #'swiper (cdr (assq t ivy-re-builders-alist)))
          ivy-re-builders-alist)
    (push (cons #'swiper-isearch (cdr (assq t ivy-re-builders-alist)))
          ivy-re-builders-alist)
    (push (cons #'counsel-M-x #'ivy--regex-fuzzy) ivy-re-builders-alist))

  (when (executable-find "rg")
    (setq counsel-grep-base-command
          "rg -S --no-heading --line-number --color never '%s' %s"))

  (use-package ivy-hydra)
  (use-package ivy-prescient
    :custom (ivy-prescient-retain-classic-highlighting t)
    :init (ivy-prescient-mode 1))

  (use-package ivy-posframe
    :init (ivy-posframe-mode 1)
    :functions (ivy-posframe-display-at-window-bottom-left
                ivy-posframe-display-at-frame-center)
    :config
    (push (cons #'swiper nil)
          ivy-posframe-display-functions-alist)
    (push (cons t #'ivy-posframe-display-at-frame-center)
          ivy-posframe-display-functions-alist))

  (use-package counsel-projectile
    :after (counsel projectile)
    :config (counsel-projectile-mode 1))

  (use-package auto-insert
    :straight nil
    :bind (("C-c ci a" . auto-insert)))

  (use-package amx
    :init (amx-mode 1)
    :custom (amx-save-file (djm/emacs-cache "amx-items"))))

(use-package avy
  :bind (:map dired-mode-map
              ("." . avy-goto-word-or-subword-1))
  :custom (avy-style 'de-bruijn)
  :chords
  ("jj" . avy-goto-char-timer)
  ("jk" . avy-goto-word-or-subword-1)
  ("jl" . avy-goto-line)
  :config (avy-setup-default))

(use-package dimmer
  :custom (dimmer-exclusion-regexp (rx (or "posframe" "which-key")))
  :config (dimmer-mode))

(use-package ispell
  :straight nil
  :ensure-system-package (hunspell . "trizen -S hunspell")
  :custom
  (ispell-dictionary "en_US")
  (ispell-program-name (executable-find "hunspell"))
  (ispell-really-hunspell t)
  (ispell-silently-savep t))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup))
  :config
  (use-package git-commit
    :custom (git-commit-summary-max-length 50))

  (use-package git-gutter
    :commands (global-git-gutter-mode)
    :init (global-git-gutter-mode 1)))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package projectile
  :custom
  (projectile-cache-file (djm/emacs-cache "projectile.cache"))
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-known-projects-file (djm/emacs-cache "projectile-bookmarks.eld"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-indication-mode 'right-fringe))

(use-package sh-script
  :ensure-system-package shfmt
  :mode ((rx (and (? ".") (or "bash" "zsh"))) . sh-mode)
  :custom
  (sh-indentation 2)
  (sh-basic-offset 2))

(provide 'init)
;;; init.el ends here
