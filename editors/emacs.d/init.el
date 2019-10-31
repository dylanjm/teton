;;; init.el --- Emacs main configuration file -*- lexical-binding: t; buffer-read-only: t; coding: utf-8-*-
;;;
;;; Commentary:
;;; Emacs config by dylanjm
;;; Do not change this file.  Main config is located in emacs.d/dotemacs.org
;;;
;;; Code:

(use-package cus-start
  :straight nil
  :custom
  (ad-redefinition-action 'accept)
  (auto-save-list-file-prefix nil)
  (cursor-in-non-selected-windows nil)
  (cursor-type 'bar)
  (custom-file (make-temp-file "emacs-custom"))
  (debug-on-error t)
  (display-time-default-load-average nil)
  (echo-keystrokes 0.02)
  (enable-recursive-minibuffers t)
  (fill-column 80)
  (ffap-machine-p-known 'reject)
  (frame-title-format '("%b - Emacs"))
  (icon-title-format frame-title-format)
  (indent-tabs-mode nil)
  (inhibit-compacting-font-caches t)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  (load-prefer-newer t)
  (message-log-max 10000)
  (mode-line-in-non-selected-windows nil)
  (mouse-wheel-progressive-speed nil)
  (mouse-wheel-scroll-amount '(1))
  (ring-bell-function #'ignore)
  (select-enable-clipboard t)
  (set-horizontal-scroll-bar-mode nil)
  (scroll-conservatively most-positive-fixnum)
  (scroll-margin 5)
  (scroll-preserve-screen-position t)
  (scroll-step 1)
  (sentence-end-double-space nil)
  (tab-always-indent 'complete)
  (tab-width 4)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (vc-follow-symlinks t)
  (window-combination-resize t))

(use-package mule
  :demand t
  :straight nil
  :init
  (prefer-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")
  (set-keyboard-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-selection-coding-system 'utf-8)
  (modify-coding-system-alist 'process "*" 'utf-8)
  (set-file-name-coding-system 'utf-8))

(use-package files
  :demand t
  :straight nil
  :init
  (setq-default backup-by-copying t
                confirm-kill-processes nil
                create-lockfiles nil
                delete-old-versions t
                insert-directory-program "gls"
                kept-new-versions 6
                kept-old-versions 2
                require-final-newline t
                view-read-only t
                version-control t))

(use-package autorevert
  :demand t
  :straight nil
  :init
  (setq-default auto-revert-verbose nil
                global-auto-revert-non-file-buffers t
                auto-revert-use-notify nil)
  (global-auto-revert-mode 1))

(use-package recentf
  :demand t
  :straight nil
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 20
        recentf-auto-cleanup-timer 'never)
  (recentf-mode 1))

(use-package osx-trash
  :defer 2.0
  :init
  (setq delete-by-moving-to-trash t)
  (osx-trash-setup))

(use-package hl-line
  :defer 3
  :straight nil
  :commands (hl-line-mode global-hl-line-mode))

(use-package frame
  :demand t
  :straight nil
  :init
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1)
  (blink-cursor-mode 0)
  (global-unset-key (kbd "C-z"))
  (window-divider-mode 1))

(use-package simple
  :demand t
  :straight nil
  :init
  (setq column-number-mode nil
        eval-expression-print-length nil
        eval-expression-print-level nil
        line-number-mode nil
        line-move-visual nil
        set-mark-command-repeat-pop t
        track-eol t))

(use-package pixel-scroll
  :demand t
  :straight nil
  :init (pixel-scroll-mode 1))

(use-package ns-win
  :demand t
  :straight nil
  :init
  (setq mac-command-modifier 'meta
        mac-option-modifier 'meta
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none
        mac-function-modifier 'hyper
        ns-pop-up-frames nil
        ns-use-native-fullscreen nil
        ns-use-thin-smoothing t))

(use-package windmove
  :bind (("C-c w l" . windmove-left)
         ("C-c w r" . windmove-right)
         ("C-c w p" . windmove-up)
         ("C-c w n" . windmove-down))
  :custom (windmove-default-keybindings 'shift))

(use-package saveplace
  :demand t
  :straight nil
  :init (save-place-mode 1))

(use-package savehist
  :demand t
  :straight nil
  :init
  (setq history-delete-duplicates t
        savehist-autosave-interval 300
        savehist-save-minibuffer-history 1
        savehist-additional-variables '(kill-ring search-ring))
  (savehist-mode 1))

(use-package focus-autosave-mode
  :defer 10
  :config (focus-autosave-mode 1))

(use-package default-text-scale
  :defer 10
  :commands (default-text-scale-increase
             default-text-scale-decrease
             default-text-scale-reset)
  :bind (("C-c <up>" . default-text-scale-increase)
         ("C-c <down>" . default-text-scale-decrease)
         ("C-M-]". default-text-scale-reset))
  :custom (default-text-scale-amount 30))

(use-package delsel
  :demand t
  :bind (:map mode-specific-map
              ("C-g" . minibuffer-keyboard-quit))
  :init (delete-selection-mode 1))

(use-package align
  :disabled t
  :straight nil
  :general ("C-x a a" #'align-regexp))

(use-package zop-to-char
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package undo-tree
  :defer 10.0
  :init (global-undo-tree-mode 1))

(use-package aggressive-indent
  :defer 10.0
  :commands (aggressive-indent-mode))

(use-package hungry-delete
  :defer 10.0
  :commands (hungry-delete-mode))

(use-package prog-mode
  :straight nil
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . show-paren-mode)
         (prog-mode . display-line-numbers-mode)
         (prog-mode . display-fill-column-indicator-mode))
  :custom
  (prettify-symbols-unprettify-at-point 'right-edge))

(use-package term
  :straight nil
  :hook (term-mode . (lambda () (hl-line-mode -1))))

(use-package dired
:defer 3
:straight nil
:functions (dired wdired-change-to-wdired-mode)
:bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode))
              :custom
              (dired-auto-revert-buffer t)
              (dired-dwim-target t)
              (dired-guess-shell-gnutar "gtar")
              (dired-listing-switches "-alhF --group-directories-first -v")
              (dired-ls-F-marks-symlinks t)
              (dired-recursive-deletes 'always)
              (dired-recursive-copies 'always))

(use-package dired-aux
  :straight nil
  :after (dired))

(use-package dired-x
  :straight nil
  :after (dired))

(use-package diredfl
  :after (dired)
  :hook (dired-mode . diredfl-global-mode))

(use-package dired-ranger
  :bind (:map dired-mode-map
              ("C-c C-c" . dired-ranger-copy)
              ("C-c C-m" . dired-ranger-move)
              ("C-c C-p" . dired-ranger-move)
              ("C-c C-b" . dired-ranger-bookmark)
              ("C-c b v" . dired-ranger-bookmark-visit)))

(use-package dired-git-info
  :bind (:map dired-mode-map
              (":" . dired-git-info-mode)))

(use-package dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

(use-package fd-dired
  :after (dired))

(use-package dired-sidebar
  :bind ("M-\\" . dired-sidebar-toggle-sidebar)
  :custom (dired-sidebar-theme 'vscode)
  :config
  (use-package vscode-icon))

(use-package async
  :defer 1.5
  :preface
  (autoload 'aysnc-bytecomp-package-mode "async-bytecomp")
  (autoload 'dired-async-mode "dired-async.el" nil t)
  :config
  (async-bytecomp-package-mode 1)
  (dired-async-mode 1))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-formats '((mark modified " " (mode 1 1) " " (name 25 25 :left :elide) " " filename-and-process)))
  (ibuffer-never-show-predicates (list (rx (or "*Messages*"
                                               "*magit-"
                                               "*git-auto-push*"
                                               "*Backtrace*"
                                               "*new*"
                                               "*Org*"
                                               "*Flycheck error messages*"
                                               "*Help*")))))

(use-package ibuf-ext
  :straight nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :custom (ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-projectile
  :defer 5.0
  :commands (ibuffer-projectile-set-filter-groups)
  :functions (ibuffer-do-sort-by-alphabetic)
  :preface
  (defun config-ibuffer--setup-buffer ()
    (ibuffer-projectile-set-filter-groups)
    (add-to-list 'ibuffer-filter-groups '("Dired" (mode . dired-mode)))
    (add-to-list 'ibuffer-filter-groups '("Ensime" (predicate . (s-matches? "Ensime" (buffer-name)))))
    (add-to-list 'ibuffer-filter-groups '("System" (predicate . (-contains? '("*Messages*" "*scratch*") (buffer-name)))))
    (add-to-list 'ibuffer-filter-groups '("Shells" (mode . eshell-mode)))
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))
    (when (bound-and-true-p page-break-lines-mode)
      (page-break-lines--update-display-tables)))
  :init
  (add-hook 'ibuffer-hook #'config-ibuffer--setup-buffer)
  :custom
  (ibuffer-projectile-prefix ""))

(use-package ws-butler
  :commands (ws-butler-global-mode)
  :hook ((prog-mode . (lambda () (require 'ws-butler)))
         (text-mode . (lambda () (require 'ws-butler))))
  :config (ws-butler-global-mode 1))

(use-package eldoc
  :defer 2.0
  :custom (eldoc-idle-delay 2))

(use-package which-key
  :defer 2.0
  :custom (which-key-idle-delay 0.5)
  :config (which-key-mode))

(use-package man
  :defer 2.0)

(use-package help
  :defer 2.0
  :straight nil
  :init
  (setq help-window-select t)
  (advice-add 'help-window-display-message :override #'ignore))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package multiple-cursors
  :disabled t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package eww
  :defer 5.0
  :straight nil)

(use-package browse-url
  :defer 5.0
  :straight nil
  :custom (browse-urls-browser-function "firefox"))

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents . 5)
                     (projects . 5)
                     (bookmarks . 5)
                     (agenda . 5)))
  :config
  (set-face-bold 'dashboard-heading-face t))

(use-package doom-themes
  :demand t
  :config
  (setq doom-gruvbox-brighter-comments t
        doom-themes-enable-italic t
        doom-themes-enable-bold t)
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config))

;; Emacs 27 added new `:extend' keyword which breaks most themes
(if (boundp 'hl-line)
    (set-face-attribute hl-line nil :extend t))

(dolist (face '(region secondary-selection))
  (set-face-attribute face nil :extend t))

(with-eval-after-load 'org
  (dolist (face '(org-block
                  org-block-begin-line
                  org-block-end-line
                  org-level-1
                  org-quote))
    (set-face-attribute face nil :extend t)))

(with-eval-after-load 'magit
  (dolist (face '(magit-diff-hunk-heading
                  magit-diff-hunk-heading-highlight
                  magit-diff-hunk-heading-selection
                  magit-diff-hunk-region
                  magit-diff-lines-heading
                  magit-diff-lines-boundary
                  magit-diff-conflict-heading
                  magit-diff-added
                  magit-diff-removed
                  magit-diff-our
                  magit-diff-base
                  magit-diff-their
                  magit-diff-context
                  magit-diff-added-highlight
                  magit-diff-removed-highlight
                  magit-diff-our-highlight
                  magit-diff-base-highlight
                  magit-diff-their-highlight
                  magit-diff-context-highlight
                  magit-diff-whitespace-warning
                  magit-diffstat-added
                  magit-diffstat-removed
                  magit-section-heading
                  magit-section-heading-selection
                  magit-section-highlight
                  magit-section-secondary-heading
                  magit-diff-file-heading
                  magit-diff-file-heading-highlight
                  magit-diff-file-heading-selection))
    (set-face-attribute face nil :extend t)))

(use-package minions
  :defer 0.5
  :custom
  (minions-mode-line-lighter "...")
  (minions-mode-line-delimiters '("" . ""))
  :config (minions-mode 1))

(use-package tab-line
  :disabled t
  :straight nil
  :custom
  (tab-line-new-tab-choice nil)
  (tab-line-separator nil)
  (tab-line-close-button-show nil)
  :init (global-tab-line-mode))

(use-package page-break-lines
  :defer 1.0
  :config
  (setq page-break-lines-modes '(prog-mode
                                 ibuffer-mode
                                 text-mode
                                 compilation-mode
                                 help-mode
                                 org-agenda-mode))
    (global-page-break-lines-mode))

(use-package posframe
  :defer 1.0
  :custom
  (posframe-arghandler #'hemacs-posframe-arghandler)
  :config
  (defun hemacs-posframe-arghandler (posframe-buffer arg-name value)
    (let ((info '(:internal-border-width 15 :min-width 80)))
      (or (plist-get info arg-name) value))))

(use-package which-key-posframe
  :defer 5.0
  :config (which-key-posframe-mode)
  :custom (which-key-posframe-poshandler
           'posframe-poshandler-point-bottom-left-corner))

(use-package org-src
  :defer 1.0
  :straight nil
  :preface
  (defun config-org--supress-final-newline ()
    (setq-local require-final-newline nil))

  (defun config-org--org-src-delete-trailing-space (&rest _)
    (delete-trailing-whitespace))
  :config
  (setq org-src-window-setup 'split-window-below)
  (add-hook 'org-src-mode-hook #'config-org--supress-final-newline)
  (advice-add 'org-edit-src-exit :before #'config-org--org-src-delete-trailing-space))

(use-package ace-window
  :defer 10.0
  :bind (("C-x o" . ace-window)))

(use-package key-chord
  :custom (key-chord-two-keys-delay 0.05)
  :init (key-chord-mode 1))

(use-package prescient
  :defer 1.0
  :config (prescient-persist-mode))

(use-package dimmer
  :disabled t
  :custom
  (dimmer-fraction 0.33)
  (dimmer-exclusion-regexp-list '(".*Minibuf.*"
                                  ".*which-key.*"
                                  ".*Messages.*"
                                  ".*Async.*"
                                  ".*Warnings.*"
                                  ".*LV.*"
                                  ".*Ilist.*"
                                  ".*posframe.*"
                                  ".*transient.*"))
  :config (dimmer-mode))

(use-package smartparens
  :defer 10
  :functions (sp-backward-delete-char))

(use-package rainbow-delimiters
  :defer 1.0
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom (rainbow-delimters-max-face-count 5))



(use-package hippie-exp
  :defer 5.0
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package auto-insert
  :straight nil
  :bind (("C-c ci a" . auto-insert)))

(use-package company
  :defer 2.0
  :bind (:map company-active-map
              ("RET" . nil)
              ([return] . nil)
              ("TAB" . company-complete-selection)
              ([tab] . company-complete-selection)
              ("C-f" . company-complete-common)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-require-match 'never
        company-async-timeout 10
        company-idle-delay 0.15
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t)
  (global-company-mode 1))

(use-package company-prescient
  :demand t
  :after (company)
  :config (company-prescient-mode 1))

(use-package company-math
  :demand t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package company-flx
  :demand t
  :after (company)
  :config (company-flx-mode 1))

(use-package company-lsp
  :after (lsp-mode)
  :config (setq company-lsp-cache-canidates 'auto))

(use-package company-anaconda
  :after (anaconda-mode)
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package company-box
  :disabled t
  :after (company)
  :config (company-box-mode 1))

(use-package yasnippet
  :defer 5.0
  :commands (yas-reload-all
             yas-global-mode))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package ivy-yasnippet
  :after (yasnippet))

(use-package counsel
  :diminish
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
  (ivy-dynamic-exhibit-delay-ms 250)
  (ivy-use-selectable-prompt t)
  (ivy-initial-inputs-alist nil)
  (ivy-case-fold-search-default t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'name)
  (ivy-count-format "")
  (ivy-flx-limit 2000)

  :config
  (use-package ivy-hydra)

  (use-package ivy-prescient
    :config (ivy-prescient-mode 1))

  (setq counsel-grep-base-command
        "rg -S --no-heading --line-number --color never '%s' %s")

  (setq ivy-re-builders-alist '((t . ivy-prescient-re-builder)
                                (t . ivy--regex-fuzzy)
                                (swiper . ivy--regex-plus)
                                (swiper-isearch . ivy--regex-plus))))

(use-package amx
  :hook (ivy-mode . amx-mode))

(use-package ivy-posframe
  :hook (ivy-mode . ivy-posframe-mode)
  :config
  (setq ivy-posframe-hide-minibuffer t)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)
                                               (swiper . nil))))


(use-package counsel-projectile
  :after (counsel projectile)
  :custom (counsel-projectile-switch-project-action #'dired)
  :config (counsel-projectile-mode 1))

(use-package avy
  :diminish
  :bind (:map dired-mode-map
              ("." . avy-goto-word-or-subword-1))
  :chords
  ("jk" . avy-pop-mark)
  ("jl" . avy-goto-line)
  :config (avy-setup-default))

(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  :config
  (projectile-mode 1))

(use-package ispell
  :defer 5.0
  :straight nil
  :custom
  (ispell-dictionary "en_US")
  (ispell-program-name (executable-find "hunspell"))
  (ispell-really-hunspell t)
  (ispell-silently-savep t))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup)))

(use-package git-commit
  :after (magit)
  :custom (git-commit-summary-max-length 50))

(use-package git-gutter
  :commands (global-git-gutter-mode)
  :config (global-git-gutter-mode 1))

(use-package vterm
  :defer 10)

(use-package vterm-toggle
  :straight (:host github :repo "jixiuf/vterm-toggle")
  :bind (("C-c C-t" . vterm-toggle)
         ("C-c C-y" . term-toggle-cd)))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package flycheck
  :defer 4
  :init
  (defun flycheck-disable-checkers (&rest checkers)
    (unless (bounp 'flycheck-disabled-checkers)
      (setq flycheck-disabled-checkers nil))
    (dolist (checker checkers)
      (cl-pushnew checker flycheck-disabled-checkers)))
  :commands (flycheck-list-errors
             flycheck-error-list-next-error
             flycheck-error-list-previous-error
             flycheck-error-list-goto-error)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  :config
  (global-flycheck-mode 1))

(use-package flycheck-posframe
  :after (flycheck)
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config (add-to-list 'flycheck-posframe-inhibit-functions
                       #'(lambda () (bound-and-true-p company-backend))))

(use-package flycheck-pos-tip
  :after (flycheck)
  :defines flycheck-pos-tip-timeout
  :hook (global-flycheck-mode . flycheck-pos-tip-mode)
  :config (setq flycheck-pos-tip-timeout 30))

(use-package flycheck-popup-tip
  :after (flycheck)
  :hook (flycheck-mode . flycheck-popup-tip-mode))

;; (use-package sh-script
;;   :straight nil
;;   :ensure-system-package shfmt
;;   :mode ((rx (and (? ".") (or "bash" "zsh"))) . sh-mode)
;;   :custom
;;   (sh-indentation 2)
;;   (sh-basic-offset 2))

(use-package ess
  :hook (julia-mode . ess-mode)
  :config
  (add-to-list 'safe-local-variable-values '(outline-minor-mode))
  (add-to-list 'safe-local-variable-values '(whitespace-style
                                             face tabs spaces
                                             trailing lines space-before-tab::space
                                             newline indentation::space empty
                                             space-after-tab::space space-mark
                                             tab-mark newline-mark)))

(use-package lsp-mode
  :hook ((python-mode cc-mode) . lsp-deferred)
  :custom
  (lsp-eldoc-enable-hover nil)
  (lsp-edoc-render-all nil)
  (lsp-prefer-fly-make nil)
  (lsp-restart nil)
  (lsp-enable-on-type-formatting nil)
  :config
  (use-package lsp-clients
    :straight nil))

(use-package lsp-ui
  :after (lsp-mode)
  :bind (("C-c f" . lsp-ui-sideline-apply-code-actions))
  :config
  (setq lsp-ui-sideline-show-hover nil))

(use-package lsp-ui-doc
  :after (lsp-ui lsp-mode)
  :straight nil)

(use-package python
  :hook (python-mode . config-python--init-python-mode)
  :preface
  (progn
    (autoload 'python-indent-dedent-line "python")
    (autoload 'python-shell-get-process "python")

    (defun config-python--init-python-mode ()
      (setq-local comment-inline-offset 2)
      (setq-local tab-width 4)
      (prettify-symbols-mode -1)
      (when (executable-find "ipython")
        (setq-local python-shell-interpreter "ipython")
        (setq-local python-shell-interpreter-args "--simple-promt -i")))

    (defun config-python-backspace ()
      (interactive)
      (if (equal (char-before) ?\s)
          (unless (python-indent-dedent-line)
            (backward-delete-char-untabify 1))
        (sp-backward-delete-char)))

    (defvar config-python-prev-source-buffer)

    (defun config-python-repl-switch-to-source ()
      (interactive)
      (-when-let (buf config-python-prev-source-buffer)
        (when (buffer-live-p buf)
          (pop-to-buffer buf))))

    (defun config-python-repl ()
      (interactive)
      (when (derived-mode-p 'python-mode)
        (setq config-python-prev-source-buffer (current-buffer)))
      (let ((shell-process
             (or (python-shell-get-process)
                 (with-demoted-errors "Error: %S"
                   (call-interactively #'run-python)
                   (python-shell-get-process)))))
        (unless shell-process
          (error "Failed to start python shell properly"))
        (pop-to-buffer (process-buffer shell-process))))
    :config
    (progn
      (setq python-indent-guess-indent-offset nil)
      (setq python-indent-offset 4)
      (setq python-fill-docstring-style 'django))))

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package py-yapf
  :hook (python-mode . python-auto-format-mode)
  :preface
  (progn
    (defvar python-auto-format-buffer t)

    (defun python-auto-format-maybe ()
      (when python-auto-format-buffer
        (py-yapf-buffer)))

    (define-minor-mode python-auto-format-mode
      nil nil nil nil
      (if python-auto-format-mode
          (add-hook 'before-save-hook 'python-auto-format-maybe nil t)
        (remove-hook 'before-save-hook 'python-auto-format-maybe t)))))

(defconst moose-c-style
  '((c-tab-always-indent . t)
    (c-basic-offset . 2)
    (c-hanging-braces-alist . ((substatement-open before after)))
    (c-offsets-alist . ((innamespace .0)
                        (member-init-intro . 4)
                        (statement-block-into . +)
                        (substatement-open . 0)
                        (substatement-label .0)
                        (label .0)
                        (statement-cont . +)
                        (case-label . +))))
  "Moose C++ Programming Style.")

(c-add-style "MOOSE" moose-c-style)

(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
                ("\\.i$" . conf-mode)
                ("tests" . conf-mode)
                ("\\.cu". c++-mode))
              auto-mode-alist))

(defun djm--moose-hook ()
  (c-set-style "MOOSE")
  (setq-local indent-tabs-mode nil)
  (c-toggle-auto-hungry-state)
  (c-toggle-auto-newline)
  (c-toggle-auto-state)
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook 'djm--moose-hook)

(provide 'init)
;;; init.el ends here
