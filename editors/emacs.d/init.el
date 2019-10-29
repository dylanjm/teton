(use-package dashboard
  :init (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents . 5)
                     (projects . 5)
                     (bookmarks . 5)
                     (agenda . 5)))
  :config
  (set-face-bold 'dashboard-heading-face t))

(use-package org
  :straight org-plus-contrib
  :hook (org-mode . visual-line-mode)
  :requires (org-capture org-protocol)
  :custom
  (org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE BUT" "DONE")
                       (sequence "MAYBE" "CANCELED" "|")))
  :config
  (org-add-link-type "project" 'projectile-switch-project-by-name)
  (use-package org-habit-plus
    :straight (org-habit-plus :type git :host github
                              :repo "oddious/org-habit-plus")
    :custom
    (org-habit-scheduled-past-days org-scheduled-past-days))
  (use-package org-make-toc
    :straight (org-make-toc :type git :host github
                            :repo "alphapapa/org-make-toc")
    :init (org-make-toc-mode 1)))

(provide 'init)
;;; init.el ends here

;;; init.el --- Emacs main configuration file -*- lexical-binding: t; buffer-read-only: t; no-byte-compile: t; coding: utf-8-*-
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
  (auto-window-vscroll nil)
  (backup-by-copying t)
  (browse-urls-browser-function "firefox")
  (confirm-kill-processes nil)
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (cursor-type 'bar)
  (custom-file (make-temp-file "emacs-custom"))
  (debug-on-error t)
  (display-time-default-load-average nil)
  (echo-keystrokes 0.02)
  (enable-recursive-minibuffers t)
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)
  (fill-column 80)
  (frame-inhibit-implied-resize t)
  (fast-but-imprecise-scrolling t)
  (ffap-machine-p-known 'reject)
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
  (line-spacing 1)
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
  (scroll-preserve-screen-position t)
  (scroll-step 1)
  (select-enable-clipboard t)
  (sentence-end-double-space nil)
  (tab-always-indent 'complete)
  (tab-width 4)
  (transient-history-file (djm/emacs-cache "transient/history.el"))
  (transient-levels-file (djm/emacs-cache "transient/levels.el"))
  (transient-values-file (djm/emacs-cache "transient/values.el"))
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
  (blink-cursor-mode 0))

(use-package osx-trash :init (osx-trash-setup))

(use-package files
  :straight nil
  :custom
  (auto-save-file-name-transforms `((".*" ,(djm/emacs-cache "backups/") t)))
  (backup-directory-alist `(("." . ,(djm/emacs-cache "backups/")))))

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

  (use-package fringe
    :straight nil
    :custom
    (fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                                  fringe-indicator-alist))
    :config (fringe-mode '(10 . 8)))

  (use-package pixel-scroll
    :demand t
    :straight nil
    :config (pixel-scroll-mode 1))

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

  (use-package windmove
    :bind (("C-c w l" . windmove-left)
           ("C-c w r" . windmove-right)
           ("C-c w p" . windmove-up)
           ("C-c w n" . windmove-down))
    :custom (windmove-default-keybindings 'shift))

(advice-add 'help-window-display-message :override #'ignore)

(use-package focus-autosave-mode :init (focus-autosave-mode))

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

(use-package recentf
  :straight nil
  :functions (recentf-save-list)
  :init (recentf-mode 1)
  :custom
  (recentf-save-file (djm/emacs-cache "recentf"))
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 20)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("\\.?cache"
                     ".cask"
                     "url"
                     "COMMIT_EDITMSG\\'"
                     "bookmarks"
                     "NEWS"
                     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
                     "^/tmp/"nnn
                     "^/ssh:"
                     "\\.?ido\\.last$"
                     "\\.revive$"
                     "/TAGS$"
                     "^/var/folders/.+$"
                     (lambda (file)
                       (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (run-at-time nil (* 3 60) (lambda () (let ((save-silently t)) (recentf-save-list)))))

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
    :commands (ansi-term)
    :preface
    (defun config-basic-settings--shell-hl-line-off ()
      (when (bound-and-true-p hl-line-mode)
        (hl-line-mode -1)))
    :config
    (add-hook 'term-mode-hook #'config-basic-settings--shell-hl-line-off))

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

  (use-package ibuffer
    :bind (([remap list-buffers] . ibuffer))
    :hook (ibuffer-mode . hl-line-mode)
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
  :config
  (setq ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-projectile
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
  :config
  (setq ibuffer-projectile-prefix ""))

  (use-package whitespace
    :straight nil
    :hook (((prog-mode text-mode conf-mode) . whitespace-mode)
           (before-save . delete-trailing-whitespace))
    :custom
    (whitespace-style '(face indentation space-after-tab space-before-tab
                             tab-mark empty trailing)))

  (use-package zop-to-char
    :bind (("M-z" . zop-to-char)
           ("M-Z" . zop-up-to-char)))

  (use-package eldoc
    :custom (eldoc-idle-delay 2))

  (use-package which-key
    :custom (which-key-idle-delay 0.5)
    :config (which-key-mode))

  (use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . helpful-callable)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key))

  (use-package async
    :preface
    (progn
      (autoload 'aysnc-bytecomp-package-mode "async-bytecomp")
      (autoload 'dired-async-mode "dired-async.el" nil t))
    :config
    (progn
      (async-bytecomp-package-mode 1)
      (dired-async-mode 1)))

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
  (load-theme 'doom-gruvbox t)
  (setq doom-gruvbox-brighter-comments t)
  (doom-themes-org-config)
  (dolist (face '(region hl-line secondary-selection))
    (set-face-attribute face nil :extend t))
  (with-eval-after-load 'org
    (dolist (face '(org-block
                    org-block-begin-line
                    org-block-end-line
                    org-level-1
                    org-quote))
      (set-face-attribute face nil :extend t)))
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

(use-package page-break-lines
  :demand t
  :commands (global-page-break-lines-mode)
  :config
  (progn
    (setq page-break-lines-modes
          '(prog-mode
            ibuffer-mode
            text-mode
            compilation-mode
            help-mode
            org-agenda-mode))
    (global-page-break-lines-mode)))

(use-package man)

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package aggressive-indent
  :commands (aggressive-indent-mode))

(use-package hungry-delete
  :commands (hungy-delete-mode))

(use-package key-chord
  :custom (key-chord-two-keys-delay 0.05)
  :init (key-chord-mode 1))

(use-package prescient
  :custom (prescient-save-file (djm/emacs-cache "prescient-save.el"))
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
  :functions (sp-backward-delete-char))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom (rainbow-delimters-max-face-count 5))

(use-package undo-tree
  :init (global-undo-tree-mode 1))

(use-package posframe
  :custom
  (posframe-arghandler #'hemacs-posframe-arghandler)
  :config
  (defun hemacs-posframe-arghandler (posframe-buffer arg-name value)
    (let ((info '(:internal-border-width 15 :min-width 80)))
      (or (plist-get info arg-name) value))))

(use-package which-key-posframe
  :config (which-key-posframe-mode)
  :custom (which-key-posframe-poshandler
           'posframe-poshandler-point-bottom-left-corner))

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
  (company-transformers '(company-sort-by-statistics
                          company-sort-by-occurrence))
  (company-frontends '(company-preview-common-frontend
                       company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend))
  (company-backends '(company-capf
                      company-files
                      company-xcode
                      company-keywords))
  :config
  (global-company-mode 1)

  (use-package company-statistics
    :init (company-statistics-mode 1)
    :custom (company-statistics-file
             (djm/emacs-cache "company-statistics-cache.el")))
  (use-package company-math
    :init
    (add-to-list 'company-backends 'company-math-symbols-unicode)
    (add-to-list 'company-backends 'company-math-symbols-latex))
  (use-package company-flx
    :init (company-flx-mode 1))
  (use-package company-prescient
    :init (company-prescient-mode 1))
  (use-package company-lsp
    :init (setq company-lsp-cache-canidates 'auto))
  (use-package company-anaconda
    :config (add-to-list 'company-backends 'company-anaconda)))

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
  (ivy-dynamic-exhibit-delay-ms 250)
  (ivy-use-selectable-prompt t)
  (ivy-format-function #'ivy-format-function-arrow)
  (ivy-height 10)
  (ivy-initial-inputs-alist nil)
  (ivy-case-fold-search-default t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-count-format "")
  (ivy-flx-limit 2000)

  :config
  (use-package ivy-hydra)
  (use-package ivy-prescient
    :custom (ivy-prescient-retain-classic-highlighting t)
    :init (ivy-prescient-mode 1))

  (use-package ivy-posframe
    :init (ivy-posframe-mode 1)
    :functions (ivy-posframe-display-at-window-bottom-left
                ivy-posframe-display-at-frame-center)
    :custom
    (ivy-posframe-border-width 20)
    (ivy-posframe-style 'frame-center)
    ;(ivy-posframe-hide-minibuffer t)
    (ivy-posframe-parameters '((alpha 100 100)))

    :config
    (push (cons #'swiper nil)
          ivy-posframe-display-functions-alist)
    (push (cons t #'ivy-posframe-display-at-frame-center)
          ivy-posframe-display-functions-alist))

  (use-package counsel-projectile
    :after (counsel projectile)
    :custom
    (counsel-projectile-switch-project-action #'dired)
    :config (counsel-projectile-mode 1))

  (use-package auto-insert
    :straight nil
    :bind (("C-c ci a" . auto-insert)))

  (use-package amx
    :init (amx-mode 1)
    :custom (amx-save-file (djm/emacs-cache "amx-items")))

  (use-package flx)

  (when (executable-find "rg")
    (setq counsel-grep-base-command
          "rg -S --no-heading --line-number --color never '%s' %s"))

  (with-eval-after-load 'ivy
    (push (cons #'swiper (cdr (assq t ivy-re-builders-alist)))
          ivy-re-builders-alist)
    (push (cons #'swiper-isearch (cdr (assq t ivy-re-builders-alist)))
          ivy-re-builders-alist)
    (push (cons #'counsel-M-x #'ivy--regex-fuzzy) ivy-re-builders-alist)
    (push (cons t #'ivy--regex-fuzzy) ivy-re-builders-alist)))

(use-package avy
  :bind (:map dired-mode-map
              ("." . avy-goto-word-or-subword-1))
  :custom (avy-style 'de-bruijn)
  :chords
  ("jj" . avy-goto-char-timer)
  ("jk" . avy-goto-word-or-subword-1)
  ("jl" . avy-goto-line)
  :config (avy-setup-default))


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


(use-package projectile
  :custom
  (projectile-cache-file (djm/emacs-cache "projectile.cache"))
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-known-projects-file (djm/emacs-cache "projectile-bookmarks.eld"))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))

(use-package vterm)
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package shell-pop
  :bind ("C-x t" . shell-pop)
  :custom
  (shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell)))))
  (shell-pop-term-shell (getenv "SHELL"))
  :config
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package flycheck
  :hook ((after-init . global-flycheck-mode)
         (prog-mode . flycheck-mode-on-safe))
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
  (use-package flycheck-posframe
    :hook (flycheck-mode . flycheck-posframe-mode)
    :config (add-to-list 'flycheck-posframe-inhibit-functions
                         #'(lambda () (bound-and-true-p company-backend))))
  (use-package flycheck-pos-tip
    :defines flycheck-pos-tip-timeout
    :hook (global-flycheck-mode . flycheck-pos-tip-mode)
    :config (setq flycheck-pos-tip-timeout 30))
  (use-package flycheck-popup-tip
    :hook (flycheck-mode . flycheck-popup-tip-mode)))

(use-package sh-script
  :ensure-system-package shfmt
  :mode ((rx (and (? ".") (or "bash" "zsh"))) . sh-mode)
  :custom
  (sh-indentation 2)
  (sh-basic-offset 2))

(use-package ess
  :init
  (progn
    (add-to-list 'safe-local-variable-values '(outline-minor-mode))
    (add-to-list 'safe-local-variable-values '(whitespace-style
                                               face tabs spaces
                                               trailing lines space-before-tab::space
                                               newline indentation::space empty
                                               space-after-tab::space space-mark
                                               tab-mark newline-mark))))

(use-package lsp-mode
  :hook ((python-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (c-mode-common . lsp-deferred))
  :preface
  (defun config-lsp--setup-buffer ()
    (when (gethash "documentHighlightProvider" (lsp--server-capabilities))
      (highlight-thing-mode -1)))
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (lsp-auto-guess-root t)
  (lsp-edoc-render-all nil)
  (lsp-prefer-fly-make nil)
  (lsp-session-file (djm/emacs-cache "lsp-session-v1"))
  (lsp-restart 'ignore)
  (lsp-enable-on-type-formatting nil)
  :config
  (progn
    (when (and lsp-auto-configure lsp-auto-require-clients)
      (require 'lsp-clients))

    (add-hook 'lsp-after-open-hook #'config-lsp--setup-buffer)
    (define-key lsp-mode-map (kbd "C-c SPC") #'lsp-execute-code-action)))

(use-package dap-mode
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :preface
  (defvar config-lsp--dap-cache-dir (djm/emacs-cache "dap"))
  :init
  (progn
    (f-mkdir config-lsp--dap-cache-dir)
    (setq dap-utils-extension-path (expand-file-name "extensions" config-lsp--dap-cache-dir)))
  :config
  (setq dap-breakpoints-file (expand-file-name "breakpoints" config-lsp--dap-cache-dir)))

(use-package lsp-ui
  :preface
  (progn
    (defun config-lsp-toggle-ui-overlays (&optional should-enable)
      (interactive (list (not (bound-and-true-p lsp-ui-mode))))
      (cond
       (should-enable
        (lsp-ui-mode +1)
        (eldoc-mode -1))
       (t
        (lsp-ui-mode -1)
        (eldoc-mode +1))))

    (defun config-lsp-configure-ui ()
      (config-lsp-toggle-ui-overlays t)
      (lsp-ui-flycheck-enable t)))
  :init
  (progn
    (use-package lsp-ui-flycheck :straight nil)
    (with-eval-after-load 'lsp-mode
      (define-key lsp-mode-map (kbd "C-C u") #'config-lsp-toggle-ui-overlays)))
  :config
  (progn
    (add-hook 'lsp-after-open-hook #'config-lsp-configure-ui)
    (setq lsp-ui-sideline-enable t
          lsp-ui-sideline-show-code-actions nil
          lsp-ui-sideline-show-flycheck nil
          lsp-ui-doc-enable nil)
    (define-key lsp-ui-mode-map (kbd "C-c C-c") #'lsp-goto-type-definition)
    (define-key lsp-ui-mode-map (kbd "C-c i") #'lsp-goto-implementation)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

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
      (setq python-fill-docstring-style 'django)

      (push "jupyter" python-shell-completion-native-disabled-interpreters)

      (define-key python-mode-map [remap python-indent-dedent-line-backspace] #'config-python-backspace)
      (define-key python-mode-map [remap python-shell-switch-to-shell] #'config-python-repl)
      (define-key inferior-python-mode-map (kbd "C-c C-z") #'config-python-repl-switch-to-source)

      (add-to-list 'display-buffer-alist
                   `(,(rx bos "*Python*" eos)
                     (display-buffer-reuse-window
                      display-buffer-at-bottom)
                     (reusable-frames . visible)
                     (slot . 0)
                     (window-height . 0.2))))))

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
