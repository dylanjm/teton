;;; init.el --- Emacs main configuration file -*-
;;;
;;; Commentary:
;;; Emacs config by dylanjm
;;;
;;; Code:
(defun doom-defer-garbage-collection-h ()
  "Increase `gc-cons-threshold' to stave off garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Restore `gc-cons-threshold' to a reasonable value so the GC can do its job."
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

;; Not restoring these to their defaults will cause stuttering/freezes.
(add-hook 'emacs-startup-hook #'doom-restore-garbage-collection-h)

;; When Emacs loses focus seems like a great time to do some garbage collection
;; all sneaky breeky like, so we can return to a fresh(er) Emacs.
(add-hook 'focus-out-hook #'garbage-collect)

(setq-default  auto-save-file-name-transforms '((".*" "~/.cache/emacs/emacs-backups" t))
               auto-window-vscroll nil
               backup-by-copying t
               backup-directory-alist '(("." . "~/.cache/emacs/emacs-backups"))
               create-lockfiles nil
               cursor-in-non-selected-windows nil
               cursor-type 'bar
               display-time-default-load-average nil
               fill-column 80
               frame-title-format '("%b - Emacs")
               help-window-select t
               icon-title-format frame-title-format
               indent-tabs-mode nil
               inhibit-compacting-font-caches t
               inhibit-default-init t
               inhibit-startup-screen t
               inhibit-startup-echo-area-message t
               initial-scratch-message ""
               load-prefer-newer t
               mode-line-in-non-selected-windows nil
               ns-use-thin-smoothing t
               redisplay-dont-pause t
               ring-bell-function #'ignore
               sentence-end-double-space nil
               select-enable-clipboard t
               tab-width 4
               vc-follow-symlinks t
               view-read-only t
               uniquify-buffer-name-style 'forward
               use-file-dialog nil
               use-dialog-box nil
               window-divider-default-places t
               window-divider-default-bottom-width 1
               window-divider-default-right-width 1)

(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) .1))
        mouse-wheel-progressive-speed nil))

(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively most-positive-fixnum)
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8-unix)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(set-file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8 . utf-8))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-hook 'after-load-theme-hook
          (lambda ()
            (let ((bg (frame-parameter nil 'background-mode)))
              (set-frame-parameter nil 'ns-appearance bg)
              (setcdr (assq 'ns-appearance default-frame-alist) bg))))
(add-hook 'window-setup-hook #'window-divider-mode)
(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(global-display-line-numbers-mode)

(blink-cursor-mode 0)
(fringe-mode 0)

(fset 'menu-bar-open nil)
(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist
             '(font . "-*-Iosevka Nerd Font Mono-ultralight-normal-normal-*-18-*-*-*-m-0-iso10646-1"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-verbose t
        use-package-always-defer t))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package server
  :ensure nil
  :hook (after-init . server-mode))

(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

(use-package simple
  :ensure nil
  :hook ((window-setup . size-indication-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        line-move-visual nil
        track-eol t
        set-mark-command-repeat-pop t)
  (setq-default show-trailing-whitespace nil)
  (defun enable-trailing-whitespace ()
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (history-delete-duplicates t)
  (history-length t)
  (savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   extended-command-history
                                   kill-ring))
  (savehist-autosave-intervall 300)
  (savehist-file (expand-file-name "~/.cache/emacs/emacs-history"))
  (savehist-save-minibuffer-history 1)
  (history-length 1000))

(use-package ns-win
  :ensure nil
  :defer t
  :if (eq system-type 'darwin)
  :config
  (and (or (daemonp)
           (display-graphic-p))
       (require 'ns-auto-titlebar nil t)
       (ns-auto-titlebar-mode 1))
  (setq ns-pop-up-frames nil
        ns-use-native-fullscreen nil
        mac-option-modifier 'meta
        mac-command-modifier 'meta
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none
        mac-function-modifier 'hyper)
  (add-hook 'after-make-frame-functions
            (defun init-menu-bar-in-gui-frames-h (frame)
              (when (display-graphic-p frame)
                (set-frame-parameter frame 'menu-bar-lines 1)))))

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . show-paren-mode)
         (prog-mode . prettify-symbols-mode))
  :init
  (setq-default prettify-symbols-alist '(("lambda" . ?λ) ("<-" . ?⭠)))
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(use-package bmx-mode
  :after company
  :hook (after-init . bmx-mode-setup-defaults))

(use-package fish-mode
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-exclude '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
                     "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
                     (lambda (file) (file-in-directory-p file package-user-dir))))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 200)
  (recentf-save-file "~/.cache/emacs/emacs-recentf")
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package darktooth-theme
  :demand
  :config
  (load-theme 'darktooth t)
  (set-face-attribute 'mode-line nil :height 160)
  (set-face-attribute 'mode-line-inactive nil :height 160)
  (set-face-attribute 'hl-line nil :extend t))

(use-package doom-themes
  :config
  ;;(doom-themes-treemacs-config)
  ;;(doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (set-face-attribute 'doom-visual-bell nil
                      :inherit 'mode-line
                      :background (face-foreground 'error)
                      :inverse-video nil)
  (setq doom-themes-enable-italic
        doom-themes-enable-bold))

(use-package solaire-mode
  :functions persp-load-state-from-file
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer)
         (after-load-theme . solaire-mode-swap-bg))
  :config
  (setq solaire-mode-remap-fringe nil)
  (solaire-global-mode 1)
  (solaire-mode-swap-bg)
  (advice-add #'persp-load-state-from-file
              :after #'solaire-mode-restore-persp-mode-buffers))

(use-package hide-mode-line
  :hook (((completion-list-mode completion-in-region-mode) . hide-mode-line-mode)))

(use-package all-the-icons)
(use-package flx)
(use-package eldoc :hook (emacs-lisp-mode . eldoc-mode))
(use-package use-package-ensure-system-package
  :commands (use-package-ensure-system-package-exists?))

(use-package eyebrowse
  :commands eyebrowse-mode
  :config (eyebrowse-mode t))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :custom (aggressive-indent-comments-too))

(use-package hungry-delete
  :defer 1
  :config (hungry-delete-mode 1))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package undo-tree
  :commands global-undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package counsel
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("C-w" . ivy-yank-word)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-switch-buffer-kill)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap dired] . counsel-dired)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-d" . counsel-dired-jump)
         ("C-x C-l" . counsel-find-library)
         ("C-x C-r" . counsel-buffer-or-recentf)
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
         ("C-h v" . counsel-describe-variable)

         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("s-f" . swiper)
         ("C-S-s" . swiper-all)

         :map swiper-map
         ("M-%" . swiper-query-replace)
         ("M-s" . swiper-isearch-toggle)
         :map isearch-mode-map
         ("M-s" . swiper-isearch-toggle))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq enable-recursive-minibuffers t
        ivy-dynamic-exhibit-delay-ms 250
        ivy-use-selectable-prompt t
        ivy-height 10
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil
        ivy-case-fold-search-default t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' %s")))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

(use-package amx
  :init (setq amx-history-length 20)
  :hook (after-init . amx-mode))

(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
              ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic)))

(use-package flyspell
  :hook (((markdown-mode org-mode text-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_US")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcom-message nil))

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
  :custom (abbrev-file-name "~/.cache/emacs/emacs_abbrev")
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package git-commit
  :after magit
  :hook (git-commit-mode . my/git-commit-auto-fill-everywhere)
  :custom (git-commit-summary-max-length 50)
  :preface
  (defun my/git-commmit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil)))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup)))

(use-package git-gutter
  :defer 0.3
  :init (global-git-gutter-mode 1))

(use-package gitconfig-mode
  :defer t
  :hook (gitconfig-mode . (lambda () (setf indent-tabs-mode nil tab-width 4))))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  (setq dired-use-ls-dired nil)
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  (use-package dired-rsync
    :bind (:map dired-mode-map
                ("C-c C-r" . dired-rsync)))
  (use-package diredfl
    :init (diredfl-global-mode 1))
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand))

(when (executable-find "fd")
  (use-package fd-dired))

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

(use-package julia-mode)
(use-package julia-repl
  :hook (julia-mode . julia-repl-mode))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package flycheck
  :config (global-flycheck-mode))

(provide 'init)
;;; init.el ends here
