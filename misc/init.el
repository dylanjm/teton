;;; init.el --- Emacs main configuration file -*-
;;;
;;; Commentary:
;;; Emacs config by dylanjm
;;;
;;; Code:

;; Font setup
;; (defun lunaryorn-configure-fonts (frame)
;;   "Set up fonts for FRAME.
;; Set the default font, and configure various overrides for
;; symbols, emojis, greek letters, as well as fall backs for."
;;   ;; Additional fonts for special characters and fallbacks
;;   ;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ
;;   (dolist (script '(symbol mathematical))
;;     (set-fontset-font t script (font-spec :family "XITS Math")
;;                       frame 'prepend))

;;   ;; Define a font set stack for symbols, greek and math characters
;;   (dolist (script '(symbol greek mathematical))
;;     (set-fontset-font t script (font-spec :family "Arial Unicode MS")
;;                       frame 'prepend)
;;     (set-fontset-font t script (font-spec :family "Menlo")
;;                       frame 'prepend)
;;     (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
;;                       frame 'prepend))

;;   (when (eq system-type 'darwin)
;;     ;; Colored Emoji on OS X, prefer over everything else!
;;     (set-fontset-font t nil (font-spec :family "Apple Color Emoji")
;;                       frame 'prepend))

;;   ;; Fallbacks for math and generic symbols
;;   (set-fontset-font t nil (font-spec :family "Apple Symbols")
;;                     frame 'append))

;; (when-let (frame (selected-frame))
;;   (lunaryorn-configure-fonts frame))
;; (add-hook 'after-make-frame-functions #'lunaryorn-configure-fonts)

(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(global-display-line-numbers-mode)
(blink-cursor-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)

;; (when (fboundp 'set-charset-priority)
;;   (set-charset-priority 'unicode))
;; (prefer-coding-system 'utf-8-unix)
;; (set-language-environment 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-clipboard-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (modify-coding-system-alist 'process "*" 'utf-8)
;; (set-file-name-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8
;;       default-process-coding-system '(utf-8 . utf-8))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-verbose t
        use-package-always-defer t
        use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(use-package files
  :ensure nil
  :custom
  (auto-save-file-name-transforms '((".*" "~/.cache/emacs/emacs-backups/" t)))
  (backup-directory-alist '(("." . "~/.cache/emacs/emacs-backups/"))))


(use-package cus-start
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :custom ((ad-redefinition-action 'accept)
           (auto-window-vscroll nil)
           (auto-save-list-file-prefix nil)
           (backup-by-copying t)
           (create-lockfiles nil)
           (cursor-in-non-selected-windows nil)
           (display-time-default-load-average nil)
           (debug-on-error t)
           (fill-column 80)
           (frame-title-format '("%b - Emacs"))
           (icon-title-format frame-title-format)
           (indent-tabs-mode nil)
           (inhibit-compacting-font-caches t)
           (load-prefer-newer t)
           (mode-line-in-non-selected-windows nil)
           (ns-use-thin-smoothing t)
           (ring-bell-function #'ignore)
           (debug-on-error t)
           (help-window-select t)
           (inhibit-startup-screen t)
           (inhibit-startup-echo-area-message t)
           (inhibit-default-init t)
           (initial-scratch-message "")
           (sentence-end-double-space nil)
           (tab-width 4)
           (vc-follow-symlinks t)
           (view-read-only t)
           (use-file-dialog nil)
           (use-dialog-box nil)
           (scroll-conservatively most-positive-fixnum)
           (scroll-margin 2)
           (scroll-step 1)
           (select-enable-clipboard t)
           (show-trailing-whitespace t)
           (uniquify-buffer-name-style 'forward)))

;; (use-package frame
;;   :ensure nil
;;   :custom ((window-divider-default-places t)
;;            (window-divider-default-bottom-width 1)
;;            (window-divider-default-right-width 1))
;;   :init (global-unset-key (kbd "C-z"))
;;   :config (window-divider-mode 1))

;; (use-package bindings
;;   :ensure nil
;;   :custom ((mode-line-percent-position t)))

;; (use-package simple
;;   :ensure nil
;;   :custom ((column-number-mode nil)
;;            (line-number-mode nil)
;;            (line-move-visual nil)
;;            (track-eol t)
;;            (set-mark-command-repeat-pop t)))

(use-package saveplace
  :ensure nil
  :custom (save-place-file "~/.cache/emacs/emacs-places")
  :hook (after-init . save-place-mode))

;; (use-package savehist
;;   :ensure nil
;;   :hook (after-init . savehist-mode)
;;   :custom ((history-delete-duplicates t)
;; 	       (history-length t)
;; 	       (savehist-additional-variables '(mark-ring
;; 					                        global-mark-ring
;; 					                        search-ring
;; 					                        extended-command-history
;; 					                        kill-ring))
;; 	       (savehist-autosave-intervall 300)
;; 	       (savehist-file (expand-file-name "~/.cache/emacs/emacs-history"))
;; 	       (savehist-save-minibuffer-history 1)
;; 	       (history-length 1000)))

(use-package ns-win
  :ensure nil
  :if (eq system-type 'darwin)
  :custom ((ns-pop-up-frames nil)
	       (ns-use-native-fullscreen nil)
	       (mac-option-modifier 'meta)
	       (mac-command-modifier 'meta)
	       (mac-right-command-modifier 'left)
	       (mac-right-option-modifier 'none)
	       (mac-function-modifier 'hyper)))

;; (use-package prog-mode
;;   :ensure nil
;;   :hook ((prog-mode . show-paren-mode)
;; 	     (prog-mode . prettify-symbols-mode))
;;   :custom (prettify-symbols-unprettify-at-point 'right-edge))

(use-package recentf
  :ensure nil
  :functions (recentf-save-list)
  :hook (after-init . recentf-mode)
  :init (setq recentf-save-file "~/.cache/emacs/emacs-recentf"
              recentf-max-saved-items 200
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks" "NEWS"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
                "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (run-at-time nil (* 5 60) (lambda () (let ((save-silently t)) (recentf-save-list)))))

;; (use-package exec-path-from-shell
;;   :demand t
;;   :custom ((exec-path-from-shell-check-startup-files nil)
;; 	       (exec-path-from-shell-variables '("PATH" "MANPATH"))
;; 	       (exec-path-from-shell-arguments '("-l")))
;;   :config (exec-path-from-shell-initialize))

;; (use-package doom-themes
;;   :demand t
;;   :config
;;   (doom-themes-org-config)
;;   (load-theme 'doom-nord-light t)
;;   (setq doom-themes-enable-italic t)
;;   (setq doom-themes-enable-bold t)
;;   (set-face-attribute 'hl-line nil :extend t)
;;   (set-face-attribute 'region nil :extend t)
;;   (set-face-attribute 'secondary-selection nil :extend t)
;;   (set-face-attribute 'mode-line nil :foreground (doom-color 'red))
;;   (set-face-attribute 'font-lock-comment-face nil :font "-*-Iosevka Slab-bold-italic-normal-*-18-*-*-*-m-0-iso10646-1"))

;; ;; (use-package solaire-mode
;; ;;   :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
;; ;;          (minibuffer-setup . solaire-mode-in-minibuffer))
;; ;;   :config (solaire-global-mode 1))

;; (use-package minions
;;   :demand t
;;   :commands (minions-mode)
;;   :config
;;   (setq minions-mode-line-lighter "...")
;;   (setq minions-mode-line-delimiters '("" . ""))
;;   (minions-mode 1))

;; ;(use-package all-the-icons)
;; ;(use-package diffview)
;; ;(use-package focus)
;; ;(use-package flx)
;; ;(use-package eldoc :ensure nil :hook (emacs-lisp-mode . eldoc-mode))
;; (use-package use-package-ensure-system-package
;;   :commands (use-package-ensure-system-package-exists?))

;; (use-package eyebrowse
;;   :commands (eyebrowse-mode)
;;   :config (eyebrowse-mode 1))

;; (use-package aggressive-indent
;;   :hook (prog-mode . aggressive-indent-mode)
;;   :custom (aggressive-indent-comments-too))

;; (use-package hungry-delete
;;   :init (global-hungry-delete-mode 1))

;; (use-package company
;;   :demand t
;;   :commands global-company-mode
;;   :bind (:map company-active-map
;;               ("RET" . nil)
;;               ([return] . nil)
;;               ("TAB" . company-complete-selection)
;;               ([tab] . company-complete-selection)
;;               ("C-f" . company-complete-common)
;;               ("C-n" . company-select-next)
;;               ("C-p" . company-select-previous))
;;   :custom
;;   (company-require-match 'never)
;;   (company-async-timeout 5)
;;   (company-idle-delay 0)
;;   (company-minimum-prefix-length 2)
;;   (company-tooltip-align-annotations t)
;;   :config
;;   (use-package company-statistics
;;     :custom (company-statistics-file "~/.cache/emacs/company-statistics-cache.el")
;;     :config (company-statistics-mode))

;;   (use-package company-flx
;;     :config (company-flx-mode 1))

;;   (global-company-mode 1))

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package undo-tree
;;   :hook (after-init . global-undo-tree-mode))

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
         :map ivy-switch-buffer-map
         ("C-x k" . ivy-switch-buffer-kill)

         :map counsel-mode-map
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
         ("C-h v" . counsel-describe-variable)

         ("C-s" . swiper)
         ("C-c c s" . swiper-isearch)
         ("C-c c r" . swiper-isearch-backward)
         ("C-S-s" . swiper-all)
         :map swiper-map
         ("M-%" . swiper-query-replace)
         ("M-s" . swiper-isearch-toggle)
         :map isearch-mode-map
         ("M-s" . swiper-isearch-toggle))

  :custom ((enable-recursive-minibuffers t)
           (ivy-dynamic-exhibit-delay-ms 250)
           (ivy-use-selectable-prompt t)
           (ivy-height 10)
           (ivy-initial-inputs-alist nil)
           (ivy-case-fold-search-default t)
           (ivy-use-virtual-buffers t)
           (ivy-count-format "(%d/%d) "))

  :config
  (with-eval-after-load 'ivy
    (push (cons #'swiper (cdr (assq t ivy-re-builders-alist)))
          ivy-re-builders-alist)
    (push (cons #'swiper-isearch (cdr (assq t ivy-re-builders-alist)))
          ivy-re-builders-alist)
    (push (cons t #'ivy--regex-fuzzy) ivy-re-builders-alist))

  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' %s"))

  (use-package amx
    :custom (amx-save-file "~/.cache/emacs/amx-items")
    :hook (counsel-mode . amx-mode)))

;; (use-package posframe)
;; (use-package ivy-posframe
;;   :demand t
;;   :functions (ivy-posframe-display-at-window-bottom-left
;;               ivy-posframe-display-at-frame-center)
;;   :config
;;   (push (cons #'swiper nil)
;;         ivy-posframe-display-functions-alist)
;;   (push (cons #'counsel-M-x #'ivy-posframe-display-at-window-bottom-left)
;;         ivy-posframe-display-functions-alist)
;;   (push (cons t #'ivy-posframe-display-at-frame-center)
;;         ivy-posframe-display-functions-alist)
;;   (ivy-posframe-enable))

;; (use-package counsel-projectile
;;   :after (counsel projectile)
;;   :config (counsel-projectile-mode 1))

;; (use-package prescient)
;; (use-package ivy-prescient)
;; (use-package company-prescient)

;; ;; (use-package ispell
;; ;;   :ensure nil
;; ;;   :ensure-system-package (hunspell . "trizen -S hunspell")
;; ;;   :custom
;; ;;   (ispell-dictionary "en_US")
;; ;;   (ispell-program-name (executable-find "hunspell"))
;; ;;   (ispell-really-hunspell t)
;; ;;   (ispell-silently-savep t))

;; (use-package magit
;;   :bind (("C-x g" . magit-status)
;;          ("C-x M-g" . magit-dispatch)
;;          ("C-c M-g" . magit-file-popup))
;;   :config
;;   (use-package git-commit
;;     :defines (djm/git-commit-style)
;;     :preface
;;     (defun djm/git-commit-style ()
;;       (setq fill-column 72)
;;       (setq-local comment-auto-fill-only-comments nil))
;;     :hook (git-commit-mode . djm/git-commit-style)
;;     :custom (git-commit-summary-max-length 50))

;;   (use-package git-gutter
;;     :config (global-git-gutter-mode 1)))

;; (use-package ace-window
;;   :bind (("C-x o" . ace-window)))

;; (use-package dired
;;   :demand t
;;   :ensure nil
;;   :functions (wdired-change-to-wdired-mode
;;               dired-get-file-for-visit
;;               aw-switch-to-window
;;               ace-window
;;               aw-window-list
;;               aw-select)
;;   :defines (djm/dired-choose-window
;;             dired-window)
;;   :bind (:map dired-mode-map
;;               ("C-c C-p" . wdired-change-to-wdired-mode)
;;               ("C-c C-o" . djm/dired-choose-window))
;;   :custom  ((dired-recursive-deletes 'always)
;;             (dired-recursive-copies 'always)
;;             (dired-use-ls-dired nil))
;;   :config
;;   (defun dired-window ()
;;     (window-at (frame-width) 1))
;;   (defun djm/dired-choose-window ()
;;     "Use ace window to select a window for opening a file from dired.
;; https://stackoverflow.com/questions/15441961/opening-a-file-from-dired-in-particular-window"
;;     (interactive)
;;     (let ((file (dired-get-file-for-visit)))
;;       (if (> (length (aw-window-list)) 1)
;;           (aw-select "" (lambda (window)
;;                           (aw-switch-to-window window)
;;                           (find-file file)))
;;         (find-file-other-window file))))

;;   (when (executable-find "fd")  (use-package fd-dired))
;;   (when (executable-find "gls") (setq insert-directory-program "gls"))

;;   (use-package dired-rsync
;;     :bind (:map dired-mode-map
;;                 ("C-c C-r" . dired-rsync)))

;;   (use-package diredfl
;;     :config (diredfl-global-mode 1))

;;   (use-package dired-aux
;;     :ensure nil)

;;   (use-package dired-x
;;     :ensure nil)

;;   (use-package dired-subtree
;;     :demand t
;;     :bind (:map dired-mode-map
;;                 ("TAB" . dired-subtree-insert)
;;                 (";" . dired-subtree-remove)))

;;   (use-package dired-git-info
;;     :demand t
;;     :bind (:map dired-mode-map
;;                 (")" . dired-git-info-mode))))

;; (use-package projectile
;;   :custom
;;   (projectile-cache-file "~/.cache/emacs/projectile.cache")
;;   (projectile-completion-system 'ivy)
;;   (projectile-enable-caching t)
;;   (projectile-known-projects-file "~/.cache/emacs/projectile-bookmarks.eld")
;;   :config
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode 1))

;; (use-package eterm-256color
;;   :hook (term-mode . eterm-256color-mode))

;; (use-package flycheck
;;   :hook (after-init . global-flycheck-mode)
;;   :config
;;   (setq flycheck-emacs-lisp-load-path 'inherit)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (setq flycheck-indication-mode 'right-fringe))

;; (use-package which-key
;;   :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
;;   :hook (after-init . which-key-mode))

;; (use-package yasnippet
;;   :demand t
;;   :hook (term-mode . (lambda () (yas-minor-mode -1)))
;;   :config
;;   (yas-global-mode 1))

(provide 'init)
;;; init.el ends here
