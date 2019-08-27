(let ((gc-cons-threshold most-positive-fixnum))

  (defconst user-emacs-modules-directory
    (expand-file-name (concat user-emacs-directory "lisp/"))
    "Directory for storing modules.")

  ;; Set repositories
  (require 'package)
  (setq-default
   load-prefer-newer t
   package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t )
  (package-initialize)

  ;; Install dependencies
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package t))
  (require 'use-package)
  (setq-default
   use-package-always-defer t
   use-package-always-ensure t
   use-package-vervose t)

  ;; Use latest Org
  (use-package org :ensure org-plus-contrib)
  (require 'org-tempo)

  ;; Useful Modules
  (add-to-list 'load-path user-emacs-modules-directory)
  (require 'init-modeline)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
  (garbage-collect))

