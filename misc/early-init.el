;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq gc-cons-threshold (if (display-graphic-p) 400000000 100000000))
(setq package-enable-at-startup nil)

(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines .0 ) default-frame-alist))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)
(advice-add #'x-apply-session-resources :override #'ignore)
;;; early-init.el ends here
