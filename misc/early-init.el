;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

(advice-add #'x-apply-session-resources :override #'ignore)
;;; early-init.el ends here
