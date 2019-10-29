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
;; (provide 'font-config)
;;; font-config.el ends here
