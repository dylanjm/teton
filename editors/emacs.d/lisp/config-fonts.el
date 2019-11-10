;;; config-fonts.el --- setting up unicode & emojis
;;; Commentary:
;;; We use these fonts:
;;;
;;; - Monoid (http://larsenwork.com/monoid/) as default
;;; - XITS Math (https://github.com/khaledhosny/xits-math) as fallback for math
;;;
;;; Source Code Pro (https://github.com/adobe-fonts/source-code-pro) is a good
;;; monospace font, too.  An alternative is Consolas.  Another great monospace
;;; font is and Pragmata Pro (http://www.fsd.it/fonts/pragmatapro.htm,
;;; proprietary, around 200$).
;;;
;;; Currently this setup only works for OS X, as we rely on Apple's Emoji and
;;; Symbol fonts.
;;;
;;; TODO:  Find Emoji and symbol fonts for Linux and Windows
;;; TODO: need a better fallback option if these custom fonts are not available

;;; Code:
(require 'cl-lib)

;; Font setup
(defun lunaryorn-configure-fonts (frame)
  "Set up fonts for FRAME.
Set the default font, and configure various overrides for
symbols, emojis, greek letters, as well as fall backs for."
  ;; Additional fonts for special characters and fallbacks
  ;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ
  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))

  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Arial Unicode MS")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Menlo")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
                      frame 'prepend))

  (when (eq system-type 'darwin)
    ;; Colored Emoji on OS X, prefer over everything else!
    (set-fontset-font t nil (font-spec :family "Apple Color Emoji")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Apple Symbols")
                    frame 'append))

(provide 'config-fonts)
;;; config-fonts.el ends here
