(let ((gc-cons-threshold most-positive-fixnum))
  ;; Set repositories
  (prog1 "leaf"
    (prog1 "install leaf"
      (custom-set-variables
       '(package-archives '(("melpa" . "https://melpa.org/packages/")
                            ("org" . "https://orgmode.org/elpa/")
                            ("gnu"   . "https://elpa.gnu.org/packages/"))))
      (unless (bound-and-true-p package--initialized)
        (package-initialize))
      (unless (package-installed-p 'leaf)
        (package-refresh-contents)
        (package-install 'leaf)))

    (leaf leaf-keywords
      :ensure t
      :config (leaf-keywords-init)))

  (leaf auto-compile
    :ensure t
    :config (auto-compile-on-load-mode))

  ;; Use latest Org
  (leaf org
    :ensure org-plus-contrib)

  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
  (garbage-collect))
