(defadvice dired-copy-filename-as-kill (after dired-filename-to-clipboard activate)
  (with-temp-buffer
    (insert (current-kill 0))
    (shell-command-on-region (point-min) (point-max)
                             (cond
                              ((eq system-type 'cygwin) "putclip")
                              ((eq system-type 'darwin) "pbcopy")
                              (t "xsel -ib")
                              )))
  (message "%s => clipboard" (current-kill 0)))

(defun djm/remove-dir-and-ext-from-path (string)
  (file-name-nondirectory (file-name-sans-extension string)))

(defun djm/replace-underscore-as-space (string)
  (replace-regexp-in-string "_" " " string))

(defun djm/insert-markdown-link (&optional string)
  (interactive)
  (let (input-str output-str)
    (cond
     (string
      (setq input-str from to))

     ((use-region-p)
      (setq from (region-beginning))
      (setq to (region-end))
      (setq input-str (buffer-substring-no-properties from to)))

     (t
      (setq from (point-at-bol))
      (setq to (point-at-eol))
      (setq input-str (buffer-substring-no-properties from to))))

    (setq output-str
          (progn
            (let* ((path input-str)
                   (file (djm/remove-dir-and-ext-from-path path))
                   (title (djm/replace-underscore-as-space file)))
              (concat "[" title "]" "(" input-str ")"))))

    (if string
        output-str
      (save-excursion
        (delete-region from to)
        (goto-char from)
        (insert output-str)))))

(defun djm/insert-markdown-link-at-region ()
  (interactive)
  (save-excursion
    (goto-char (region-end))
    (let ((end-marker (copy-marker (point-marker)))
          next-line-marker)
      (goto-char (region-beginning))
      (if (not (bolp))
          (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
        (let ((start nil)
              (end nil))
          (goto-char next-line-marker)
          (save-excursion
            (setq start (point))
            (forward-line 1)
            (set-marker next-line-marker (point))
            (setq end (point)))
          (save-excursion
            (let ((mark-active nil))
              (narrow-to-region start end)
              (djm/insert-markdown-link)
              (widen)))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))))
