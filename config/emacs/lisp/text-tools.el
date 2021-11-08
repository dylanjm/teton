;;; text-tools.el --- dylanjm text-tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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
  "Get the pure basename of STRING."
  (file-name-nondirectory (file-name-sans-extension string)))

(defun djm/replace-underscore-as-space (string)
  "Replace all underscores in STRING with space."
  (replace-regexp-in-string "_" " " string))

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

;; Custom Function to Handle converting Markdown Image links to MooseDown Image Links
;; Comes from https://emacs.stackexchange.com/questions/47804/how-do-i-create-a-key-binding-to-replace-specific-markdown-text/47806#47806
(defun markdown-to-dylanjm-image ()
  "Convert a Markdown image element to dylanjm's house markup format."
  (interactive "@*")
  ;; Look for a Markdown image element before the cursor or immediately
  ;; after the cursor. Don't look any further than the start of the current
  ;; paragraph.
  (let ((limit (save-excursion
                 (backward-paragraph)
                 (point)))
        (regexp "\\s-*!\\[\\([^][]*\\)\\](\\([^()]*\\))\\({[^}{]*}\\)?"))
    (if (and (eq ?! (char-after (1- (point))))
             (eq ?\[ (char-after (point))))
        (backward-char))
    (while (not (looking-at regexp))
      (search-backward "![" limit))
    ;; If the search didn't error out, then we exited the loop with
    ;; `looking-at' matching `regexp'. The match data therefore contains
    ;; the parts of the image link.
    (skip-syntax-forward "-")
    (let ((caption (subst-char-in-string ?\n ?\  (match-string 1)))
          (id (match-string 2))
          (style (and (match-string 3)
                      (substring (match-string 3) 1 -1))))
      ;; Remove the Markdown syntax.
      (delete-region (- (match-beginning 1) 2) (match-end 0))
      ;; Add line breaks before and after if it looks like there aren't any.
      (unless (eolp)
        (open-line 1))
      (unless (<= (point) (save-excursion
                            (back-to-indentation)
                            (point)))
        (insert "\n"))
      ;; Insert the new syntax.
      (insert "!media figures/" id ".png\n"
              "    id=" id "\n"
              "    caption=" caption)
      (when style
        (insert "\n    style=width:50%")))))


(defun djm/insert-markdown-link (&optional string)
  (interactive)
  (let (input-str output-str from to)
    (cond
      (string (setq input-str string))
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

(defun sum-numbers-in-region (start end)
  (interactive "r")
  (message "%s"
    (cl-reduce #'+
      (split-string (buffer-substring start
                      end))
      :key #'string-to-number)))

(defun arco/eval-to-kill-ring ()
  (interactive)
  (kill-new (with-output-to-string
              (princ (call-interactively 'eval-expression)))))


(defun emacs-screen-capture (arg &optional name dir format)
  (interactive "P")
  (let* ((format (completing-read "Screenshot Format: "
                   '(".png" ".pdf" ".jpg" ".tiff" ".svg")))
          (name (or name (read-string "Name of Screenshot: " nil)))
          (dir (or dir (read-directory-name (format "Save %s%s to: " name format))))
          (fp (concat dir name format))
          (abb-fp (concat "file:" (file-relative-name fp))))
    (set-process-sentinel
      (start-process-shell-command
        "imagecapture" nil (format "screencapture -i %s" fp))
      `(lambda (process msg)
         (when (memq (process-status process) '(exit signal))
           (message (concat (process-name process) " - " msg))
           (when (equal #',current-prefix-arg '(4))
             (org-insert-link nil ,abb-fp nil)))))))


(provide 'text-tools)
;;; text-tools.el ends here
