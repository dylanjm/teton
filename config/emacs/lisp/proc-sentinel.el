;;; proc-sentinel.el --- dylanjm lisp -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code found here: https://emacs.stackexchange.com/questions/48306/how-to-automatically-kill-a-shell-buffer-when-the-shell-process-exits

;;; Code:

(defun add-process-sentinel (sentinel &optional process)
  "Add SENTINEL to PROCESS.
PROCESS defaults to the process of the current buffer.
Use this function with care.
If there is already a process sentinel SENTINEL is used as after-advice.
That can fail if the process sentinel is reset by some other function."
  (unless process
    (setq process (get-buffer-process (current-buffer))))
  (let ((old (process-sentinel process)))
    (cond
     ((symbolp old)
      (advice-add old :after sentinel))
     ((null old)
      (set-process-sentinel process sentinel))
     (t (warn "Cannot set sentinel %S for process %S." sentinel process)))))

(defun arco/ielm-mode-hook ()
  (add-process-sentinel (lambda (process signal)
                          (and (memq (process-status process) '(exit signal))
                               (buffer-live-p (process-buffer process))
                               (kill-buffer (process-buffer process))))))

(add-hook 'ielm-mode-hook 'arco/ielm-mode-hook)
(arco/ielm-mode-hook hs-minor-mode)

(provide 'proc-sentinel)
;;; proc-sentinel.el ends here
