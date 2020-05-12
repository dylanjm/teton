;;; subtitle-capture.el --- Fetching subtitles from mpv player -*- lexical-binding: t-*-
;;; Commentary:
;;;
;;; Code:
(require 'emms)
(require 'dash)
(require 's)

(defgroup subtitle-capture nil
  "Capturing subtitle text from EMMS Video"
  :prefix "subcap-"
  :group 'multimedia)


(defun subcap--fetch-srt-file ()
  "Return subtitle file path from current playing .mp4 file."
  (let* ((track (emms-playlist-current-selected-track))
          (file (emms-track-name track))
          (srt-file (s-replace ".mp4" ".srt" file)))
    (unless (file-exists-p srt-file)
      (error "File does not exist: %s" srt-file))
    srt-file))

(defun subcap--read-srt-file (file)
  "Read subtitles FILE into temp buffer and break into sublists of three."
  (-partition-all 3
    (split-string (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-substring-no-properties (point-min) (point-max)))
      "\n" t)))

(defun subcap--set-timestamp-datum (sublist)
  "Replace each SUBLIST with a lower and upper bound list."
  (let* ((time-string (car (cdr sublist)))
          (ts-split (s-split " --> " time-string))
          (ts-list (mapcar 'subcap--convert-ts-to-seconds ts-split)))
    (setf time-string ts-list)))

(defun subcap--convert-ts-to-pair (sublist)
  "Convert the SUBLIST string '00:00:00,000 --> 00:00:00,000' to (0.0 0.0)."
  (let* ((ts (car (cdr sublist)))
          (tslist (s-split " --> " ts)))
    (mapcar 'subcap--convert-ts-to-seconds tslist)))

(defun subcap--convert-ts-to-seconds (timestamp)
  "Apply function to each portion of the TIMESTAMP."
  (let* ((time-list (mapcar 'string-to-number (s-split "[:|,]" timestamp))))
    (reduce '+ (-zip-with (lambda (f x) (funcall f x))
                 '((lambda (x) (* x 3600.0))
                    (lambda (x) (* x 60.0))
                    (lambda (x) x)
                    (lambda (x) (/ (float x) 1000.0)))
                 time-list))))

(defun subcap--get-srt-alist ()
  "Return alist containing relevant information for current video."
  (let* ((file (emms-subtitles-fetch-srt-file))
          (dalist (read-subtitle-file-into-alist file)))
    (mapc 'subcap--set-timestamp-datum dalist)
    dalist))

(defun subcap--check-invertal (moe x elem)
  "Compare X to the list ELEM MOE."
  (let* ((bounds (car (cdr elem)))
          (lower (-first-item bounds))
          (upper (-last-item bounds)))
    (if moe
      (subcap--in-interval-margin-error-p lower upper x)
      (subcap--in-interval-p lower upper x))))

  (defun subcap--in-interval-p (lower upper x)
    "Return t if LOWER <= X < UPPER."
    (and (<= lower x) (> upper x)))

  (defun subcap--in-interval-margin-error-p (lower upper x)
    "LOWER UPPER X."
    (-any-p (-partial 'in-interval-p lower upper)
      `(,(* 0.70 x) ,x ,(* 1.30 x))))

  (defun subcap--get-timestamp-indices (ts sta)
    "TS STA."
    (-find-indices (-partial 'subcap--check-invertal t ts) sta))

  (defun subcap--get-timestamp-text (idxs sta)
    "IDXS STA."
    (let* ((numlist (-map (lambda (x)
                            (funcall
                              (-compose 'number-to-string (-partial '+ 1))
                              x))
                      idxs))
            (text (-map (lambda (x)
                          (-last-item
                            (assoc x sta)))
                    numlist)))
      (mapconcat 'identity text " ")))














  (provide 'emms-subtitles)
;;; emms-subtitles.el ends here
