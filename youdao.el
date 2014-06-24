;; youdao.el a translate fontend for youdao translate.
;; Copyright (C) 2004 kvmaker@gmail.com
;; 
;; Version 0.1
;;
;; in your .emacs add 
;; (require 'youdao)
;; (setf keyfrom <keyfrom>)
;; (setf key     <key>)
;; (global-set-key (kbd "C-c C-v") 'translate)
(require 'json)

(defvar keyfrom "")
(defvar key "")
(defvar fmt "http://fanyi.youdao.com/openapi.do?keyfrom=%s&key=%s&type=data&doctype=json&version=1.1&q=%s")

(defun get-current-word ()
  "Get the word to translate."
  (save-excursion
    (when (not mark-active)
      (forward-word)
      (backward-word)
      (mark-word))
    (buffer-substring
     (region-beginning)
     (region-end))))

(defun get-json (url)
  (let ((buffer (url-retrieve-synchronously url))
        (json nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    json))

(defun get-and-parse-json (url)
  (let ((json-object-type 'plist))
    (json-read-from-string 
     (decode-coding-string (get-json url) 'utf-8))))

(defun translate0 (word)
  (let ((pstring (get-and-parse-json (format fmt
                                             keyfrom
                                             key
                                             word))))
    (if (equal pstring nil)
        nil
      (let ((explains    (plist-get (plist-get pstring :basic) :explains))
            (us-phonetic (plist-get (plist-get pstring :basic) :us-phonetic))
            (uk-phonetic (plist-get (plist-get pstring :basic) :uk-phonetic)))
        (cond ((equal explains nil) nil)
              ((or (equal us-phonetic nil) (equal uk-phonetic nil)) explains)
              (t (vconcat (vector (format "us: [%s]" us-phonetic)
                                  (format "uk: [%s]" uk-phonetic))
                          explains)))))))

(defun trans-format (word explains)
  (concat word 
          "\n"
          (if (equal explains nil)
              "N/A"
            (let ((res nil))
              (mapcar (lambda (exp)
                        (setq res (concat res (concat exp "\n"))))
                      explains)
              (substring res 0 -1)))))

(defun translate ()
  (interactive)
  (let ((word (get-current-word)))
    (let ((explains (translate0 word)))
      (popup-tip (trans-format word explains)))))

(provide 'youdao)
