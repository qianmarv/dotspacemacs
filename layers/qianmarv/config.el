;;; config.el --- qianmarv Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2017 qianmarv
;;
;; Author: qianmarv <qianmarv@gmail.com>
;; URL: https://github.com/qianmarv/
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;; User Information
(setq user-full-name "Marvin Qian")
(setq user-mail-address "qianmarv@gmail.com")

(setq auth-sources
      '((:source "~/Emacs/Personal/.authinfo.gpg")))

(setq request-curl-options '("--insecure" ))

;; Setup Font
;; (setq fonts
;;       (cond ((eq system-type 'darwin)     '("Monaco"    "STHeiti"))
;;             ((eq system-type 'gnu/linux)  '("Menlo"     "WenQuanYi Zen Hei"))
;;             ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei"))))
;; (set-face-attribute 'default nil :font
;;                     (format "%s:pixelsize=%d" (car fonts) 14))
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font) charset
;;                     (font-spec :family (car (cdr fonts)))))
;; Fix chinese font width and rescale
;; (setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2) ("STHeiti". 1.2)))


;; Set Default Path
(setq default-directory (getenv "HOME"))

;; Display Time
(display-time-mode t)

;; Set Time Display Format As 24H
(setq display-time-24hr-format t)


;; (setq request-log-level 'debug)
 ;; (setq request-message-level 'debug)

;; (setq request-log-level -1)
;; (setq request-message-level -1)
