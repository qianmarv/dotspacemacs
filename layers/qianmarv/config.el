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
