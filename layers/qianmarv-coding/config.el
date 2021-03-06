;;; config.el --- qianmarv Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2017 qianmarv
;;
;; Author: qianmarv <qianmarv@gmail.com>
;; URL: https://github.com/qianmarv/
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;------------------------------------------------------------------------------
;; Racket / mit-scheme
;;------------------------------------------------------------------------------

(when (string-equal system-type "windows-nt")
  (progn
    (setq racket-program "C:/Program Files/Racket/Racket.exe")
    (setq org-babel-racket-command "\"C:/Program Files/Racket/Racket.exe\"")
    (setq geiser-mit-binary "C:/Program Files (x86)/MIT-GNU Scheme/bin/mit-scheme.exe --library C:/Program Files (x86)/MIT-GNU Scheme/lib")
    ))

;; (setq yas-snippet-dirs (append yas-snippet-dirs
;;                               '("~/.spacemacs.d/snippets")))


