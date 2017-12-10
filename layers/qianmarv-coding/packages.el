;;; packages.el --- qianmarv Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2017 qianmarv
;;
;; Author: qianmarv <qianmarv@gmail.com>
;; URL: https://github.com/qianmarv/
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.

(setq qianmarv-coding-packages
      '(
        css-mode
        ;; restclient  ;; https://github.com/pashky/restclient.el
        ))

;;------------------------------------------------------------------------------
;; CSS Mode
;;------------------------------------------------------------------------------
(defun qianmarv-coding/post-init-css-mode()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))
    ))
