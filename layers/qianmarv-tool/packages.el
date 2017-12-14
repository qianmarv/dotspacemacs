;;; packages.el --- qianmarv-tool layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Marvin Qian <qianmarv@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defconst qianmarv-tool-packages
  '(
    google-translate
    ))

;;------------------------------------------------------------------------------
;; google-translate
;; https://github.com/atykhonov/google-translate
;;------------------------------------------------------------------------------
(defun qianmarv-coding/init-google-translate()
  (use-package google-translate)
  (with-eval-after-load google-translate
    :config
    (progn
      (setq google-translate-default-target-language "zh-CN")
      ))
  )

