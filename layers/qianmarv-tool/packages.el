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
    (excorporate
     :location (recipe :fetcher github :repo "qianmarv/excorporate"))
    ))

;;------------------------------------------------------------------------------
;; google-translate
;; https://github.com/atykhonov/google-translate
;;------------------------------------------------------------------------------
(defun qianmarv-tool/post-init-google-translate()
  (with-eval-after-load 'google-translate
    (progn
      (setq google-translate-default-target-language "zh-CN")
      ))
  )

;;------------------------------------------------------------------------------
;; excorporate
;; https://www.emacswiki.org/emacs/MsOutlook
;;------------------------------------------------------------------------------
(defun qianmarv-tool/init-excorporate ()
  (use-package excorporate
    :defer t
    :init
    (spacemacs/declare-prefix  "ae" "excorporate")
    (spacemacs/set-leader-keys "aec" 'qianmarv-tool/exco-connect)
    (spacemacs/set-leader-keys "ae." 'qianmarv-tool/exco-show-today)
    :config
    (progn
      (setq excorporate-configuration "marvin.qian@sap.com")
      ))
  )

