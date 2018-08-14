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
    cnfonts
    nov
    (excorporate
     :location (recipe :fetcher github :repo "qianmarv/excorporate"))
    ))

;;------------------------------------------------------------------------------
;; ebookreader
;; https://github.com/bddean/emacs-nov
;;------------------------------------------------------------------------------
(defun qianmarv-tool/init-nov()
  (use-package nov)
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )

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

;;------------------------------------------------------------------------------
;; cnfonts
;; https://github.com/tumashu/cnfonts
;;------------------------------------------------------------------------------
(defun qianmarv-tool/init-cnfonts()
  (use-package cnfonts
    :init
    :config
    (progn
      (cnfonts-enable)
      (cnfonts-set-spacemacs-fallback-fonts)
      (setq cnfonts-profiles
            '("program" "org-mode" "read-book"))
      (setq cnfonts-use-face-font-rescale t)
      ))
  )


