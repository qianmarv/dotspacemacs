;;; funcs.el --- qianmarv Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2017 qianmarv
;;
;; Author: qianmarv <qianmarv@gmail.com>
;; URL: https://github.com/qianmarv/
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; Functions for Excorporate
(defun qianmarv-tool/exco-connect()
  (interactive)
  (excorporate)
  )

(defun qianmarv-tool/exco-show-today()
  (interactive)
  (let ((year (string-to-number (format-time-string "%Y" (current-time))))
        (month (string-to-number (format-time-string "%m" (current-time))))
        (day (string-to-number (format-time-string "%d" (current-time)))))
    (exco-org-show-day month day year)))
