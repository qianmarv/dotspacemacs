;;; packages.el --- sap layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Marvin Qian <qianmarv@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

;;; packages.el ends here

;;; Seems that quelpa not working well on windows!!
;;; Had to run from MINGW at first run in order to install from github
(defconst qianmarv-sap-packages
  '(
    (abap-mode
     :location (recipe :fetcher github :repo "qianmarv/sap-abap-mode"))

    (abap
     :location (recipe :fetcher github :repo "qianmarv/ABAPInEmacs"))

    (sapwiki :location local)
    ;; (sapjira :location local)

    ))

(defun qianmarv-sap/init-sapwiki()
  (use-package sapwiki
    :init
    ))

(defun qianmarv-sap/init-abap-mode ()
  (use-package abap-mode
    :mode ("\\.abap\\'" . abap-mode)
    :init
        ;;    (add-to-list 'auto-mode-alist '("\\.abap\\'" . abap-mode))
    ))

(defun qianmarv-sap/init-abap ()
  (use-package abap
    :defer t
    :after (request)
    :init
    (progn
       (spacemacs/declare-prefix  "aa" "ABAP Development")
       (spacemacs/set-leader-keys "aac" 'abap-create-project)
       (spacemacs/set-leader-keys "aaa" 'abap-add-project)
       (spacemacs/set-leader-keys "aar" 'abap-remove-project)
       (spacemacs/set-leader-keys "aao" 'abap-open-project)
       (spacemacs/set-leader-keys "aas" 'abap-add-server)
       (spacemacs/set-leader-keys "aaf" 'abap-search-object)
       (spacemacs/set-leader-keys "aal" 'abap-login)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "p" 'abap-open-project)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "f" 'abap-search-object)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "r" 'abap-retrieve-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "c" 'abap-check-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "r" 'abap-retrieve-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "s" 'abap-submit-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "a" 'abap-activate-source)
       (spacemacs/set-leader-keys-for-major-mode 'abap-mode "=" 'abap-format-source)
       (global-set-key (kbd "<C-return>") 'abap-code-completion)
       )))


;; (defun qianmarv-sap/init-sapjira()
;;   (use-package sapjira
;;     :init
;;     )
;;   )

(setq dk-sapwiki-user "I074218")
(setq org-publish-project-alist
      '(("sapwiki"
         :base-directory "C:/SAP/sapwiki/work/"
         :base-extension "org"
         :publishing-directory "C:/SAP/sapwiki/pub/"
         :recursive t
         :publishing-function dk-html-publish-to-sapwiki
         :headline-levels 4
         :section-number t
         :auto-preamble nil
         :auto-sitemap nil
         :html-doctype "xhtml5"
         :html-html5-fancy t
         :html-indent t
         :html-preamble nil
         :html-postamble nil
         :html-table-header-tags ("<th>" . "</th>")
         :html-table-data-tags ("<td>" . "</td>")
         :with-author nil
         :with-date nil
         :with-title nil
         :with-footnotes t )))
