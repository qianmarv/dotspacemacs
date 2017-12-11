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

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `sap-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sap/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sap/pre-init-PACKAGE' and/or
;;   `sap/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

;; (defconst sap-packages
;;   '(
;;     (abap-mode :location local)
;;     (sapwiki :location local)
;;     (sapjira :location local)
;;     )
  "The list of Lisp packages required by the sap layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format"


;;; packages.el ends here


(defconst qianmarv-sap-packages
      '(
        (abap-mode :location local)
        (sapwiki :location local)
        (sapjira :location local)
        ))

(defun qianmarv-sap/init-sapwiki()
  (use-package sapwiki
    :init
    )
  )

(defun qianmarv-sap/init-abap-mode ()
  (use-package abap-mode
    :init
    )
  )

(defun qianmarv-sap/init-sapjira()
  (use-package sapjira
    :init
    )
  )

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
