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

;; Automatic Create Daily Memo Template And Folders If Required!
(defun qianmarv-org/get-daily ()
    "Automatic create daily memo based on template while first call capture in the day \
the daily memo will be named as YYYYMMDD.org, and will be put into base-path/YYYY/MM \
if the folder is not exists, will create accordingly!"
    (let ((v-year-path (format "~/Emacs/Journal/%s" (format-time-string "%Y" (current-time))))
          (v-path (format "~/Emacs/Journal/%s" (format-time-string "%Y/%m" (current-time))))
          (v-filename (format "%s.org" (format-time-string "%Y%m%d" (current-time)))))
      (if (not (file-exists-p (format "%s" v-year-path))) (make-directory (format "%s" v-year-path)))
      (if (not (file-exists-p (format "%s" v-path))) (make-directory (format "%s" v-path)))
      (if (not (file-exists-p (format "%s/%s" v-path v-filename)))
          (save-excursion
            (copy-file "~/Emacs/Journal/template.org" (format "%s/%s" v-path v-filename))
            (set-buffer (find-file-noselect (qianmarv-org/get-daily)))
            (re-search-forward "<title>" (point-max) t 1)
            (replace-match (format "Daily Memo@%s" (format-time-string "%Y%m%d" (current-time))))))
      (format "%s/%s" v-path v-filename)))


;; Automatic Create Monthly Review File And Folders If Required!
(defun qianmarv-org/get-monthly ()
    "Automatic create daily memo based on template while first call capture in the month\
the montly memo will be named as YYYY-MM.org, and will be put into base-path/YYYY \
if the folder is not exists, will create accordingly!"
    (let ((v-path (format "~/Emacs/Journal/%s" (format-time-string "%Y" (current-time))))
          (v-filename (format "%s.org" (format-time-string "%Y-%m" (current-time)))))
      (if (not (file-exists-p (format "%s" v-path))) (make-directory (format "%s" v-path)))
      (if (not (file-exists-p (format "%s/%s" v-path v-filename)))
          (save-excursion
            (copy-file "~/Emacs/Journal/template.org" (format "%s/%s" v-path v-filename))
            (set-buffer (find-file-noselect (qianmarv-org/get-monthly)))
            (re-search-forward "<title>" (point-max) t 1)
            (replace-match (format "Monthly Plan & Review for %s" (format-time-string "%Y-%m" (current-time))))))
      (format "%s/%s" v-path v-filename)))

(defun qianmarv-org/find-h1 (text)
  (progn
   (goto-char (point-min))
   (unless (derived-mode-p 'org-mode)
     (error
      "Target buffer \"%s\" for file+headline should be in Org mode"
      (current-buffer)))
   (if (re-search-forward
        (format org-complex-heading-regexp-format text)
        nil t)
       (goto-char (point-at-bol))
     (progn
       (goto-char (point-max))
       (insert (format "* %s\n" text))
       ;; (goto-char (point-max))
       ;; (goto-char (point-at-bol))
       )
     ))
  )

(defun qianmarv-org/find-date-entry()
  (progn
    ;; (goto-char (point-min))
    (qianmarv-org/find-h1 "Daily Activity")
    (setq today (format-time-string "%Y-%m-%d" (current-time)))
    (unless (derived-mode-p 'org-mode)
      (error
       "Target buffer \"%s\" for file+headline should be in Org mode"
       (current-buffer)))
    (if (re-search-forward
         ;; (format org-complex-heading-regexp-format (regexp-quote today))
         (format org-complex-heading-regexp-format today)
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "** " today)
      (goto-char (point-at-bol))
      ;; (beginning-of-line 0))
    )
  )
)

(defun qianmarv-org/find-date-entry-notes()
  (progn
    (qianmarv-org/find-date-entry)
    (setq notes "Notes")
    (unless (derived-mode-p 'org-mode)
      (error
       "Target buffer \"%s\" for file+headline should be in Org mode"
       (current-buffer)))
    (if (re-search-forward
         (format org-complex-heading-regexp-format notes)
         nil t)
        (goto-char (point-at-eol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "*** " notes)
      (goto-char (point-at-eol))
      )
    )
  )

(defun qianmarv-org/insert-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  ;; (org-display-inline-images)
  (setq foldername (concat (file-name-directory(buffer-file-name)) "img/")
        filename (format-time-string "%Y%m%d_%H%M%S.png")
        fullfilename (concat foldername filename ))
  (if (not (file-exists-p foldername))
      (mkdir foldername))
                                        ;convert bitmap from clipboard to file
  (if (eq system-type 'windows-nt)
      (call-process "convert" nil nil nil "clipboard:" fullfilename))
                                        ; insert into file if correctly taken
  (if (file-exists-p fullfilename)
      (insert (concat "[[./IMG/" filename "]]"))))

(defun qianmarv-org/insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))
