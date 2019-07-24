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
    (let* ((v-path (format "~/Emacs/Journal/%s/" (format-time-string "%Y" (current-time))))
           (v-filename (format "%s.org" (format-time-string "%Y-%m" (current-time))))
           (v-fullfile (format "%s%s" v-path v-filename)))
      (when (not (file-exists-p v-path)) (make-directory v-path))
      (when (not (file-exists-p v-fullfile))
          (save-excursion
            (copy-file "~/Emacs/Journal/template.org" v-fullfile)
            (set-buffer (find-file-noselect (qianmarv-org/get-monthly)))
            (re-search-forward "<title>" (point-max) t 1)
            (replace-match (format "Monthly Plan & Review for %s" (format-time-string "%Y-%m" (current-time))))
            (save-buffer)))
      v-fullfile))

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
        ))))

(defun qianmarv-org/find-week-entry()
  (progn
    (qianmarv-org/find-h1 "Weekly Review")
      (goto-char (point-at-bol))))

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
      )))

;; (defun qianmarv-org/find-track-entry-books()
;;   (progn
;;     (qianmarv-org/find-h1 "Reading")
;;     ;; (org-narrow-to-subtree )
;;     (helm-org-in-buffer-headings)))
;; (defun qianmarv-org/track-reading()
;;   (interactive)
;;   (let ((track-file "~/Emacs/Notebooks/005_Book.org"))
;;     ))

(defun qianmarv-org/find-date-entry-notes()
  (let ((notes-title "Memo"))
    (qianmarv-org/find-date-entry)
    (unless (derived-mode-p 'org-mode)
      (error
       "Target buffer \"%s\" for file+headline should be in Org mode"
       (current-buffer)))
    (if (re-search-forward
         (format org-complex-heading-regexp-format notes-title)
         nil t)
        (progn
          (org-narrow-to-subtree)
          (goto-char (point-max)))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "*** " notes-title)
      (goto-char (point-at-eol))
      )))

;; (defun qianmarv-org/find-event-entry-fix-break()
;;   (let ((entry-name "Track Fixed Break for Tea/WC"))
;;     (qianmarv-org/find-h1 "Short Break")
;;     (unless (derived-mode-p 'org-mode)
;;       (error
;;        "Target buffer \"%s\" for file+headline should be in Org mode"
;;        (current-buffer)))
;;     (if (re-search-forward
;;          (format org-complex-heading-regexp-format entry-name)
;;          nil t)
;;         (progn
;;           (org-narrow-to-subtree)
;;           (goto-char (point-max)))
;;       (goto-char (point-max))
;;       (or (bolp) (insert "\n"))
;;       (insert "** " entry-name)
;;       (goto-char (point-at-eol)))))
(defun org-insert-clock-range (&optional n)
  (interactive "NTime Offset (in min): ")
  (let* ((ctime (cdr (decode-time (current-time))))
         (min (car ctime))
         (start (apply 'encode-time 0 (- min n) (cdr ctime))))
    (org-insert-time-stamp start t t "CLOCK: ")
    (insert "--")
    (org-insert-time-stamp (current-time) t t)))

(defun qianmarv-org/insert-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  ;; (org-display-inline-images)
  (let*
      ;; foldername (replace-regexp-in-string "\.org" "" (buffer-file-name))
      ((image-folder "IMG")
       (image-name (format-time-string "%Y%m%d_%H%M%S.png"))
       (parent-directory (file-name-directory (buffer-file-name)))
       ;; subfolder (replace-regexp-in-string "\.org" "" (file-name-nondirectory (buffer-file-name)))
       (relative-image-path  (concat "./" image-folder "/" image-name))
       (full-image-directory (concat parent-directory "/" image-folder))
       (full-image-path (concat full-image-directory "/" image-name)))
    (if (not (file-exists-p full-image-directory))
        (mkdir full-image-directory))
    ;;convert bitmap from clipboard to file
    ;;https://imagemagick.org/script/download.php
    (if (eq system-type 'windows-nt)
        (call-process "magick" nil nil nil  "clipboard:" full-image-path))
    ;; insert into file if correctly taken
    (if (file-exists-p full-image-path)
        (progn
          (insert (message "#+CAPTION: %s" (read-from-minibuffer "Caption: ")))
          (indent-new-comment-line)
          (insert (message "[[./%s/%s]]" image-folder image-name))))))

(defun qianmarv-org/insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("abap" "emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby" "racket"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun qianmarv-org/insert-quote (quote-format)
  "Insert quote"
  (interactive
   (let ((quote-formats
          '("VERSE" "QUOTE" "CENTER")))
     (list (ido-completing-read "Quote Format: " quote-formats))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_%s\n" quote-format))
    (newline-and-indent)
    (insert (format "#+END_%s\n" quote-format))
    (previous-line 2)))


;; Credit to https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
(defun qianmarv-org/get-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

(defun qianmarv-org/get-global-prop(property)
  (org-element-property :value (car (qianmarv-org/get-global-props property)))
  )
;; Different Apps to Be Called Under Different OS Platform
;;   Win: Powershell Tool https://github.com/Windos/BurntToast
(defun qianmarv-org/show-alarm (min-to-app new-time message)
  (cond ((string-equal system-type "windows-nt")
         (call-process "powershell"
                       nil
                       t
                       nil
                       (format " New-BurntToastNotification -Text '%s' -Sound 'Alarm2' -SnoozeAndDismiss" message)
                       ))))


;; Different Apps to Be Called Under Different OS Platform
;;   Win: Powershell Tool https://github.com/Windos/BurntToast
(defun qianmarv-org/show-alarm (min-to-app new-time message)
  (cond ((string-equal system-type "windows-nt")
         (call-process "powershell"
                       nil
                       t
                       nil
                       (format " New-BurntToastNotification -Text '%s' -Sound 'Alarm2' -SnoozeAndDismiss" message)
                       ))))

;;; Show the clocked-in task - if any - in the header line
(defun qianmarv/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun qianmarv/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))
