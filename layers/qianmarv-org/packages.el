;;; packages.el --- qianmarv Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2017 qianmarv
;;
;; Author: qianmarv <qianmarv@gmail.com>
;; URL: https://github.com/qianmarv/
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(defconst qianmarv-org-packages
  '(
    (org :location built-in)
    (org-super-agenda)
    ;; (org-jira
    ;;  ;; :location elpa)
    ;;  :location (recipe :fetcher github :repo "qianmarv/org-jira"))
    ;; org-pomodoro
    ))

(defun qianmarv-org/init-org-super-agenda()
  (use-package org-super-agenda
    :after org-agenda
    :config
    (setq org-super-agenda-groups
          '((:name "Log "
                   :log t)
            (:name "Current Focus "
                   :todo "STARTED")
            (:name "Schedule "
                   :time-grid t)
            (:name "Today "
                   :scheduled today)
            (:name "Habits "
                   :habit t)
            (:name "Due today "
                   :deadline today)
            (:name "Overdue "
                   :deadline past)
            (:name "Due soon "
                   :deadline future)
            (:name "Scheduled earlier "
                   :scheduled past)))
    (org-super-agenda-mode)))
;; Refer to: https://github.com/jfim/org-jira
;; Fixed authorization issue
;;;; https://www.emacswiki.org/emacs/GnusEncryptedAuthInfo
;;;; https://github.com/ahungry/org-jira
;; (defun qianmarv-org/init-org-jira ()
;;  (use-package org-jira
;; ;;    ;;    :defer t
;; ;;    :config
;; ;;    (progn
;; ;;      (spacemacs/declare-prefix-for-mode 'org-mode "mj" "jira")
;; ;;      (spacemacs/declare-prefix-for-mode 'org-mode "mjp" "projects")
;; ;;      (spacemacs/declare-prefix-for-mode 'org-mode "mji" "issues")
;; ;;      (spacemacs/declare-prefix-for-mode 'org-mode "mjs" "subtasks")
;; ;;      (spacemacs/declare-prefix-for-mode 'org-mode "mjc" "comments")
;; ;;      (spacemacs/declare-prefix-for-mode 'org-mode "mjt" "todos")
;; ;;      (spacemacs/set-leader-keys-for-major-mode 'org-mode
;; ;;        "jpg" 'org-jira-get-projects
;; ;;        "jib" 'org-jira-browse-issue
;; ;;        "jig" 'org-jira-get-issues
;; ;;        "jih" 'org-jira-get-issues-headonly
;; ;;        "jif" 'org-jira-get-issues-from-filter-headonly
;; ;;        "jiF" 'org-jira-get-issues-from-filter
;; ;;        "jiu" 'org-jira-update-issue
;; ;;        "jiw" 'org-jira-progress-issue
;; ;;        "jir" 'org-jira-refresh-issue
;; ;;        "jic" 'org-jira-create-issue
;; ;;        "jik" 'org-jira-copy-current-issue-key
;; ;;        "jsc" 'org-jira-create-subtask
;; ;;        "jsg" 'org-jira-get-subtasks
;; ;;        "jcu" 'org-jira-update-comment
;; ;;        "jtj" 'org-jira-todo-to-jira)
;; ;;      )
;;    ))

;; (defun qianmarv-org/post-init-org-jira()
;;   (progn
;;     (setq jiralib-url "https://sapjira.wdf.sap.corp:443")
;;     (setq org-jira-working-dir "~/Emacs/GTD/2018")
;;     (setq org-jira-serv-alist '(
;;                                (org-jira :username I074218)
;;                                ))
;;     ))
;; Refer to: https://github.com/zilongshanren/spacemacs-private/blob/develop/layers/zilongshanren-org/packages.el
(defun qianmarv-org/post-init-org ()
  (add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append)
  (with-eval-after-load 'org
    (progn
      (spacemacs|disable-company org-mode)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "," 'org-priority
        "ir" 'qianmarv-org/insert-src-block
        "iq" 'qianmarv-org/insert-quote
        "ip" 'qianmarv-org/insert-screenshot)
      (require 'org-compat)
      (require 'org)
      ;; (add-to-list 'org-modules "org-habit")
      ;; (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)
      ;; Set auto fill in text mode
      (add-hook 'org-mode-hook 'auto-fill-mode)

      ;; Resume Clocking Task On Clock-in If The Clock Is Open
      (setq org-clock-in-resume t)

      ;; Resume Clocking Task When Emacs Is Restarted
      (org-clock-persistence-insinuate)

      ;; Save The Running Clock And All Clock History When Exiting Emacs, Load It On Startup
      (setq org-clock-persist t)

      ;; Do Not Prompt To Resume An Active Clock
      (setq org-clock-persist-query-resume nil)

      ;; Set Agenda Span To Daily By Default
      (setq org-agenda-span 'day)

      ;; Removes Clocked Tasks With 0:00 Duration
      (setq org-clock-out-remove-zero-time-clocks t)

      ;; Set Org Clock, Change default leve from 2 to 3
      (setq org-clock-clocktable-default-properties '(:maxlevel 3 :scope file))

      ;; When Setting This Variable To nil,
      ;; 'a_b' Will Not Be Interpreted As A Subscript, But 'a_{b}' Will.
      ;; Default value is t
      (setq org-export-with-sub-superscripts nil)

      ;; Active Org-babel languages
      ;;      (org-babel-do-load-languages
      ;;       'org-babel-load-languages
      ;;       '(;; other Babel languages
      ;;
      ;;         ;; Config plantuml
      ;;         ;; http://archive.3zso.com/archives/plantuml-quickstart.html
      ;;         (plantuml . t)
      ;;         (ditaa . t)
      ;;         (python . t)
      ;;         (perl . t)
      ;;         (ruby . t)
      ;;         (R . t)
      ;;         (sh . t)
      ;;         (gnuplot . t)
      ;;         (org . t)
      ;;         (latex . t)
      ;;         (java . t)
      ;;         (emacs-lisp . t)
      ;;         (racket . t)
      ;;         (calc . t)
      ;;         (sql . t)
      ;;         (dot . t)
      ;;         ))
      ;; Config plantuml path
      (setq org-plantuml-jar-path
            (expand-file-name "~/.spacemacs.d/plugins/plantuml.jar"))
      ;; Config ditaa path
      (setq org-ditaa-jar-path
            (expand-file-name "~/.spacemacs.d/plugins/ditaa0_9.jar")) ;

      ;; Setup for GTD
      ;; Refer to http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
      (setq org-todo-keywords
            '(
              (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
              (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
              ))

      (setq org-todo-keyword-faces
            '(("IDEA" . (:foreground "GoldenRod" :weight bold))
              ("NEXT" . (:foreground "IndianRed1" :weight bold))   
              ("STARTED" . (:foreground "OrangeRed" :weight bold))
              ("WAITING" . (:foreground "coral" :weight bold)) 
              ("CANCELED" . (:foreground "LimeGreen" :weight bold))
              ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
              ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
              ))

      ;; (setq org-tag-persistent-alist
      ;;       '((:startgroup . nil)
      ;;         ("G@2019_EfficientWork" . ?e)
      ;;         ("G@2019_DevExpert" . ?d)
      ;;         ("G@2019_BizExpert" . ?b)
      ;;         (:endgroup . nil)
      ;;         ;; (:startgroup . nil)
      ;;         ;; ("EASY" . ?e)
      ;;         ;; ("MEDIUM" . ?m)
      ;;         ;; ("HARD" . ?a)
      ;;         ;; (:endgroup . nil)
      ;;         ;; ("URGENT" . ?u)
      ;;         ;; ("KEY" . ?k)
      ;;         ;; ("BONUS" . ?b)
      ;;         )
      ;;       )

      (setq org-tag-faces
            '(
              ("G@2019_EfficientWork" . (:foreground "GoldenRod" :weight bold))
              ("G@2019_DevExpert" . (:foreground "GoldenRod" :weight bold))
              ("G@2019_BizExpert" . (:foreground "GoldenRod" :weight bold))
              ("G@2019_MSE2020" . (:foreground "Red" :weight bold))
              ("G@2019_SelfMastery" . (:foreground "Red" :weight bold))
              ("@2019_Emacsen" . (:foreground "Red" :weight bold))
              ("@2019_Health" . (:foreground "OrangeRed" :weight bold))
              ("G@2019_Education" . (:foreground "OrangeRed" :weight bold))
              ("G@2019_Trip" . (:foreground "OrangeRed" :weight bold))
              ;; ("BONUS" . (:foreground "GoldenRod" :weight bold))
              )
            )

      (setq org-enable-priority-commands nil)
      ;; --------------------------------------------------------------------
      ;; Encypting files or org
      ;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
      ;; --------------------------------------------------------------------
      ;; (require 'epa-file)
      (epa-file-enable)
      (require 'org-crypt)
      (org-crypt-use-before-save-magic)
      (setq org-tags-exclude-from-inheritance (quote ("crypt")))
      ;; GPG key to use for encryption
      ;; Either the Key ID or set to nil to use symmetric encryption.
      ;; 
      ;; (setq org-crypt-key "AC88F93004D199BC")
      (setq org-crypt-key nil)


      ;;
      ;;FIXME If the Emacs keep open then the file name would be not correct if passes one month!
      ;;TODO Put it in a single Journal.org file, and like other files, keep data for one year only.
      (setq qianmarv-org/gtd-path "~/Emacs/GTD")
      (setq qianmarv-org/journal-file (qianmarv-org/get-monthly))
      (setq org-capture-templates
            `(("c" "Collect/Capture")
              ("ci" "Capture Ideas/Mighty new todos" entry (file+headline ,(format "%s/Inbox.org" qianmarv-org/gtd-path ) "Ideas") "* IDEA %?\n %i\n")
              ("cb" "Books want Read" entry (file+headline ,(format "%s/Inbox.org" qianmarv-org/gtd-path ) "Books") (file "~/Emacs/GTD/templates/book.tpl"))
              ("cm" "Moviews want Watch" entry (file+headline ,(format "%s/Inbox.org" qianmarv-org/gtd-path ) "Movie") "* IDEA %?\n %i\n")
              ("cj" "Journals, Morning Write" entry (file+function qianmarv-org/journal-file qianmarv-org/find-date-entry)
               "* Morning Write \n\t%U\n\t%?")
              ("cn" "Take Notes" plain (file+function qianmarv-org/journal-file qianmarv-org/find-date-entry-notes) "- Note taken on %U \\\\\n  %K  \n%?" :empty-lines 1)
              ("b" "Break / Unplanned Interrupt")
              ("bi" "Other Interrupt - Requires Refile" entry (file+headline ,(format "%s/Event.org" qianmarv-org/gtd-path ) "Other Interrupts") "* DONE %? \n%U %i\n" :clock-in t :clock-resume t)
              ("bp" "Phone Calls" entry (file+headline ,(format "%s/Event.org" qianmarv-org/gtd-path ) "Phone Calls") "* DONE %? \n%U %i\n" :clock-in t :clock-resume t)
              ("bs" "Someone Inquiry" entry (file+headline ,(format "%s/Event.org" qianmarv-org/gtd-path ) "Someone Inquiry") "* DONE %? \n%U %i\n" :clock-in t :clock-resume t)
              ("bb" "Breaks, Tea Break / WC / etc..." entry (file+headline ,(format "%s/Event.org" qianmarv-org/gtd-path ) "Short Break") "* DONE %? \n%U %i\n" :clock-in t :clock-resume t)
              ("r" "Review")
              ("rd" "Daily Review"  entry (file+function qianmarv-org/journal-file qianmarv-org/find-date-entry) (file "~/Emacs/GTD/templates/daily_review.tpl"))
              ("rw" "Weekly Review" entry (file+function qianmarv-org/journal-file qianmarv-org/find-week-entry) (file "~/Emacs/GTD/templates/weekly_review.tpl"))
              ("rm" "Month Review" entry (file+function qianmarv-org/journal-file qianmarv-org/find-date-entry) "* Monthly Review\n%U\n%?")
              ))

      (setq org-agenda-files  `(,qianmarv-org/gtd-path))
      (setq org-agenda-compact-blocks nil)
            ;; `(
            ;;   ,(format "%s/Projects.org" qianmarv-org/gtd-path)
            ;;   ,(format "%s/Event.org" qianmarv-org/gtd-path)
            ;;   ,(format "%s/Agenda.org" qianmarv-org/gtd-path)
            ;;   ,(format "%s/Inbox.org" qianmarv-org/gtd-path)
            ;;   ,(format "%s/Habit.org" qianmarv-org/gtd-path)
            ;;   ))
      ;;      (spacemacs/set-leader-keys "'" 'qianmarv-org/insert-screenshot)

      ;; --------------------------------------------------------------------
      ;; Settings for Reminder - appt
      ;; Refer to: https://emacs.stackexchange.com/questions/3844/good-methods-for-setting-up-alarms-audio-visual-triggered-by-org-mode-events
      ;; --------------------------------------------------------------------
      (setq appt-disp-window-function 'qianmarv-org/show-alarm)
      (setq appt-message-warning-time 5) ; Show notification 5 minutes before event
      (setq appt-display-interval appt-message-warning-time) ; Disable multiple reminders
      (setq appt-display-mode-line nil)
      ;; Add Desktop Notification
      ;; Refer to https://gist.github.com/jstewart/7664823
      (add-hook 'org-pomodoro-finished-hook
                (lambda ()
                  (qianmarv-org/show-alarm 0 0 "Pomodoro completed! - Time for a break.")))

      (add-hook 'org-pomodoro-break-finished-hook
                (lambda ()
                  (qianmarv-org/show-alarm 0 0 "Pomodoro Short Break Finished - Ready for Another?")))

      (add-hook 'org-pomodoro-long-break-finished-hook
                (lambda ()
                  (qianmarv-org/show-alarm 0 0 "Pomodoro Long Break Finished - Ready for Another?")))

      (add-hook 'org-pomodoro-killed-hook
                (lambda ()
                  (qianmarv-org/show-alarm 0 0 "Pomodoro Killed - One does not simply kill a pomodoro!")))
      )))
