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
    ;; (org-jira :location elpa)
    ;; org-pomodoro
    ))


;; Refer to: https://github.com/jfim/org-jira
;; Fixed authorization issue
;;;; https://www.emacswiki.org/emacs/GnusEncryptedAuthInfo
;;;; https://github.com/ahungry/org-jira
;; (defun qianmarv-org/init-org-jira ()
;;   (use-package org-jira
;; ;;    :defer t
;;     :config
;;     (progn
;;       (spacemacs/declare-prefix-for-mode 'org-mode "mj" "jira")
;;       (spacemacs/declare-prefix-for-mode 'org-mode "mjp" "projects")
;;       (spacemacs/declare-prefix-for-mode 'org-mode "mji" "issues")
;;       (spacemacs/declare-prefix-for-mode 'org-mode "mjs" "subtasks")
;;       (spacemacs/declare-prefix-for-mode 'org-mode "mjc" "comments")
;;       (spacemacs/declare-prefix-for-mode 'org-mode "mjt" "todos")
;;       (spacemacs/set-leader-keys-for-major-mode 'org-mode
;;         "jpg" 'org-jira-get-projects
;;         "jib" 'org-jira-browse-issue
;;         "jig" 'org-jira-get-issues
;;         "jih" 'org-jira-get-issues-headonly
;;         "jif" 'org-jira-get-issues-from-filter-headonly
;;         "jiF" 'org-jira-get-issues-from-filter
;;         "jiu" 'org-jira-update-issue
;;         "jiw" 'org-jira-progress-issue
;;         "jir" 'org-jira-refresh-issue
;;         "jic" 'org-jira-create-issue
;;         "jik" 'org-jira-copy-current-issue-key
;;         "jsc" 'org-jira-create-subtask
;;         "jsg" 'org-jira-get-subtasks
;;         "jcu" 'org-jira-update-comment
;;         "jtj" 'org-jira-todo-to-jira)
;;       )
;;     ))

;; (defun qianmarv-org/post-init-org-jira()
;;   (progn
;;     (setq jiralib-url "https://sapjira.wdf.sap.corp:443")
;;     (setq org-jira-working-dir "~/Emacs/GTD")
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

      ;; --------------------------------------------------------------------
      ;; Encypting files or org
      ;; http://orgmode.org/worg/org-tutorials/e
      ;; --------------------------------------------------------------------
      ;; (require 'epa-file)
      (epa-file-enable)
      (require 'org-crypt)
      (org-crypt-use-before-save-magic)
      (setq org-tags-exclude-from-inheritance (quote ("crypt")))
      ;; GPG key to use for encryption
      ;; Either the Key ID or set to nil to use symmetric encryption.
      (setq org-crypt-key "AC88F93004D199BC")



      ;;FIXME If the Emacs keep open then the file name would be not correct if passes one month!
      ;;TODO Put it in a single Journal.org file, and like other files, keep data for one year only.
      (setq qianmarv-org/gtd-path "~/Emacs/GTD/2018")
      (setq qianmarv-org/journal-file (qianmarv-org/get-monthly))
      (setq org-capture-templates
            `(("t" "Todo" entry (file+headline ,(format "%s/Inbox.org" qianmarv-org/gtd-path ) "Tasks")
               "* %?\n %i\n")
              ("j" "Morning Write" entry (file+function qianmarv-org/journal-file qianmarv-org/find-date-entry)
               "** Morning Write \n %?\n%T\n")
              ("m" "Notes" plain (file+function qianmarv-org/journal-file qianmarv-org/find-date-entry-notes)
               "\n%T\n%?\n%a\n\n")
              ("d" "Daily Review" entry (file+function qianmarv-org/journal-file qianmarv-org/find-date-entry)
               "** Daily Review\n %?\n%T\n")
              ("o" "Other" entry (file+headline ,(format "%s/Event.org" qianmarv-org/gtd-path ) "Other Interrupt")
               "* DONE %? \n%U %i\n" :clock-in t :clock-resume t)))

      (setq org-agenda-files
            `(
              ,(format "%s/Projects.org" qianmarv-org/gtd-path)
              ,(format "%s/Event.org" qianmarv-org/gtd-path)
              ,(format "%s/Agenda.org" qianmarv-org/gtd-path)
              ,(format "%s/Calendar.org" qianmarv-org/gtd-path)
              ,(format "%s/Habit.org" qianmarv-org/gtd-path)
              ))
      ;;      (spacemacs/set-leader-keys "'" 'qianmarv-org/insert-screenshot)

      )))
