;;; sapjira.el --- Adapt org-jira for SAP internal usage  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marvin Qian

;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: jira

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'org-jira)

(defvar sapjira-project-key nil)

(defconst SAPJIRA-TASK-ID "5")
(defconst SAPJIRA-BACKLOG-ID "6")

;; Only Get Task
(defvar sapjira-default-jql
      (format "assignee = currentUser() and resolution = unresolved and issueType = %s ORDER BY created ASC" SAPJIRA-TASK-ID))

(defvar sapjira-get-issue-list-callback
  (cl-function
   (lambda (&rest data &allow-other-keys)
     "Callback for async, DATA is the response from the request call."
     (let ((issues (append (cdr (assoc 'issues (cl-getf data :data))) nil)))
       (sapjira-get-issues issues)))))

(defun sapjira-get-issue-list ()
  (interactive
   (unless sapjira-project-key
     (setq sapjira-project-key (read-string "Input Project Key: "))))
  (let ((jql (format "project = \"%s\" and %s" sapjira-project-key sapjira-default-jql)))
    (jiralib-do-jql-search jql nil sapjira-get-issue-list-callback)))

(defun sapjira-get-issues (issues)
  "Get list of ISSUES into an org buffer. "

  ;; If the user doesn't provide a default, async call to build an issue list
  ;; from the JQL style query
  (let* ((project-key sapjira-project-key)
         (project-file (expand-file-name (concat project-key ".org") org-jira-working-dir))
         (project-buffer (or (find-buffer-visiting project-file)
                             (find-file project-file))))
    (with-current-buffer project-buffer
      (save-excursion
        (org-jira-mode t)
        (widen)
        (outline-show-all)
        (goto-char (point-min))

        (message "Start to Process %i Tasks" (length issues))

        (mapc (lambda (issue)
                (sapjira-handle-task issue)) issues)
        (switch-to-buffer project-buffer)
        (message "End of Proccess")))))


(defun sapjira-get-issue-type (issue)
  (org-jira-find-value (assoc 'fields issue) 'issuetype 'id))

(defun sapjira-get-parent-issue(issue)
  (cdr (assoc 'parent (cdr (assoc 'fields issue))))
  )

(defun sapjira-handle-task (issue)
  (let* ((issue-id (org-jira-get-issue-key issue))
         (issue-summary (org-jira-get-issue-summary issue))
         (issue-headline issue-summary)
         (parent-issue (sapjira-get-parent-issue issue))
         (parent-id (org-jira-get-issue-key parent-issue)))

    (setq p (org-find-entry-with-id issue-id))

    ;; (message "Backlog => %s" issue-id)
    (save-restriction
      (if (and p (>= p (point-min))
               (<= p (point-max)))
          (progn
            (goto-char p)
            (forward-thing 'whitespace)
            (kill-line)) ;; Find Existing
        ;; Not Found, Try to Find Parent
        ;; TODO Try not to always update parent
        ;; Always Update Parent information for now
        (sapjira-handle-backlog parent-issue)
        (setq parent_p (org-find-entry-with-id parent-id)) ;; Should Always Find!
        (org-narrow-to-subtree)

        (goto-char (point-max))
        (unless (looking-at "^")
          (insert "\n"))
        (insert "** ")
        )

      (let ((status (org-jira-get-issue-val 'status issue)))
        (org-jira-insert (concat (cond (org-jira-use-status-as-todo
                                        (upcase (replace-regexp-in-string " " "-" status)))
                                       ((member status org-jira-done-states) "DONE")
                                       ("TODO")) " "
                                       issue-headline)))
      (save-excursion
        (unless (search-forward "\n" (point-max) 1)
          (insert "\n")))

      (org-narrow-to-subtree)
      (org-change-tag-in-region
       (point-min)
       (save-excursion
         (forward-line 1)
         (point))
       (replace-regexp-in-string "-" "_" issue-id)
       nil)

      (mapc (lambda (entry)
              (let ((val (org-jira-get-issue-val entry issue)))
                (when (or (and val (not (string= val "")))
                          (eq entry 'assignee)) ;; Always show assignee
                  (org-jira-entry-put (point) (symbol-name entry) val))))
            '(assignee reporter type priority resolution status components created updated))

      (org-jira-entry-put (point) "ID" (org-jira-get-issue-key issue))

      ;; Insert the duedate as a deadline if it exists
      (when org-jira-deadline-duedate-sync-p
        (let ((duedate (org-jira-get-issue-val 'duedate issue)))
          (when (> (length duedate) 0)
            (org-deadline nil duedate))))

      ;; only sync worklog clocks when the user sets it to be so.
      ;; FIXME By Marvin: Enable it will lead to system interrupt 
      ;; (when org-jira-worklog-sync-p
      ;;   (org-jira-update-worklogs-for-current-issue))
      )))

(defun sapjira-handle-backlog (issue)
  (let* ((issue-id (org-jira-get-issue-key issue))
         (issue-summary (org-jira-get-issue-summary issue))
         (issue-headline issue-summary))

    ;; (message "Backlog => %s" issue-id)
    (setq p (org-find-entry-with-id issue-id))

    (save-restriction
      (if (and p (>= p (point-min))
               (<= p (point-max)))
          (progn
            (goto-char p)
            (forward-thing 'whitespace)
            (kill-line))
        (goto-char (point-max))
        (unless (looking-at "^")
          (insert "\n"))
        (insert "* ")
        )

      (let ((status (org-jira-get-issue-val 'status issue)))
        (org-jira-insert (concat (cond (org-jira-use-status-as-todo
                                        (upcase (replace-regexp-in-string " " "-" status)))
                                       ((member status org-jira-done-states) "DONE")
                                       ("TODO")) " "
                                       issue-headline)))
      (save-excursion
        (unless (search-forward "\n" (point-max) 1)
          (insert "\n")))

      (org-narrow-to-subtree)
      (org-change-tag-in-region
       (point-min)
       (save-excursion
         (forward-line 1)
         (point))
       (replace-regexp-in-string "-" "_" issue-id)
       nil)

      (mapc (lambda (entry)
              (let ((val (org-jira-get-issue-val entry issue)))
                (when (or (and val (not (string= val "")))
                          (eq entry 'assignee)) ;; Always show assignee
                  (org-jira-entry-put (point) (symbol-name entry) val))))
            '(assignee type priority status ))


      (org-jira-entry-put (point) "ID" (org-jira-get-issue-key issue))

      ;; Insert the duedate as a deadline if it exists
      (when org-jira-deadline-duedate-sync-p
        (let ((duedate (org-jira-get-issue-val 'duedate issue)))
          (when (> (length duedate) 0)
            (org-deadline nil duedate))))

      ;; only sync worklog clocks when the user sets it to be so.
      ;; (when org-jira-worklog-sync-p
      ;;   (org-jira-update-worklogs-for-current-issue))
      )))
