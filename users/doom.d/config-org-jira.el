;;; config-org-jira.el --- Jira integration for OrgLife -*- lexical-binding: t; -*-

;; Hybrid Jira ticket management for OrgLife:
;;   - View issues assigned to me        (org-jira-get-issues / custom JQL)
;;   - Navigate an epic and its children  (org-life-jira-open-epic)
;;   - Update issue status (write-back)   (org-jira-progress-issue, live transitions)
;;
;; Engine: org-jira (ahungry/org-jira). It targets Jira Cloud's modern
;; /rest/api/3/search/jql endpoint (jiralib-target-api-version 3) and reads the
;; API token natively from ~/.authinfo via auth-source (set in config.el).
;; Synced issues are written as ordinary Org headings under `org-jira-working-dir',
;; so the existing OrgLife agenda, org-super-agenda groups, and startup dashboard
;; surface them with almost no extra plumbing.
;;
;; One-time setup (NOT in this repo):
;;   1. Create a Cloud API token: https://id.atlassian.com/manage/api-tokens
;;   2. Add to ~/.authinfo (host must match `jiralib-url'; password is the token):
;;        machine join-build.atlassian.net login jon@join.build password <API_TOKEN> port 443

(eval-and-compile
  (unless (fboundp 'after!)
    (defmacro after! (_feature &rest body) `(progn ,@body)))
  (unless (fboundp 'map!)
    (defmacro map! (&rest _args) nil)))

;; --- Connection + working dir --------------------------------------------
;; Atlassian Cloud site for join.build.
(setq jiralib-url "https://join-build.atlassian.net")
(setq jiralib-target-api-version 3)   ;; force modern /rest/api/3/search/jql
(setq org-jira-working-dir (expand-file-name "~/org/jira/"))

;; Auth: no code needed — jiralib reads ~/.authinfo via auth-source
;; (`auth-sources' is already configured in config.el).

;; --- Jira status string -> OrgLife keyword (see config-org-gtd.el) --------
;; Maps fetched Jira statuses onto the existing TODO/NEXT/WAITING/DONE/CANCELLED
;; keywords so super-agenda :todo grouping and the dashboard work unchanged.
;; Extend this alist after the first sync to cover your project's real statuses.
(setq org-jira-use-status-as-todo t)
(setq org-jira-jira-status-to-org-keyword-alist
      '(("To Do" . "TODO") ("Open" . "TODO") ("Backlog" . "TODO")
        ("Selected for Development" . "NEXT") ("In Progress" . "NEXT")
        ("In Review" . "WAITING") ("In Code Review" . "WAITING") ("Blocked" . "WAITING")
        ("Done" . "DONE") ("Closed" . "DONE") ("Resolved" . "DONE")
        ("Won't Do" . "CANCELLED") ("Cancelled" . "CANCELLED")))

;; Default path for `org-jira-progress-issue-next' (SPC o J n).
(setq org-jira-progress-issue-flow
      '(("To Do" . "In Progress") ("In Progress" . "Done")))

;; --- Goal 1: assigned-to-me as a scoped saved JQL ------------------------
;; `statusCategory != Done' is robust across custom workflow statuses.
;; Writes results to ~/org/jira/my-open.org.
(setq org-jira-custom-jqls
      '((:jql "assignee = currentUser() AND statusCategory != Done ORDER BY updated DESC"
         :limit 100 :filename "my-open")))

;; --- Hybrid surface: make Jira issues a first-class agenda source --------
(after! org
  (unless (file-directory-p org-jira-working-dir)
    (ignore-errors (make-directory org-jira-working-dir t)))
  (add-to-list 'org-agenda-files org-jira-working-dir t))

;; --- Goal 2: epic navigation ---------------------------------------------
;; org-jira has no first-class epic command. This thin wrapper drives its own
;; custom-JQL machinery with `parent = EPIC-KEY' (the modern Cloud field that
;; covers both team- and company-managed projects), then visits the resulting
;; file so the children can be navigated as an ordinary Org outline.
;; NOTE: on a legacy company-managed project where `parent' is incomplete,
;; change the JQL below to (format "\"Epic Link\" = %s ..." key).
(defun org-life-jira-open-epic (epic-key)
  "Sync children of EPIC-KEY into ~/org/jira/epic-<KEY>.org and visit it."
  (interactive "sEpic key (e.g. ENG-123): ")
  (let* ((key (upcase (string-trim epic-key)))
         (org-jira-custom-jqls
          (list (list :jql (format "parent = %s ORDER BY status, priority DESC" key)
                      :limit 100 :filename (format "epic-%s" key)))))
    (org-jira-get-issues-from-custom-jql org-jira-custom-jqls)
    (let ((file (expand-file-name (format "epic-%s.org" key) org-jira-working-dir)))
      (when (file-exists-p file) (find-file file)))))

(provide 'config-org-jira)
;;; config-org-jira.el ends here
