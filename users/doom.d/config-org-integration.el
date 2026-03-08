;;; config-org-integration.el --- org-life integration primitives -*- lexical-binding: t; -*-

(require 'org)
(require 'org-id)
(require 'subr-x)

(defconst org-life-integration-gtd-directory
  (expand-file-name "~/org/gtd/")
  "Directory containing canonical GTD task/project headings.")

(defconst org-life-integration-prop-link-kind "ORGLIFE_LINK_KIND")
(defconst org-life-integration-prop-target-id "ORGLIFE_LINK_TARGET_ID")
(defconst org-life-integration-prop-target-title "ORGLIFE_LINK_TARGET_TITLE")
(defconst org-life-integration-prop-target-file "ORGLIFE_LINK_TARGET_FILE")
(defconst org-life-integration-prop-target-type "ORGLIFE_LINK_TARGET_TYPE")

(defun org-life-integration--require-heading ()
  "Raise a user error when point is not on an Org heading."
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not an Org buffer"))
  (unless (org-at-heading-p)
    (user-error "Place point on a heading before linking")))

(defun org-life-integration--ensure-id-at-heading ()
  "Ensure current heading has an ID and return it."
  (org-life-integration--require-heading)
  (org-id-get-create)
  (org-entry-get nil "ID" t))

(defun org-life-integration--insert-link-after-metadata (target-id target-title)
  "Insert an inline id link for TARGET-ID and TARGET-TITLE.
Returns the inserted link string."
  (let ((link (format "[[id:%s][%s]]" target-id target-title)))
    (save-excursion
      (org-back-to-heading t)
      (org-end-of-meta-data t)
      (unless (bolp)
        (insert "\n"))
      (insert link "\n"))
    link))

(defun org-life-integration--store-link-metadata (kind target-id target-title target-file target-type)
  "Persist ORGLIFE link metadata on the current heading."
  (org-entry-put nil org-life-integration-prop-link-kind kind)
  (org-entry-put nil org-life-integration-prop-target-id target-id)
  (org-entry-put nil org-life-integration-prop-target-title target-title)
  (org-entry-put nil org-life-integration-prop-target-file target-file)
  (org-entry-put nil org-life-integration-prop-target-type target-type))

(defun org-life-integration--select-roam-node ()
  "Prompt for an org-roam node and return plist metadata for it."
  (require 'org-roam)
  (let ((node (org-roam-node-read nil nil t)))
    (list :id (org-roam-node-id node)
          :title (org-roam-node-title node)
          :file (org-roam-node-file node)
          :type "roam")))

(defun org-life-integration--gtd-heading-candidates ()
  "Return GTD TODO heading candidates for heading-level links."
  (let ((candidates nil)
        (files (seq-filter
                (lambda (file)
                  (and (string-match-p "\\.org\\'" file)
                       (not (string-match-p "/archive/" file))))
                (directory-files-recursively org-life-integration-gtd-directory "\\.org\\'"))))
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward org-heading-regexp nil t)
           (org-back-to-heading t)
           (let ((todo (org-get-todo-state))
                 (title (org-get-heading t t t t)))
             (when (and todo (not (string-empty-p title)))
               (push (cons (format "%s :: %s"
                                   (file-name-nondirectory file)
                                   title)
                           (list :marker (copy-marker (point))
                                 :title title
                                 :file file
                                 :type "gtd-heading"))
                     candidates)))))))
    (nreverse candidates)))

(defun org-life-integration--select-gtd-heading ()
  "Prompt for a concrete GTD heading and return plist metadata with stable ID."
  (let* ((candidates (org-life-integration--gtd-heading-candidates))
         (choice (completing-read
                  "Link journal entry to GTD heading: "
                  (mapcar #'car candidates)
                  nil t)))
    (unless (and choice (not (string-empty-p choice)))
      (user-error "Heading selection is required"))
    (let* ((metadata (cdr (assoc choice candidates)))
           (marker (plist-get metadata :marker))
           target-id)
      (unless marker
        (user-error "Could not resolve heading target"))
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (org-back-to-heading t)
          (setq target-id (org-id-get-create))))
      (plist-put metadata :id target-id)
      metadata)))

(defun org-life-integration--create-link (target kind)
  "Create dual-representation link to TARGET and persist metadata for KIND."
  (org-life-integration--require-heading)
  (org-life-integration--ensure-id-at-heading)
  (let* ((target-id (plist-get target :id))
         (target-title (plist-get target :title))
         (target-file (plist-get target :file))
         (target-type (or (plist-get target :type) "heading")))
    (org-life-integration--insert-link-after-metadata target-id target-title)
    (org-life-integration--store-link-metadata kind target-id target-title target-file target-type)
    (message "Linked to %s" target-title)))

(defun org-life-link-task-to-roam ()
  "Link the current GTD heading to an org-roam node using a stable ID link."
  (interactive)
  (org-life-integration--create-link
   (org-life-integration--select-roam-node)
   "task-to-roam"))

(defun org-life-link-journal-to-heading ()
  "Link the current journal heading to a concrete GTD heading target."
  (interactive)
  (org-life-integration--create-link
   (org-life-integration--select-gtd-heading)
   "journal-to-heading"))

(defun org-life-integration-capture-link-prompt (context)
  "Prompt for an optional link during capture CONTEXT and return template text.
CONTEXT should be "task" or "journal"."
  (let ((ctx (if (symbolp context) (symbol-name context) context)))
    (if (not (y-or-n-p "Add integration link? "))
        ""
      (pcase ctx
        ("task"
         (let* ((target (org-life-integration--select-roam-node))
                (target-id (plist-get target :id))
                (target-title (plist-get target :title)))
           (format "[[id:%s][%s]]" target-id target-title)))
        ("journal"
         (let* ((target (org-life-integration--select-gtd-heading))
                 (target-id (plist-get target :id))
                 (target-title (plist-get target :title)))
            (format "[[id:%s][%s]]" target-id target-title)))
        (_ "")))))

(defun org-life-dashboard-open ()
  "Open Doom dashboard using the best available entrypoint."
  (interactive)
  (cond
   ((fboundp '+doom-dashboard/open) (+doom-dashboard/open))
   ((fboundp 'doom/open-dashboard) (doom/open-dashboard))
   (t (user-error "No Doom dashboard open command is available"))))

(defun org-life-dashboard-refresh ()
  "Refresh Doom dashboard using the best available entrypoint."
  (interactive)
  (cond
   ((fboundp '+doom-dashboard/reload) (+doom-dashboard/reload))
   ((fboundp '+doom-dashboard/open) (+doom-dashboard/open))
   (t (user-error "No Doom dashboard refresh command is available"))))

(map! :leader
      (:prefix ("o" . "org-life")
       :desc "Capture (DWIM)" "c" #'my/org-capture-dwim
       :desc "Capture menu" "C" #'org-capture
       (:prefix ("a" . "agenda/review")
        :desc "Daily planning" "d" (cmd! (org-agenda nil "d"))
        :desc "Weekly planning" "w" (cmd! (org-agenda nil "w"))
        :desc "Daily review" "r" (cmd! (org-agenda nil "r"))
        :desc "Weekly review" "R" (cmd! (org-agenda nil "R"))
        :desc "Inbox dashboard" "i" (cmd! (org-agenda nil "I"))
        :desc "Context review @home" "h" (cmd! (org-agenda nil "H"))
        :desc "Context review @work" "W" (cmd! (org-agenda nil "W")))
       (:prefix ("g" . "gtd")
        :desc "Open GTD inbox" "i" #'my/org-gtd-open-inbox)
       (:prefix ("j" . "journal")
        :desc "Open today's journal" "t" #'org-life-journal-open-today
        :desc "Search journal history" "s" #'org-life-journal-search-history)
       (:prefix ("r" . "roam")
        :desc "Find roam note" "f" #'org-life-roam-node-find
        :desc "Insert roam link" "i" #'org-life-roam-node-insert
        :desc "Open roam graph" "g" #'org-life-roam-ui-open
        :desc "Open local roam graph" "l" #'org-life-roam-ui-open-local
        :desc "Toggle roam graph mode" "u" #'org-life-roam-ui-mode)
       (:prefix ("n" . "denote")
        :desc "Create denote note" "n" #'denote
        :desc "Open or create denote note" "o" #'denote-open-or-create
        :desc "Link or create denote note" "i" #'denote-link-or-create
        :desc "Show denote backlinks" "b" #'denote-backlinks
        :desc "Rename denote file" "r" #'denote-rename-file)
       (:prefix ("d" . "dashboard")
        :desc "Open OrgLife dashboard" "o" #'org-life-dashboard-open
        :desc "Refresh OrgLife dashboard" "r" #'org-life-dashboard-refresh)))

(provide 'config-org-integration)
;;; config-org-integration.el ends here
