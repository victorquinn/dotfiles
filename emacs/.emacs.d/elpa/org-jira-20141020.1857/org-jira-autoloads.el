;;; org-jira-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "org-jira" "org-jira.el" (21593 12900 0 0))
;;; Generated autoloads from org-jira.el

(autoload 'org-jira-mode "org-jira" "\
Toggle org-jira mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org-jira-entry-mode-map}

Entry to this mode calls the value of `org-jira-mode-hook'.

\(fn &optional ARG)" t nil)

(autoload 'org-jira-get-projects "org-jira" "\
Get list of projects.

\(fn)" t nil)

(autoload 'org-jira-get-issues-headonly "org-jira" "\
Get list of issues assigned to you and unresolved, head
only. With a prefix argument, allow you to customize the jql. See `org-jira-get-issue-list'

\(fn ISSUES)" t nil)

(autoload 'org-jira-get-issues "org-jira" "\
Get list of issues. Default is get unfinished issues assigned
to you, but you can customize jql with a prefix argument. See
`org-jira-get-issue-list'

\(fn ISSUES)" t nil)

(autoload 'org-jira-update-comment "org-jira" "\
update a comment for the current issue

\(fn)" t nil)

(autoload 'org-jira-copy-current-issue-key "org-jira" "\
Copy the current issue's key into clipboard

\(fn)" t nil)

(autoload 'org-jira-update-issue "org-jira" "\
update an issue

\(fn)" t nil)

(autoload 'org-jira-todo-to-jira "org-jira" "\
convert an ordinary todo item to a jira ticket

\(fn)" t nil)

(autoload 'org-jira-get-subtasks "org-jira" "\
get subtasks for the current issue

\(fn)" t nil)

(autoload 'org-jira-create-issue "org-jira" "\
create an issue

\(fn PROJECT TYPE SUMMARY DESCRIPTION)" t nil)

(autoload 'org-jira-create-subtask "org-jira" "\
create an subtask issue

\(fn PROJECT TYPE SUMMARY DESCRIPTION)" t nil)

(autoload 'org-jira-refresh-issue "org-jira" "\
Refresh issue from jira to org

\(fn)" t nil)

(autoload 'org-jira-progress-issue "org-jira" "\
Progress issue workflow

\(fn)" t nil)

(autoload 'org-jira-browse-issue "org-jira" "\
Open the current issue in external browser.

\(fn)" t nil)

(autoload 'org-jira-get-issues-from-filter "org-jira" "\
Get issues from filter which are jql created and saved on the
server side. Provide this command in case some users are not able
to use client side jql (maybe because of Jira server version?).

\(fn FILTER)" t nil)

(autoload 'org-jira-get-issues-from-filter-headonly "org-jira" "\
Get issues *head only* from saved filter. See `org-jira-get-issues-from-filter'

\(fn FILTER)" t nil)

;;;***

;;;### (autoloads nil nil ("jiralib.el" "org-jira-pkg.el") (21593
;;;;;;  12900 469875 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-jira-autoloads.el ends here
