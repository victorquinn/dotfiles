;;; gitignore-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "gitignore-mode" "gitignore-mode.el" (21262
;;;;;;  7181 0 0))
;;; Generated autoloads from gitignore-mode.el

(autoload 'gitignore-mode "gitignore-mode" "\
A major mode for editing .gitconfig files.

\(fn)" t nil)

(setq auto-mode-alist (append '(("/\\.gitignore\\'" . gitignore-mode) ("/\\.git/info/exclude\\'" . gitignore-mode)) auto-mode-alist))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gitignore-mode-autoloads.el ends here
