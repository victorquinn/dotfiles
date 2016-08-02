;;; evernote-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evernote-mode-pkg" "evernote-mode-pkg.el"
;;;;;;  (21705 37658 0 0))
;;; Generated autoloads from evernote-mode-pkg.el
 (require 'evernote-mode)
 (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
 (global-set-key "\C-cec" 'evernote-create-note)
 (global-set-key "\C-ceo" 'evernote-open-note)
 (global-set-key "\C-ces" 'evernote-search-notes)
 (global-set-key "\C-ceS" 'evernote-do-saved-search)
 (global-set-key "\C-cew" 'evernote-write-note)
 (global-set-key "\C-cep" 'evernote-post-region)
 (global-set-key "\C-ceb" 'evernote-browser)

;;;***

;;;### (autoloads nil nil ("evernote-mode.el") (21705 37660 157227
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evernote-mode-autoloads.el ends here
