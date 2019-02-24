;;; jade-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
<<<<<<< HEAD

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jade-mode" "jade-mode.el" (0 0 0 0))
=======
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "jade-mode" "jade-mode.el" (22548 52533 450982
;;;;;;  503000))
>>>>>>> Latest changes, remove a lot of cruft, fix emacs indent to 2
;;; Generated autoloads from jade-mode.el

(autoload 'jade-mode "jade-mode" "\
Major mode for editing jade node.js templates

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

(add-to-list 'auto-mode-alist '("\\.pug\\'" . jade-mode))

<<<<<<< HEAD
(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jade-mode" '("jade-")))

=======
>>>>>>> Latest changes, remove a lot of cruft, fix emacs indent to 2
;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
<<<<<<< HEAD
;; coding: utf-8
=======
>>>>>>> Latest changes, remove a lot of cruft, fix emacs indent to 2
;; End:
;;; jade-mode-autoloads.el ends here
