;;; coffee-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
<<<<<<< HEAD

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "coffee-mode" "coffee-mode.el" (0 0 0 0))
=======
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "coffee-mode" "coffee-mode.el" (22844 4713
;;;;;;  824932 205000))
>>>>>>> Latest changes, remove a lot of cruft, fix emacs indent to 2
;;; Generated autoloads from coffee-mode.el

(autoload 'coffee-mode "coffee-mode" "\
Major mode for editing CoffeeScript.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.iced\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("Cakefile\\'" . coffee-mode))

(add-to-list 'auto-mode-alist '("\\.cson\\'" . coffee-mode))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

<<<<<<< HEAD
(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "coffee-mode" '("coffee-" "iced-coffee-cs-keywords" "js2coffee-command")))

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
;;; coffee-mode-autoloads.el ends here
