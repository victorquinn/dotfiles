;;; js-doc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
<<<<<<< HEAD

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js-doc" "js-doc.el" (0 0 0 0))
=======
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "js-doc" "js-doc.el" (22548 52533 54324 470000))
>>>>>>> Latest changes, remove a lot of cruft, fix emacs indent to 2
;;; Generated autoloads from js-doc.el

(autoload 'js-doc-insert-file-doc "js-doc" "\
Insert specified-style comment top of the file

\(fn)" t nil)

(autoload 'js-doc-insert-function-doc "js-doc" "\
Insert JsDoc style comment of the function
The comment style can be custimized via `customize-group js-doc'

\(fn)" t nil)

(autoload 'js-doc-insert-function-doc-snippet "js-doc" "\
Insert JsDoc style comment of the function with yasnippet.

\(fn)" t nil)

(autoload 'js-doc-insert-tag "js-doc" "\
Insert a JsDoc tag interactively.

\(fn)" t nil)

(autoload 'js-doc-describe-tag "js-doc" "\
Describe the JsDoc tag

\(fn)" t nil)

<<<<<<< HEAD
(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js-doc" '("js-doc-")))

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
;;; js-doc-autoloads.el ends here
