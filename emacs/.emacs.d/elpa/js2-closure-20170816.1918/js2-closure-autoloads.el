;;; js2-closure-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js2-closure" "js2-closure.el" (0 0 0 0))
;;; Generated autoloads from js2-closure.el

(autoload 'js2-closure-fix "js2-closure" "\
Fix the `goog.require` statements in the current buffer.
This function assumes that all the requires are in one place and
sorted, without indentation or blank lines.  If you don't have
any requires, they'll be added after your provide statements.  If
you don't have those, then this routine will fail.

Effort was also made to avoid needlessly modifying the buffer,
since syntax coloring might take some time to kick back in.

This will automatically load `js2-closure-provides-file' into
memory if it was modified or not yet loaded.

\(fn)" t nil)

(autoload 'js2-closure-save-hook "js2-closure" "\
Global save hook to invoke `js2-closure-fix' if in `js2-mode'.
To use this feature, add it to `before-save-hook'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js2-closure" '("js2-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js2-closure-autoloads.el ends here
