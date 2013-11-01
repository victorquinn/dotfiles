;;; color-theme-buffer-local-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "color-theme-buffer-local" "color-theme-buffer-local.el"
;;;;;;  (21106 62420 0 0))
;;; Generated autoloads from color-theme-buffer-local.el

(autoload 'color-theme-buffer-local "color-theme-buffer-local" "\
Install the color-theme defined by THEME on BUFFER.

   THEME must be a symbol whose value as a function calls
   `color-theme-install' to install a theme.

   BUFFER defaults to the current buffer if not explicitly given.

\(fn THEME &optional BUFFER)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; color-theme-buffer-local-autoloads.el ends here
