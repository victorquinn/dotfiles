;;; keyfreq-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
<<<<<<< HEAD

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "keyfreq" "keyfreq.el" (0 0 0 0))
=======
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "keyfreq" "keyfreq.el" (22548 52526 54476 801000))
>>>>>>> Latest changes, remove a lot of cruft, fix emacs indent to 2
;;; Generated autoloads from keyfreq.el

(defvar keyfreq-mode nil "\
Non-nil if Keyfreq mode is enabled.
See the `keyfreq-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keyfreq-mode'.")

(custom-autoload 'keyfreq-mode "keyfreq" nil)

(autoload 'keyfreq-mode "keyfreq" "\
Keyfreq mode records number of times each command was
called making it possible to access usage statistics through
various keyfreq-* functions.

\(fn &optional ARG)" t nil)

(defvar keyfreq-autosave-mode nil "\
Non-nil if Keyfreq-Autosave mode is enabled.
See the `keyfreq-autosave-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keyfreq-autosave-mode'.")

(custom-autoload 'keyfreq-autosave-mode "keyfreq" nil)

(autoload 'keyfreq-autosave-mode "keyfreq" "\
Keyfreq Autosave mode automatically saves
`keyfreq-table' every `keyfreq-autosave-timeout' seconds
and when emacs is killed.

\(fn &optional ARG)" t nil)

(autoload 'keyfreq-save-now "keyfreq" "\
Save keyfreq data now.

\(fn)" t nil)

<<<<<<< HEAD
(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "keyfreq" '("keyfreq-")))

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
;;; keyfreq-autoloads.el ends here
