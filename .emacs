;; ===================
;; emacs-wide settings
;; ===================


;; Include Paths
(add-to-list 'load-path "~/.emacs.d/elpa")

;; default "history" length is just 32, apparently! Jeez.
(setq comint-input-ring-size 65536)

;; Turn off stupid auto-save mode. Good idea, but lags the program to hell constantly.
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)

;; Turn off tabs
(setq-default indent-tabs-mode nil)

;; Turn on column numbers
(setq column-number-mode t)

;; Show the current time in the bar
(display-time)

(put 'downcase-region 'disabled nil)

;; Color parenethesis matching
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(setq blink-matching-paren-distance 51200)

;; Make it so delete key works!
;; (global-set-key "\C-h" 'backward-delete-char)

;; Turn tramp password expiry off
(setq tramp-default-method "sshx")

;; Allow y or n instead of having to type yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Add line numbers
(global-set-key "\C-x\C-n" 'setnu-mode)

;; Add rainbow mode
;; (load-library "~/.emacs-dir/rainbow-mode.el")

;; Place all backup files in one directory.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Purge backup files more than a week old.
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message file)
      (delete-file file))))

;; Lose the toolbars
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Display visited file's path in the frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Mic-Paren - better paren matching
;; (require 'mic-paren)
;; (paren-activate)


;; (require 'color-theme-buffer-local)
;; (add-hook 'term-mode
;;           (lambda nil (color-theme-buffer-local 'color-theme-solarized (current-buffer))))

;; =====
;; Fonts
;; =====

(set-face-attribute 'default nil :font "Menlo")


;; =================
;; Modes and plugins
;; =================

;; Require the new emacs package manager
(require 'package)

;; Add the marmalade alternate emacs package repo
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/" ) t)

;; Add milkbox
(add-to-list 'package-archives
             '("milkbox" . "http://melpa.milkbox.net/" ) t)

(package-initialize)

;; Javascript Mode
;; (autoload 'js2-mode "js2" nil t)

;; Now defaulting to js-mode rather than js2-mode because js2-mode was too strict
;; and often buggy for me.
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
;; (custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  '(js2-basic-offset 4)
;;  '(js2-highlight-level 3))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(setnu-line-number-face ((t (:inherit default))) t))

;; drupal-mode!
;; (require 'drupal-mode)
;; (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\|inc\\)$" . drupal-mode))
;; (add-to-list 'auto-mode-alist '("\\.\\(php\\)$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))

;; Magit
;; (require 'magit)
;; (global-set-key (kbd "C-x g") 'magit-status)

;; HAML
;; (require 'haml-mode)

;; Jade mode
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.\\(jade\\|jade\\)$" . jade-mode))

;; LESS CSS mode
(require 'less-css-mode)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\)$" . yaml-mode))

;; Rainbow Delimiters mode
(require 'rainbow-delimiters)

;; Hippie Expand
(global-set-key "\M- " 'hippie-expand)

;; Markdown Mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;; node.js
(require 'nodejs-repl)

;; Load the wombat theme
(load-theme 'wombat t)

;; Add key binding for anything
(global-set-key (kbd "C-x b") 'anything)

;; Autocomplete
(require 'popup)
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Supercollider
;; (add-to-list 'load-path "~/.emacs.d/scel/")
;; (require 'sclang)

;; (custom-set-variables
;; '(sclang-auto-scroll-post-buffer t)
;; '(sclang-eval-line-forward nil)
;; '(sclang-help-path (quote ("/Applications/SuperCollider/Help")))
;; '(sclang-runtime-directory "~/sclang/"))

;; (require 'multi-mode)
;; (require 'mustache-mode)

;; Erlang
;;(require 'erlang-mode)

;; DTRT mode, to match indent style
;; (require 'dtrt-indent)
;; (dtrt-indent-mode t)

;; JSON
(require 'json-mode)

;;
(require 'handlebars-mode)
(add-to-list 'auto-mode-alist '("\\.\\(hbs\\|handlebars\\)$" . handlebars-mode))

;; =================
;; Org Mode Settings
;; =================

(setq org-log-done 'time)
;; Add a note when closed
(setq org-log-done 'note)
(global-set-key (kbd "C-c c") 'org-capture)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)))

(setq org-todo-keywords
'((sequence "TODO" "IN PROGRESS" "|" "DONE" "DEFERRED")))

(setq org-todo-keyword-faces
'(("TODO" . "pink") ("IN PROGRESS" . "yellow") ("DONE") ("DEFERRED" . "orange")))

;; =====================
;; Other Misc. Functions
;; =====================

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta shift up)] 'move-line-up)
(global-set-key [(meta shift down)] 'move-line-down)

;; ================
;; eshell Additions
;; ================

(setenv "PATH"
  (concat
   "/usr/local/bin" ":"
   (getenv "PATH")
  )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("5f81724ae9625b1286a6ef627cefa8b01ccab8e37496375dea2ab4210687300a" default)))
 '(display-time-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
