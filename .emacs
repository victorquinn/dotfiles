;; ===================
;; emacs-wide settings
;; ===================


;; Include Paths
(add-to-list 'load-path "~/.emacs-dir")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.el4r")

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
(load-library "~/.emacs-dir/rainbow-mode.el")

;; Ido
;; (load-library "~/.emacs-dir/ido.el")
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t) ;; enable fuzzy matching

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

;; Load CEDET
;;(load-file "~/.emacs-dir/cedet/common/cedet.el")
;;(global-ede-mode t)

;; Lose the toolbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; =====
;; Fonts
;; =====

(set-face-attribute 'default nil :font "Source Code Pro")


;; =================
;; Modes and plugins
;; =================

;; Javascript Mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(js2-highlight-level 3))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(setnu-line-number-face ((t (:inherit default))) t))

;; drupal-mode!
(require 'drupal-mode)
(add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\|inc\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("\\.\\(php\\)$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))

;; Magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; HAML
(require 'haml-mode)

;; LESS CSS mode
(require 'less-mode)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\)$" . yaml-mode))

;; Hippie Expand
(global-set-key "\M- " 'hippie-expand)

;; Markdown Mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)$" . markdown-mode))

;; node.js
(require 'nodejs-mode)

;; Require the new emacs package manager
(require 'package)

;; Add the marmalade alternate emacs package repo
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/" ) t)

;; Load the wombat theme
(load-theme 'wombat t)

;; Add key binding for anything
(global-set-key (kbd "C-x b") 'anything)

;; Autocomplete
(require 'popup)
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Org Mode Settings

(setq org-log-done 'time)
;; Add a note when closed
(setq org-log-done 'note)

;; Supercollider
(add-to-list 'load-path "~/.emacs.d/scel/")
(require 'sclang)

(custom-set-variables
'(sclang-auto-scroll-post-buffer t)
'(sclang-eval-line-forward nil)
'(sclang-help-path (quote ("/Applications/SuperCollider/Help")))
'(sclang-runtime-directory "~/sclang/"))


;; Beginning of the el4r block:
;; RCtool generated this block automatically. DO NOT MODIFY this block!
(add-to-list 'load-path "/Users/vquinn/.rvm/rubies/ruby-1.9.3-p194/share/emacs/site-lisp")
(require 'el4r)
(el4r-boot)
;; End of the el4r block.
;; User-setting area is below this line.
