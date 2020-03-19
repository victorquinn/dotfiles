
;; Declared Emacs Bankruptcy on April 9, 2019
;; Everything from here on is started then :) Go through git history for
;; more past changes

;; ==================
;; Package management
;; ==================
;; Note, this has to come first which breaks alphabetization but is needed for things that come below

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Place all backup files in one directory.
(setq backup-directory-alist `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.saves" t)))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 10
      kept-old-versions 2
      version-control t)
(setq make-backup-files t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-window ag handlebars-mode typescript-mode multiple-cursors markdown-toc markdown-mode graphql graphql-mode yaml-mode terraform-mode farmhouse-theme))))
 '(package-selected-packages (quote (terraform-mode json-mode farmhouse-theme typescript-mode)))


;; Inhibit .# lockfiles
(setq create-lockfiles nil)

;; ====
;; Font
;; ====

(add-to-list 'default-frame-alist '(font . "Operator Mono"))
(set-face-attribute 'default nil :font "Operator Mono" :height 120)

;; ===========
;; Keybindings
;; ===========

;; Helm
(global-set-key (kbd "C-x b") 'helm-mini)

;; Ace window
(global-set-key (kbd "C-x o") 'ace-window)

;; ========
;; Org Mode
;; ========

(setq org-log-done 'time)
;; Add a note when closed
(setq org-log-done 'note)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
'((sequence "TODO" "IN PROGRESS" "|" "DONE" "DEFERRED")))

(setq org-todo-keyword-faces
'(("TODO" . "pink") ("IN PROGRESS" . "yellow") ("DONE") ("DEFERRED" . "orange")))

(setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; (define-key org-mode-map "\M-q" 'toggle-truncate-lines)

;; ================
;; Multiple Cursors
;; ================

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ====
;; PATH
;; ====
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; ==========
;; Projectile
;; ==========

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/Development/"))

;; ======
;; Visual
;; ======

;; Hide top frame on macOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

;; Hide toolbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Load theme
(load-theme 'farmhouse-dark t)

;; Turn on column numbers
(setq column-number-mode t)

;; Indent by 2 spaces
(setq tab-width 2)
(setq js-indent-level 2)
(setq typescript-indent-level 2)

;; Turn off tabs
(setq-default indent-tabs-mode nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Wrap text in all text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
