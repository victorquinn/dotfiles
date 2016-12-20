;; ===================
;; emacs-wide settings
;; ===================


;; Include Paths
;; Load elpa packages
(add-to-list 'load-path "~/.emacs.d/elpa")
;; Load miscellaneous (non-package-manager) .el files
(add-to-list 'load-path "~/.emacs.d/misc")

;; default "history" length is just 32, apparently! Jeez.
(setq comint-input-ring-size 65536)

;; Inhibit startup message
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

;; More pretty on Mac
(setq ns-use-srgb-colorspace t)

;; Place all backup files in one directory.
(setq backup-directory-alist `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms `((".*" "~/.saves" t)))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 10
      kept-old-versions 2
      version-control t)
(setq make-backup-files t)

;; Purge backup files more than a week old.
;; (message "Deleting old backup files...")
;; (let ((week (* 60 60 24 7))
;;       (current (float-time (current-time))))
;;   (dolist (file (directory-files temporary-file-directory t))
;;     (when (and (backup-file-name-p file)
;;                (> (- current (float-time (fifth (file-attributes file))))
;;                   week))
;;       (message file)
;;       (delete-file file))))

(setq auto-save-default nil)
(setq make-backup-files nil)

;; Inhibit .# lockfiles. This was breaking lint and so on. Nice feature, but
;; wish you could specify a different directory for them.
(setq create-lockfiles nil)

;; Lose the toolbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Display visited file's path in the frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


(setq show-trailing-whitespace t)

(setq tab-width 4)

;; Mic-Paren - better paren matching
;; (require 'mic-paren)
;; (paren-activate)


;; (require 'color-theme-buffer-local)
;; (add-hook 'eshell-mode-hook
;;           (lambda nil (color-theme-buffer-local 'color-theme-solarized-dark (current-buffer))))

(global-set-key [f1] 'eshell)

;; =====
;; Fonts
;; =====

(defun font-candidate (&rest fonts)
  "Find the first font that exists"
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

(set-face-attribute 'default nil :font (font-candidate '"Hack" "Menlo" "Inconsolata" "CosmicSansNeueMono" "Source Code Pro") :height 140)

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
             '("milkbox" . "http://melpa.milkbox.net/packages/" ) t)

(package-initialize)

;; Javascript Mode
(autoload 'js2-mode "js2" nil t)
;; (js2r-add-keybindings-with-prefix "C-c C-m")

;; Now defaulting to web-mode because I'm mostly editing jsx these days
(add-to-list 'auto-mode-alist '("\\.\\(js\\|jsx\\)$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (web-mode-set-content-type "jsx")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("cdd26fa6a8c6706c9009db659d2dffd7f4b0350f9cc94e5df657fa295fffec71" "345f8f92edc3508574c61850b98a2e0a7a3f5ba3bb9ed03a50f6e41546fe2de0" "a3132bd39a977ddde4c002f8bd0ef181414c3fbe9228e3643b999491192680ad" "b79104a19e95f10698badb711bd4ab25565af3ffcf18fa7d3c7db4de7d759ac8" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "94ba29363bfb7e06105f68d72b268f85981f7fba2ddef89331660033101eb5e5" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" "5e6c2e2116c7a72ae0668390f92504fd0b77524cedd387582648b1aa1c582f59" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "5f81724ae9625b1286a6ef627cefa8b01ccab8e37496375dea2ab4210687300a" default)))
 '(display-time-mode t)
 '(js2-basic-offset 4)
 '(js2-highlight-level 3)
 '(js2-include-node-externs t)
 '(js2-indent-switch-body t)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-function-call ((t (:inherit default))))
 '(setnu-line-number-face ((t (:inherit default))) t))

;; drupal-mode!
;; (require 'drupal-mode)
;; (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\|inc\\)$" . drupal-mode))
;; (add-to-list 'auto-mode-alist '("\\.\\(php\\)$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))

;; Magit
;; (require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

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

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.\\(jsx\\|html\\)$" . web-mode))

;; node.js
(require 'nodejs-repl)

;; (load-theme 'wombat t)

;; Load the seti theme
;; (load-theme 'seti t)
(load-theme 'farmhouse-dark t)

;; Add key bindings for helm
(global-set-key (kbd "C-x b") 'helm-mini)


;; Autocomplete
(require 'popup)
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Supercollider
;; (add-to-list 'load-path "~/.emacs.d/scel/")
;; (require 'sclang)

; (zone-when-idle 120)

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

;; Load Smex for 'M-x' autocomplete
(smex-initialize)
;; Rebind 'M-x' to smex
(global-set-key (kbd "M-x") 'smex)
;; Bind classic 'M-x' to 'C-c C-c M-x'
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Litable mode
;; (load "litable")
;; (add-to-list 'auto-mode-alist '("\\.\\(el\\)$" . litable-mode))

;; Project Explorer
;; (load "project-explorer")

;; Multiple Cursor mode
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Anzu mode https://github.com/syohex/emacs-anzu
(require 'anzu)
(global-anzu-mode +1)

(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(require 'js-comint)
(setq inferior-js-program-command "node")
(setenv "NODE_NO_READLINE" "1")
;; not "node-repl"
;; Use your favorited js mode here:
(add-hook 'js2-mode-hook '(lambda ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go)))


;; Git gutter fixes for OS X
;; Fix spacing on added/deleted characters
(setq git-gutter:added-sign "+ ")
(setq git-gutter:deleted-sign "- ")
(setq git-gutter:modified-sign "= ")

;; Set update threshold so as not to impact performance
(setq git-gutter:update-threshold 2)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

;; Turn git gutter on globally
(global-git-gutter-mode +1)

;; Multi-term
(setq multi-term-program "/usr/local/bin/zsh")
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)))
(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (autopair-mode -1)))
(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'multi-term)

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-raw)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-," . term-send-input)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'multi-term)

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; Stop scss mode from trying to compile on save
(setq scss-compile-at-save nil)

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

(setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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

(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(put 'upcase-region 'disabled nil)
