List the all lines to another buffer, which is able to squeeze
by any words you input. At the same time, the original buffer's
cursor is jumping line to line according to moving up and down
the list.

Example config
----------------------------------------------------------------
helm from https://github.com/emacs-helm/helm
(require 'helm-config)
(helm-mode 1)

Locate the helm-swoop folder to your path
This line is unnecessary if you get this program from MELPA
(add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")

(require 'helm-swoop)

Change keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)

When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
----------------------------------------------------------------

Helm Swoop Edit
While doing helm-swoop, press keybind [C-c C-e] to move to edit buffer.
Edit the list and apply by [C-x C-s]. If you'd like to cancel, [C-c C-g]
