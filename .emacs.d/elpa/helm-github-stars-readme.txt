helm-github-stars provides capabilities to show and open
starred repository from github.

Usage:
 Copy helm-github-stars.el in your load-path and put this in your ~/.emacs.d/init.el:
 (require 'helm-github-stars)
 ;; Setup your github username:
 (setq helm-github-stars-username "USERNAME")

 Type M-x helm-github-stars to show starred repositories.

At the first execution of ~helm-github-stars~, list of repositories is
fetched from github and saved into a cache file.
Default cache location: ~$HOME/.emacs.d/hgs-cache~.
To refresh cache and open helm interface run ~helm-github-stars-fetch~.

You can customize cache file path:
(setq helm-github-stars-cache-file "/cache/path")
