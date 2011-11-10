Victor Quinn's Dotfiles
=======================

My relevant dotfiles. Idea being that on any new system I can just pull these off of github and be up and running quickly. Public repo in case anyone is interested.

Since git will not allow you to clone directly into an existing directory (and on any new system, your home directory already exists), I clone this repository into ~/dotfiles with:

    git clone git://github.com/victorquinn/dotfiles.git ~/dotfiles

Then run the movefiles script to copy all dotfiles/directories into my home directory. Run:

    . ~/dotfiles/movefiles.sh


Quasi-Dependencies (not really dependencies per se, but little utilities I use which should be installed to make this group of dotfiles work, consistently)

Magit
% brew install magit

Pygmentize
% easy_install Pygments
