Dotfiles
========

This is my cross-platform development environment setup config files. I
am an avid 'emacs' user, so you'll like to look at my emacs config to
see if there is anything you may find userful. Other than that, I
don't user 'Oh-my-zsh', but rather have my bash prompt modified as per
my needs using custom scripts.

I use GNU stow to manage the file/folder structure and make the dotfile installable. Read http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html, to understand how it works, and why it's awesome! 

As of now this works on, 
 - Windows(with cygwin)
 - Mac OSx
 - Linux 

Note: For Windows, I recommend setting C:\cygwin_home as %HOME%
enviroment variable, this keeps the configs away from cygwin binaries.

Installation
============

Simply git clone the repo anywhere you like (I use ~/dotfiles), install GNU stow, and run the install.sh script from there.
It won't overwrite existing files, so move those out of the way first. GNU stow gives a nice descriptive error, so re-rerun install.sh script as you keep backing up the files. 

Example:
git clone https://github.com/sidsarasvati/dotfiles.git dotfiles
cd dotfiles
./install.sh
