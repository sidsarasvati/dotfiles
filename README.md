# Dotfiles


## Update 2024
- bash config ported to zshell in bashrc_port.zsh
- we only maintain mac setup for now
- next up, we shall write cleanup install.sh as inpired by - https://gist.github.com/sidsarasvati/32834aa0eafd69339f5727621473c4c7



Dotfiles ========

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
- git clone https://github.com/sidsarasvati/dotfiles.git dotfiles
- cd dotfiles
- ./install.sh


# Mac Setting

## Architecture Detection

This repository automatically detects Apple Silicon vs Intel Mac architectures and configures Homebrew paths accordingly:
- Apple Silicon Macs: Uses `/opt/homebrew` as the Homebrew prefix
- Intel Macs: Uses `/usr/local` as the Homebrew prefix

This detection happens in several key files (`install.sh`, `zsh/env.zsh`, and `bash/bash_port.zsh`) to ensure portability across Mac architectures.

## iTerm2

### To use Left Cmd key as meta in the terminal
- Preferences->Profile->Keys : Change Left Alt to Esc+
- Preferences->Keys->Remap Modifier : Map Left Cmd to Left Alt 

### Use intellij mono font; brew install it

### Use Tago Dark Theme



## Moving to Zsh 

- Aliases in zsh work just like aliases in bash. 
- As with aliases, functions in your zsh configuration will work just as they did in bash.


Contributing
============

Shoot me a pull request. I am open to PRs - especially if it fixes cross-platform issues. 
