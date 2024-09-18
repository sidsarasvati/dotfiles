# Dotfiles

## Introduction

This repository contains my cross-platform development environment setup configuration files. I am an avid Emacs user, so you'll like to look at my Emacs config to see if there is anything you may find useful. Other than that, I don't use 'Oh-my-zsh', but rather have my bash prompt modified as per my needs using custom scripts.

I use GNU stow to manage the file/folder structure and make the dotfile installable. Read [this article](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html) to understand how it works, and why it's awesome!

As of now, this works on:
- Windows (with Cygwin)
- Mac OS X
- Linux

Note: For Windows, I recommend setting `C:\cygwin_home` as `%HOME%` environment variable, this keeps the configs away from Cygwin binaries.

## Installation

Simply git clone the repo anywhere you like (I use `~/dotfiles`), install GNU stow, and run the `install.sh` script from there. It won't overwrite existing files, so move those out of the way first. GNU stow gives a nice descriptive error, so re-run the `install.sh` script as you keep backing up the files.

Example:
```sh
git clone https://github.com/sidsarasvati/dotfiles.git dotfiles
cd dotfiles
./install.sh
```

## Mac Setting

### iTerm2

#### To use Left Cmd key as meta in the terminal
- Preferences -> Profile -> Keys: Change Left Alt to Esc+
- Preferences -> Keys -> Remap Modifier: Map Left Cmd to Left Alt

#### Use intellij mono font; brew install it

#### Use Tago Dark Theme

## Moving to Zsh

- Aliases in zsh work just like aliases in bash.
- As with aliases, functions in your zsh configuration will work just as they did in bash.

## Emacs Configuration

The Emacs configuration code is documented in a literate programming style. The configuration files are located in the `emacs` directory. The main configuration file is `emacs/.emacs`, and additional configuration files are located in the `emacs/.emacs.d/elisp` directory.

### Features

- Package management and auto installation
- Custom load paths
- Global settings and preferences
- Mode-specific configurations
- Key bindings and convenience functions
- Hooks and custom functions

### Usage

To use the Emacs configuration, simply copy the configuration files to your Emacs configuration directory (usually `~/.emacs.d`). You can also use GNU stow to manage the configuration files.

## Contributing

Shoot me a pull request. I am open to PRs - especially if it fixes cross-platform issues.
