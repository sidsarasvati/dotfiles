# Emacs Configuration Notes

## Overview
This folder contains Sid's Emacs configuration, recently converted to a literate programming style using org-mode. The configuration has been maintained since 2012 and is designed to work on modern Emacs (version 26+).

## Key Files
- **config.org**: The main configuration file in literate programming style
- **.emacs**: A minimal bootstrap file that loads config.org
- **.emacs.d/**: Directory containing packages, customizations, and extensions

## Configuration Structure
- **Bootstrap (.emacs)**: Minimal setup to load package system, ensure theme and org-mode are available, then load config.org
- **Main Config (config.org)**: Organized into logical sections with documentation

## Recent Changes and Fixes
- Converted from a monolithic .emacs file to literate programming style
- Centralized backup and auto-save files in ~/.emacs.d/backups and ~/.emacs.d/auto-saves
- Fixed config.org path resolution using file-truename for symlink following
- Ensured Dracula theme loads early for consistent appearance
- Updated .gitignore to exclude Emacs temporary files
- Added improved terminal/GUI launching commands in zsh aliases
- Enabled show-paren-mode for highlighting matching parentheses

## Technical Details
1. **Path Resolution**: Uses file-truename to resolve the actual path of the .emacs symlink
2. **Package Management**: Standard package.el with MELPA repositories
3. **Theme Loading**: Loaded early in .emacs for immediate visual feedback
4. **File Organization**:
   - Backup files → ~/.emacs.d/backups/
   - Auto-save files → ~/.emacs.d/auto-saves/
   - Lock files → Disabled entirely

## Usage Instructions
1. The configuration is installed via GNU Stow from the dotfiles repository
2. Launch GUI Emacs with `em` command
3. Launch terminal Emacs with `emacs` command
4. Edit configuration by opening `config.org` in Emacs

## Troubleshooting
- If changes to config.org don't take effect, check the *Messages* buffer for errors
- If Emacs fails to start, use `emacs --debug-init` to see errors during startup
- Theme issues: Check if dracula-theme is installed with `M-x package-list-packages`

## Key Packages
- dracula-theme: Dark color theme
- smart-mode-line: Improved mode line (conditionally loaded if available)
- flycheck: Syntax checking
- web-mode: Web development (HTML/CSS/JS)
- go-mode: Go programming language support
- org-mode: Organization, notes, TODOs
- magit: Git interface for Emacs