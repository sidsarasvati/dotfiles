# Emacs Configuration Notes

## Overview
This folder contains Sid's Emacs configuration, recently converted to a literate
programming style using org-mode. The configuration has been maintained since
2012 and is designed to work on Emacs 30+ (tree-sitter support requires 29+).

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
- Set default frame size with 9:16 aspect ratio (100 columns × 56 lines)
- Added tree-sitter support for TypeScript/React (Dec 2025):
  - Grammars: tsx, typescript, javascript, json, css
  - Auto-install on first launch (~30sec compile)
  - .ts/.tsx/.js/.jsx/.json/.css now use tree-sitter modes

## Technical Details
1. **Path Resolution**: Uses file-truename to resolve the actual path of the .emacs symlink
2. **Package Management**: Standard package.el with MELPA repositories
3. **Theme Loading**: Loaded early in .emacs for immediate visual feedback
4. **File Organization**:
   - Backup files → ~/.emacs.d/backups/
   - Auto-save files → ~/.emacs.d/auto-saves/
   - Lock files → Disabled entirely

## Usage Instructions
1. The configuration is installed via `./install.sh` from the dotfiles repository
2. Launch GUI Emacs with `em` command (requires symlink: `ln -s /Applications/Emacs.app/Contents/MacOS/Emacs ~/.local/bin/emacs`)
3. Launch terminal Emacs with `emacs -nw` command
4. Edit configuration by opening `config.org` in Emacs

## Troubleshooting
- If changes to config.org don't take effect, check the *Messages* buffer for errors
- If Emacs fails to start, use `emacs --debug-init` to see errors during startup
- Theme issues: Check if dracula-theme is installed with `M-x package-list-packages`

## Key Packages
- dracula-theme: Dark color theme
- smart-mode-line: Improved mode line (conditionally loaded if available)
- flycheck: Syntax checking
- web-mode: HTML templates
- go-mode: Go programming language support
- org-mode: Organization, notes, TODOs
- magit: Git interface for Emacs
- tree-sitter (built-in): Modern syntax for TypeScript/React/JS/CSS

## Roadmap
- LSP integration (eglot) for TypeScript/React intellisense
