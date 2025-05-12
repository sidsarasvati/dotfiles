# Zsh Configuration

## Overview
This directory contains a standalone zsh configuration designed with a literate programming approach. The configuration is organized in a modular fashion with separate files for different aspects of the shell environment.

## Key Files
- `.zshrc`: Main configuration file with detailed comments for each section
- `env.zsh`: Environment variables, PATH configuration, and tool-specific settings
- `aliases.zsh`: Command shortcuts organized by category
- `functions/utility.zsh`: Useful shell functions for everyday tasks

## Prompt
The configuration includes multiple prompt styles that can be switched by editing `.zshrc`:
1. **Developer Pro** (default): Two-line prompt with path and git branch
2. **Minimal with Lambda**: Clean prompt with lambda symbol
3. **AI-Coder Vibe**: Modern style with minimal symbols
4. **Informativity + Style**: Includes command execution timing
5. **Classic with Lambda**: Single-line prompt with lambda

## Git Integration
- The prompt includes custom git branch detection
- Shows branch name in parentheses only when in a git repository
- Uses a custom function `git_prompt_info()` that properly handles edge cases

## Installation
- The main `install.sh` script creates a `.zshenv` file in the home directory
- This file points to this zsh configuration using the absolute path to the dotfiles location
- No external dependencies needed (no stow or other symlink management)
- Smart detection of existing configurations with proper backup options
- Automatic detection and guidance for oh-my-zsh users

## Customization
- Machine-specific settings and secrets should be added to `local.zsh` (gitignored)
- Use `local.zsh.example` as a template for your private tokens
- Additional functions can be added to `functions/` directory
- Prompt styles can be switched by commenting/uncommenting in `.zshrc`

## oh-my-zsh Migration
If migrating from oh-my-zsh, this configuration replaces it entirely. The following approaches are possible:

1. Completely replace oh-my-zsh (recommended)
2. Use both by modifying `.zshenv` to source both configurations
3. Move your oh-my-zsh customizations to this structure

To remove oh-my-zsh completely, run: `uninstall_oh_my_zsh`

## Keybindings
The configuration uses Emacs-style keybindings with common shortcuts like:
- Word navigation with Alt+F/B
- Line navigation with Ctrl+A/E
- Word deletion with Alt+Backspace and Alt+D
- History search with Ctrl+R and Ctrl+P/N

Path components are treated as separate words for navigation and deletion. This means:
- Alt+Backspace deletes just the last path component (e.g., in `~/Code/sid/dotfiles`, it deletes only `dotfiles`)
- Alt+F/B jumps between path components
This behavior is implemented via `select-word-style bash`, which makes zsh use bash-style word definitions.