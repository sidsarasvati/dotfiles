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
- This file points to this zsh configuration using the actual dotfiles location
- No need for stow or other symlink management

## Customization
- Machine-specific settings can be added to `local.zsh` (gitignored)
- Additional functions can be added to `functions/` directory
- Prompt styles can be switched by commenting/uncommenting in `.zshrc`

## Keybindings
The configuration uses Emacs-style keybindings with common shortcuts like:
- Word navigation with Alt+F/B
- Line navigation with Ctrl+A/E
- Word deletion with Alt+Backspace and Alt+D
- History search with Ctrl+R and Ctrl+P/N