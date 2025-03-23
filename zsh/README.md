# Custom Zsh Configuration

This directory contains a standalone Zsh configuration that doesn't depend on frameworks like Oh-My-Zsh. It incorporates Emacs-style keybindings and preserves the functionality from your existing bash configuration.

## Features

- **No External Dependencies**: Self-contained configuration without requiring Oh-My-Zsh
- **Emacs Keybindings**: Familiar Emacs navigation and editing commands
- **Organized Structure**:
  - `.zshrc`: Main configuration file
  - `env.zsh`: Environment variables and PATH configuration
  - `aliases.zsh`: Organized aliases by category
  - `functions/`: Directory for custom zsh functions

## Installation

The configuration is automatically installed by the main `install.sh` script, which uses GNU Stow to create symlinks.

Manually install with:
```bash
cd ~/Code/sid/dotfiles
stow zsh
```

## Key Bindings

The configuration includes Emacs-style key bindings:

- `Alt + B`: Move cursor back one word
- `Alt + F`: Move cursor forward one word
- `Alt + Backspace`: Delete previous word
- `Alt + D`: Delete next word 
- `Ctrl + A`: Jump to beginning of line
- `Ctrl + E`: Jump to end of line
- `Ctrl + R`: Search command history

## Structure

- `.zshrc`: Main configuration file
- `env.zsh`: Environment variables and PATH configuration
- `aliases.zsh`: Collection of useful aliases
- `functions/*.zsh`: Custom shell functions

## Migration from Bash

This configuration is designed to smoothly transition from bash to zsh while keeping your existing customizations. It provides improved completion, history management, and directory navigation while maintaining compatibility with your existing workflow.

## Customization

Add your personal customizations to:
- `/zsh/local.zsh` (this file is gitignored)

To add new functions:
- Create a new file in `/zsh/functions/` with a `.zsh` extension