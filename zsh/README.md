# Custom Zsh Configuration

This directory contains a standalone Zsh configuration without dependencies on external frameworks like Oh-My-Zsh. It incorporates Emacs-style keybindings and preserves the functionality from your existing bash configuration, with a clean, literate programming approach.

## Features

- **Independent Configuration**: Self-contained without requiring Oh-My-Zsh or other frameworks
- **Emacs Keybindings**: Familiar Emacs navigation and editing commands
- **Literate Programming Style**: Well-documented with explanations for each section
- **Custom Prompts**: Multiple prompt styles with git branch integration
- **Organized Structure**:
  - `.zshrc`: Main configuration file with modular loading
  - `env.zsh`: Environment variables and PATH configuration
  - `aliases.zsh`: Organized aliases by category
  - `functions/`: Directory for custom zsh functions

## Installation

The configuration is automatically installed by the main `install.sh` script:

1. It detects the dotfiles directory location
2. Creates a `~/.zshenv` file that points to the zsh configuration
3. No need for stow - the `.zshenv` file handles the configuration loading

## Prompt Styles

The zsh configuration includes several prompt styles:

1. **Developer Pro** (default): Two-line prompt with git branch and box-drawing characters
   ```
   ╭─ ~/path/to/directory (branch)
   ╰─ ❯
   ```

2. **Minimal with Lambda**: Clean style with lambda character
   ```
   ~/path/to/directory (branch)
   λ
   ```

3. **AI-Coder Vibe**: Modern style with unique symbols
   ```
   ~/path/to/directory (branch)
   ⟩
   ```

4. **Informativity + Style**: Includes command execution time and status
   ```
   ~/path/to/directory (branch)
   ❯                     2s ✓
   ```

5. **Classic with Lambda**: Single-line prompt with lambda
   ```
   ~/path/to/directory (branch) λ
   ```

To switch between prompts, edit `.zshrc` and uncomment your preferred option.

## Key Bindings

The configuration includes Emacs-style key bindings:

- `Alt + B`: Move cursor back one word
- `Alt + F`: Move cursor forward one word
- `Alt + Backspace`: Delete previous word
- `Alt + D`: Delete next word 
- `Ctrl + A`: Jump to beginning of line
- `Ctrl + E`: Jump to end of line
- `Ctrl + R`: Search command history
- `Ctrl + P/N`: Search history matching current input

## Directory Structure

- `.zshrc`: Main configuration file
- `env.zsh`: Environment variables and PATH configuration
- `aliases.zsh`: Collection of useful aliases organized by category
- `functions/*.zsh`: Custom shell functions
- `.gitignore`: Prevents committing local customizations

## Customization

Add machine-specific customizations to:
- `/zsh/local.zsh` (this file is gitignored)

To add new functions:
- Create a new file in `/zsh/functions/` with a `.zsh` extension

## Git Integration

The prompt includes git branch detection through a custom function:

- Only shows branch information when in a git repository
- Automatically updates as you navigate between directories
- Shows branch name in parentheses with custom colors