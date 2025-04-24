# CLAUDE.md - Guidelines for dotfiles repository

## Repository Context
- Personal dotfiles collection maintained for over 12 years
- Primary focus on macOS configuration (last 10 years)
- No active maintenance for Windows or other Unix configurations
- Primarily manages zsh and Emacs configurations
- Collection of useful tools and settings accumulated over time
- Designed to be portable to any new Mac or Unix-based environment
- Custom zsh configuration with literate programming approach (no external dependencies)
- Multiple prompt styles with git integration and Emacs-style keybindings

## Build & Install Commands
- `./install.sh` - Interactive setup script that manages all configurations without external dependencies
- `brew bundle` - Install packages from Brewfile (optional)
- `brew bundle check` - Verify all Brewfile dependencies are installed
- `brew bundle cleanup` - Remove packages not in Brewfile

## Installation Features
- Fully interactive prompts with "No" as the safe default
- Smart detection of already-installed configurations
- Backup functionality for existing configurations
- Diff display between existing and new configurations
- No external dependencies (formerly used GNU stow)
- Special handling for Doom Emacs detection
- Warning system for XDG-style configurations in ~/.config
- Automatic detection of oh-my-zsh installations

## Code Style Guidelines
- **Shell Scripts**: Use bash with `set -eu` for strict error handling
- **Functions**: Use lowercase snake_case for function names
- **Variables**: Uppercase for constants (HOMEBREW_DOWNLOAD_URL), lowercase for locals
- **Indentation**: 2 spaces for shell scripts
- **Comments**: Use # for comments, add TODOs for pending work
- **Path Handling**: Use quotes around paths with possible spaces
- **Platform Detection**: Check for OS type (darwin/linux) for cross-platform compatibility
- **Error Handling**: Check commands exist before running (e.g., `if ! [[ $( command -v brew ) ]]`)
- **Aliases**: Organize by category (general, git, docker, etc.)
- **Command Structure**: Place complex commands on multiple lines for readability
- **Testing**: Test scripts with `bash -n script.sh` for syntax checking

## Development Workflow
- Use feature branches for each enhancement (`feature/name-of-enhancement`)
- Make focused changes related to a single enhancement
- Test changes thoroughly before committing
- Use descriptive commit messages explaining the purpose of changes
- Update documentation (CLAUDE.md, CHANGELOG.md) when making significant changes
- Reference the workflow documentation in `docs/workflow.md` for detailed process
- Create GitHub issues for tracking work items (see `docs/workflow.md`)

## Repository Organization
- Organized by tool/category (git, emacs, zsh, claude)
- Uses direct symlinks with absolute paths for reliability
- Primary focus on macOS, limited support for Linux
- Brewfile contains all package dependencies organized by category
- Educational documentation in org-mode format under docs/emacs/

## Recent Updates (2025-04-24)
- Completely rebuilt the installation system:
  - Removed dependency on GNU stow
  - Added direct symlink approach with absolute paths
  - Made script fully interactive with safe defaults
  - Added smart detection of already-installed configurations
  - Added diff and backup functionality
  - Added Doom Emacs detection and guidance
  - Added XDG configuration detection
- Added support for Claude configurations with separate handling
- Improved zsh setup with better .zshenv integration
- Added local.zsh.example template for private tokens and secrets 
- Made all configurations primary tools with no "misc" category
- Updated PATH configuration for Homebrew on Apple Silicon Macs
- Converted Emacs configuration to literate programming style with org-mode
- Fixed config.org path resolution and theme loading in Emacs
- Centralized backup/auto-save files and disabled lock files in Emacs
- Improved Emacs launch commands:
  - `em` launches GUI Emacs properly detached from terminal
  - `emacs` launches terminal Emacs with -nw option
- Established GitHub issue-based workflow with 8 planned enhancements
- Added architecture detection for cross-platform compatibility

## Active Issues and Roadmap
The following issues have been created for ongoing improvements:
- #9: Add GitHub Copilot Integration for Emacs
- #11: Add tmux Configuration for Terminal Session Management
- #12: Add project.el Configuration for Emacs Project Management
- #13: Implement move-file Utility for Emacs
- #14: Enhance Search Capabilities with Swiper and ripgrep in Emacs
- #15: Add Terminal-based AI Assistant Tools and Functions
- #16: Integrate fzf for Fuzzy Finding in ZSH Configuration
- #17: Create Emacs Educational Documentation Series

See GitHub issues for the latest status of these enhancements.