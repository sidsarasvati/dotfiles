# CLAUDE.md - Guidelines for dotfiles repository

## Repository Context
- Personal dotfiles collection maintained for over 12 years
- Primary focus on macOS configuration (last 10 years)
- No active maintenance for Windows or other Unix configurations
- Primarily manages bash/zsh and Emacs configurations
- Collection of useful tools and settings accumulated over time
- Designed to be portable to any new Mac or Unix-based environment
- Custom zsh configuration with literate programming approach (no external dependencies)
- Multiple prompt styles with git integration and Emacs-style keybindings

## Build & Install Commands
- `./install.sh` - Main setup script (installs homebrew, packages, links configs)
- `brew bundle` - Install packages from Brewfile
- `stow <dir>` - Link configs from specific directory (bash, git, emacs, zsh, misc)
- `brew bundle check` - Verify all Brewfile dependencies are installed
- `brew bundle cleanup` - Remove packages not in Brewfile

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

## Repository Organization
- Organized by tool/category (bash, git, emacs, misc)
- Uses GNU stow for symlink management
- Primary focus on macOS, limited support for Linux
- Brewfile contains all package dependencies organized by category

## Recent Updates (2025-03-23)
- Updated PATH configuration for Homebrew on Apple Silicon Macs
- Removed Doom Emacs path configuration (using vanilla Emacs now)
- Converted Emacs configuration to literate programming style with org-mode
- Fixed config.org path resolution and theme loading in Emacs
- Centralized backup/auto-save files and disabled lock files in Emacs
- Updated .gitignore to exclude Emacs temporary files
- Improved Emacs launch commands:
  - `em` launches GUI Emacs properly detached from terminal
  - `emacs` launches terminal Emacs with -nw option