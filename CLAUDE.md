# CLAUDE.md - Guidelines for dotfiles repository

## Build & Install Commands
- `./install.sh` - Main setup script (installs homebrew, packages, links configs)
- `brew bundle` - Install packages from Brewfile
- `stow <dir>` - Link configs from specific directory (bash, git, emacs, misc)

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
- **Documentation**: Use readable comments to explain non-obvious code

## Repository Organization
- Organized by tool/category (bash, git, emacs, misc)
- Uses GNU stow for symlink management
- Primary focus on macOS, limited support for Linux