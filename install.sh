#!/bin/bash

set -eu

HOMEBREW_DOWNLOAD_URL=https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh

# Detect Mac architecture to determine Homebrew path
detect_homebrew_path() {
  # Use safe command execution with fallback
  arch=$(uname -m 2>/dev/null || echo "unknown")
  if [[ "$arch" == "arm64" ]]; then
    echo "/opt/homebrew/bin/brew"
  else
    # Intel Mac or architecture detection failed
    echo "/usr/local/bin/brew"
  fi
}

install_homebrew() {
  echo "Checking for Homebrew, and installing if necessary"
  if ! [[ $( command -v brew ) ]]; then
    echo 'Installing homebrew...'
    /bin/bash -c "$(curl -fsSL $HOMEBREW_DOWNLOAD_URL)"
  else 	
    echo 'Homebrew installed.'
  fi

  BREW_PATH=$(detect_homebrew_path)
  eval "$($BREW_PATH shellenv)"
}

# TODO - use brew bundle
install_packages() {
  echo "Installing packages..." 
  brew install stow > /dev/null ||  brew upgrade stow > /dev/null
}


stow_files() {
  # Link other files
  stow bash
  stow git
  stow emacs
  stow misc # all other things that don't fit cleanly in a category
  stow zsh  # add the new zsh configurations
  
  # Claude config is handled separately in setup_claude function
  # since it requires special handling for the .claude directory

  # Link .bash_profile -> .bashrc
  rm -f ~/.bash_profile
  ln -s ~/.bashrc ~/.bash_profile
}

setup_zsh() {
  echo "Setting up Zsh configuration..."
  
  # Get the absolute path to the dotfiles directory
  DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  
  # Create .zshenv file in home directory to point to dotfiles
  cat > ~/.zshenv << EOF
# This file redirects to the dotfiles-managed zsh configuration
export ZDOTDIR="${DOTFILES_DIR}/zsh"
EOF
  
  echo "Zsh configuration linked via ~/.zshenv"
}

setup_claude() {
  echo "Setting up Claude Code configuration..."
  
  # Ensure .claude directory exists
  mkdir -p ~/.claude
  
  # Create symlink for Claude config file
  ln -sf "$(pwd)/claude/.claude/CLAUDE.md" ~/.claude/CLAUDE.md
  
  echo "Claude configuration linked to ~/.claude/CLAUDE.md"
}

setup() {
    # Setup zsh configuration
    setup_zsh
    
    # Setup Claude configuration
    setup_claude
    
    # NOTE: The previous org-mode file symlink linking iCloud to ~/.org has been removed for two reasons:
    # 1. It created a hardcoded path dependency that only worked on macOS with iCloud configured
    # 2. The dotfiles now use a more portable approach with org-directory variable in Emacs config
    # If you need to sync org files, consider doing so outside this script with a platform-agnostic approach
}


main() {
  install_homebrew
  install_packages    
  stow_files
  setup
}

main

echo ""
echo "SUCCESS. Restart the terminal app for the new config to take effect."
