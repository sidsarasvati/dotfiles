#!/bin/bash

set -eu

HOMEBREW_DOWNLOAD_URL=https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh

# Detect Mac architecture to determine Homebrew path
detect_homebrew_path() {
  if [[ "$(uname -m)" == "arm64" ]]; then
    echo "/opt/homebrew/bin/brew"
  else
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

setup() {
    # Setup zsh configuration
    setup_zsh
    
    # NOTE: org-mode file symlink has been removed as org-mode is no longer used for knowledge management
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
