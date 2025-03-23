#!/bin/bash

set -eu

HOMEBREW_DOWNLOAD_URL=https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh

install_homebrew() {
  echo "Checking for Homebrew, and installing if necessary"
  if ! [[ $( command -v brew ) ]]; then
    echo 'Installing homebrew...'
    /bin/bash -c "$(curl -fsSL $HOMEBREW_DOWNLOAD_URL)"
  else 	
    echo 'Homebrew installed.'
  fi

  eval "$(/opt/homebrew/bin/brew shellenv)"
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
    # create symlink for org files sync with iCloud
    ln -s ~/Library/Mobile\ Documents/iCloud~com~appsonthemove~beorg/Documents/org ~/.org
    
    # Setup zsh configuration
    setup_zsh
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
