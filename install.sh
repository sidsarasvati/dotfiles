#!/bin/bash

set -eu

HOMEBREW_DOWNLOAD_URL=https://raw.githubusercontent.com/Homebrew/install/master/install

install_homebrew() {
  echo "Checking for Homebrew, and installing if necessary"
  if ! [[ $( command -v brew ) ]]; then
    echo 'Installing homebrew...'
    /usr/bin/ruby -e "$(curl -fsSL $HOMEBREW_DOWNLOAD_URL)" > /dev/null
  else
    echo 'Homebrew installed.'
  fi
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

  # Link .bash_profile -> .bashrc
  rm -f ~/.bash_profile
  ln -s ~/.bashrc ~/.bash_profile
}

setup() {
    # create symlink for org files sync with iCloud
    ln -s ~/Library/Mobile\ Documents/iCloud~com~appsonthemove~beorg/Documents/org ~/.org
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
