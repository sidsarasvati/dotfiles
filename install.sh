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
    read -p "Install Homebrew? (y/n): " choice
    if [[ "$choice" =~ ^[Yy]$ ]]; then
      echo 'Installing homebrew...'
      /bin/bash -c "$(curl -fsSL $HOMEBREW_DOWNLOAD_URL)"
    else
      echo 'Skipping Homebrew installation.'
      return 1
    fi
  else 	
    echo 'Homebrew installed.'
  fi

  BREW_PATH=$(detect_homebrew_path)
  eval "$($BREW_PATH shellenv)"
  return 0
}

install_packages() {
  read -p "Install/upgrade essential packages (like stow)? (y/n): " choice
  if [[ "$choice" =~ ^[Yy]$ ]]; then
    echo "Installing packages..." 
    brew install stow > /dev/null ||  brew upgrade stow > /dev/null
  else
    echo "Skipping package installation."
    # Check if stow is available
    if ! command -v stow &> /dev/null; then
      echo "WARNING: GNU stow is required but not found."
      echo "Please install it manually before continuing."
      read -p "Continue anyway? (y/n): " cont
      [[ "$cont" =~ ^[Yy]$ ]] || exit 1
    fi
  fi
}

# Check if configuration exists and warn about replacements
check_existing_config() {
  local config_name="$1"
  local dir_name="$2"
  local file_paths=("${@:3}")
  
  local has_existing=0
  local existing_files=()
  local already_linked=()
  local needs_update=()
  
  # Absolute path to the dotfiles directory
  local dotfiles_dir="$(pwd)"
  
  for file in "${file_paths[@]}"; do
    if [[ -e "$file" || -L "$file" ]]; then
      has_existing=1
      existing_files+=("$file")
      
      # Get the relative path from home directory
      local rel_path="${file/#$HOME\//}"
      # Find the corresponding file in the dotfiles repo
      local dotfile_path="$dotfiles_dir/$dir_name/$rel_path"
      
      # Check if it's already a symlink to our dotfiles
      if [[ -L "$file" && "$(readlink "$file")" == "$dotfile_path" ]]; then
        already_linked+=("$file")
      elif [[ -f "$file" && -f "$dotfile_path" ]]; then
        # Check if content differs (only for regular files)
        if ! diff -q "$file" "$dotfile_path" >/dev/null 2>&1; then
          needs_update+=("$file")
        fi
      else
        # Either not a symlink to our dotfiles or not identical
        needs_update+=("$file")
      fi
    fi
  done
  
  if [[ $has_existing -eq 1 ]]; then
    # If all existing files are already properly linked to our dotfiles
    if [[ ${#already_linked[@]} -eq ${#existing_files[@]} ]]; then
      echo "✅ $config_name configuration is already linked to your dotfiles."
      return 2  # Already configured correctly
    fi
    
    echo "⚠️  WARNING: Found existing $config_name configuration files:"
    for file in "${existing_files[@]}"; do
      if [[ " ${already_linked[@]} " =~ " $file " ]]; then
        echo "   - $file (already linked to dotfiles)"
      else
        echo "   - $file (will be replaced)"
      fi
    done
    
    if [[ ${#needs_update[@]} -gt 0 ]]; then
      echo "   Some files will be overwritten or modified if you proceed."
      
      # Only offer to show diffs if there are files that need updating
      read -p "Would you like to see diffs between existing and new configurations? (y/n): " show_diff
      if [[ "$show_diff" =~ ^[Yy]$ ]]; then
        for file in "${needs_update[@]}"; do
          # Get the relative path from home directory
          local rel_path="${file/#$HOME\//}"
          # Find the corresponding file in the dotfiles repo
          local dotfile_path="$dotfiles_dir/$dir_name/$rel_path"
          
          if [[ -f "$dotfile_path" ]]; then
            echo ""
            echo "Diff for $rel_path:"
            echo "------------------------------------------------------------"
            if command -v colordiff >/dev/null 2>&1; then
              # Use colordiff if available
              colordiff -u "$file" "$dotfile_path"
            else
              # Otherwise use regular diff
              diff -u "$file" "$dotfile_path"
            fi
            echo "------------------------------------------------------------"
            echo ""
          else
            echo "Cannot find corresponding file for $file in dotfiles repository."
          fi
        done
      fi
    fi
    
    return 0  # True, has existing config that needs updating
  else
    return 1  # False, no existing config
  fi
}

# Backup files before stowing
backup_configs() {
  local config_name="$1"
  local file_paths=("${@:2}")
  
  # Create backup directory
  local backup_dir="$HOME/.dotfiles_backup/$(date +%Y%m%d%H%M%S)/$config_name"
  mkdir -p "$backup_dir"
  
  local backed_up=0
  
  for file in "${file_paths[@]}"; do
    if [[ -e "$file" || -L "$file" ]]; then
      # Get just the filename
      local filename=$(basename "$file")
      # Copy to backup directory
      cp -L "$file" "$backup_dir/$filename"
      backed_up=1
    fi
  done
  
  if [[ $backed_up -eq 1 ]]; then
    echo "Backed up existing $config_name configuration to $backup_dir"
    return 0
  else
    return 1
  fi
}

stow_files() {
  # Ask which configurations to stow
  echo "Select configurations to set up:"
  
  # Check for existing git configurations
  local git_files=("$HOME/.gitconfig" "$HOME/.gitignore_global")
  local git_status=$(check_existing_config "git" "git" "${git_files[@]}")
  local git_result=$?
  
  if [[ $git_result -eq 0 ]]; then
    # Has existing config that needs updating
    echo "   - If you proceed, your existing git configuration will be replaced."
    read -p "Would you like to back up your existing git configurations first? (y/n): " backup_choice
    [[ "$backup_choice" =~ ^[Yy]$ ]] && backup_configs "git" "${git_files[@]}"
    read -p "Set up git configuration? (y/n): " setup_git
    [[ "$setup_git" =~ ^[Yy]$ ]] && stow git && echo "Git configuration linked."
  elif [[ $git_result -eq 2 ]]; then
    # Already linked to our dotfiles
    read -p "Git configuration is already linked to dotfiles. Reinstall anyway? (y/n): " setup_git
    [[ "$setup_git" =~ ^[Yy]$ ]] && stow git && echo "Git configuration re-linked."
  else
    # No existing config
    read -p "Set up git configuration? (y/n): " setup_git
    [[ "$setup_git" =~ ^[Yy]$ ]] && stow git && echo "Git configuration linked."
  fi
  
  # Check for existing emacs configurations
  local emacs_files=("$HOME/.emacs" "$HOME/.emacs.d/init.el" "$HOME/.emacs.d/config.org")
  
  # Check for Doom Emacs in all possible locations
  local doom_found=false
  local doom_locations=()
  
  # Check for Doom Emacs config directories
  [[ -d "$HOME/.config/doom" ]] && doom_found=true && doom_locations+=("$HOME/.config/doom")
  [[ -d "$HOME/.doom.d" ]] && doom_found=true && doom_locations+=("$HOME/.doom.d")
  
  # Check for Doom Emacs installation directories
  [[ -d "$HOME/.emacs.d" && -f "$HOME/.emacs.d/bin/doom" ]] && doom_found=true && doom_locations+=("$HOME/.emacs.d")
  [[ -d "$HOME/.config/emacs" && -f "$HOME/.config/emacs/bin/doom" ]] && doom_found=true && doom_locations+=("$HOME/.config/emacs")
  
  if [[ "$doom_found" == "true" ]]; then
    echo "⚠️  WARNING: Doom Emacs detected in the following locations:"
    for location in "${doom_locations[@]}"; do
      echo "   - $location"
    done
    
    echo ""
    echo "   Doom Emacs uses a completely different configuration approach"
    echo "   Installing this Emacs configuration will likely conflict with Doom"
    echo ""
    echo "   To properly remove Doom Emacs before proceeding:"
    echo "   1. Back up your configurations if needed:"
    for location in "${doom_locations[@]}"; do
      echo "      cp -r $location ${location}.bak"
    done
    echo ""
    echo "   2. Remove Doom completely:"
    for location in "${doom_locations[@]}"; do
      echo "      rm -rf $location"
    done
    echo ""
    echo "   Doom stores configuration in these directories and doesn't write"
    echo "   files elsewhere on your system. Removing these directories should"
    echo "   completely uninstall Doom Emacs."
    echo ""
    echo "   Alternatively, skip the Emacs configuration if you want to keep using Doom."
    
    read -p "Do you want to continue anyway (not recommended)? (y/n): " continue_with_doom
    if [[ ! "$continue_with_doom" =~ ^[Yy]$ ]]; then
      echo "Skipping Emacs configuration to avoid conflicts with Doom Emacs."
      return
    fi
  fi
  
  # Check for standard Emacs configurations
  local emacs_status=$(check_existing_config "emacs" "emacs" "${emacs_files[@]}")
  local emacs_result=$?
  
  if [[ $emacs_result -eq 0 ]]; then
    # Has existing config that needs updating
    echo "   - If you proceed, your existing emacs configuration will be replaced."
    echo "   - Your existing emacs packages won't be affected, but configuration will change."
    read -p "Would you like to back up your existing emacs configurations first? (y/n): " backup_choice
    [[ "$backup_choice" =~ ^[Yy]$ ]] && backup_configs "emacs" "${emacs_files[@]}"
    read -p "Set up emacs configuration? (y/n): " setup_emacs
    [[ "$setup_emacs" =~ ^[Yy]$ ]] && stow emacs && echo "Emacs configuration linked."
  elif [[ $emacs_result -eq 2 ]]; then
    # Already linked to our dotfiles
    read -p "Emacs configuration is already linked to dotfiles. Reinstall anyway? (y/n): " setup_emacs
    [[ "$setup_emacs" =~ ^[Yy]$ ]] && stow emacs && echo "Emacs configuration re-linked."
  else
    # No existing config
    read -p "Set up emacs configuration? (y/n): " setup_emacs
    [[ "$setup_emacs" =~ ^[Yy]$ ]] && stow emacs && echo "Emacs configuration linked."
  fi
  
  # Misc folder is kept for historical reference but not included in automatic installation
  
  # Check for existing zsh configurations
  local zsh_files=("$HOME/.zshrc" "$HOME/.zprofile" "$HOME/.zshenv")
  local zsh_status=$(check_existing_config "zsh" "zsh" "${zsh_files[@]}")
  local zsh_result=$?
  
  if [[ $zsh_result -eq 0 ]]; then
    # Has existing config that needs updating
    echo "   - If you proceed, your existing zsh configuration will be replaced."
    echo "   - If you're using oh-my-zsh, this will override its configuration."
    echo "   - If using oh-my-zsh, you may want to keep it but adapt your .zshenv"
    echo "     to source these dotfiles as well."
    read -p "Would you like to back up your existing zsh configurations first? (y/n): " backup_choice
    [[ "$backup_choice" =~ ^[Yy]$ ]] && backup_configs "zsh" "${zsh_files[@]}"
    read -p "Set up zsh configuration? (y/n): " setup_zsh_files
    [[ "$setup_zsh_files" =~ ^[Yy]$ ]] && stow zsh && echo "Zsh files linked."
  elif [[ $zsh_result -eq 2 ]]; then
    # Already linked to our dotfiles
    read -p "Zsh configuration is already linked to dotfiles. Reinstall anyway? (y/n): " setup_zsh_files
    [[ "$setup_zsh_files" =~ ^[Yy]$ ]] && stow zsh && echo "Zsh files re-linked."
  else
    # No existing config
    read -p "Set up zsh configuration? (y/n): " setup_zsh_files
    [[ "$setup_zsh_files" =~ ^[Yy]$ ]] && stow zsh && echo "Zsh files linked."
  fi
  
  # Bash configuration has been removed as macOS now uses zsh by default
}

setup_zsh() {
  # Check for existing .zshenv file
  if [[ -e "$HOME/.zshenv" ]]; then
    echo "⚠️  WARNING: Found existing .zshenv file."
    echo "   Your current .zshenv will be replaced if you proceed."
    echo "   This might affect your existing Zsh configuration."
    echo "   If you're using oh-my-zsh, this will override part of its setup."
    
    # Offer to show diff
    if [[ -e "$HOME/.zshenv" ]]; then
      read -p "Would you like to see what's in your current .zshenv? (y/n): " show_content
      if [[ "$show_content" =~ ^[Yy]$ ]]; then
        echo ""
        echo "Content of current .zshenv:"
        echo "------------------------------------------------------------"
        cat "$HOME/.zshenv"
        echo "------------------------------------------------------------"
        echo ""
      fi
    fi
  fi
  
  read -p "Set up Zsh environment pointing to dotfiles? (y/n): " choice
  if [[ "$choice" =~ ^[Yy]$ ]]; then
    echo "Setting up Zsh configuration..."
    
    # Get the absolute path to the dotfiles directory
    DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    
    # Back up existing .zshenv if it exists and user confirms
    if [[ -e "$HOME/.zshenv" ]]; then
      read -p "Would you like to back up your existing .zshenv first? (y/n): " backup_choice
      if [[ "$backup_choice" =~ ^[Yy]$ ]]; then
        # Create backup directory
        local backup_dir="$HOME/.dotfiles_backup/$(date +%Y%m%d%H%M%S)/zshenv"
        mkdir -p "$backup_dir"
        cp "$HOME/.zshenv" "$backup_dir/.zshenv"
        echo "Backed up existing .zshenv to $backup_dir/.zshenv"
      fi
    fi
    
    # Create .zshenv file in home directory to point to dotfiles
    cat > ~/.zshenv << EOF
# This file redirects to the dotfiles-managed zsh configuration
export ZDOTDIR="${DOTFILES_DIR}/zsh"
EOF
    
    echo "Zsh configuration linked via ~/.zshenv"
    
    # Guide for oh-my-zsh users
    if [[ -d "$HOME/.oh-my-zsh" ]]; then
      echo ""
      echo "📝 IMPORTANT NOTE FOR OH-MY-ZSH USERS:"
      echo "   Detected existing oh-my-zsh installation."
      echo "   To use both oh-my-zsh and these dotfiles, you may need to:"
      echo "   1. Edit your ~/.zshenv to source both configurations"
      echo "   2. Or move your oh-my-zsh customizations into the dotfiles structure"
      echo "   3. Or consider using just one of the configurations"
      echo ""
      echo "   If you want to keep oh-my-zsh, you can modify your ~/.zshenv:"
      echo ""
      echo "   # First load dotfiles zsh configuration"
      echo "   export ZDOTDIR=\"${DOTFILES_DIR}/zsh\""
      echo "   source \"\$ZDOTDIR/.zshrc\""
      echo ""
      echo "   # Then load oh-my-zsh configuration"
      echo "   export ZSH=\"\$HOME/.oh-my-zsh\""
      echo "   source \"\$ZSH/oh-my-zsh.sh\""
    fi
  else
    echo "Skipping Zsh environment setup."
  fi
}

setup_claude() {
  # Check for existing Claude configuration
  if [[ -e "$HOME/.claude/CLAUDE.md" ]]; then
    echo "⚠️  WARNING: Found existing Claude configuration file."
    echo "   Your current Claude configuration will be replaced if you proceed."
    
    # Offer to show diff
    read -p "Would you like to see the difference between your current and new Claude configuration? (y/n): " show_diff
    if [[ "$show_diff" =~ ^[Yy]$ ]]; then
      echo ""
      echo "Diff between existing and new Claude configuration:"
      echo "------------------------------------------------------------"
      if command -v colordiff >/dev/null 2>&1; then
        # Use colordiff if available
        colordiff -u "$HOME/.claude/CLAUDE.md" "$(pwd)/claude/.claude/CLAUDE.md"
      else
        # Otherwise use regular diff
        diff -u "$HOME/.claude/CLAUDE.md" "$(pwd)/claude/.claude/CLAUDE.md"
      fi
      echo "------------------------------------------------------------"
      echo ""
    fi
  fi
  
  read -p "Set up Claude Code configuration? (y/n): " choice
  if [[ "$choice" =~ ^[Yy]$ ]]; then
    echo "Setting up Claude Code configuration..."
    
    # Back up existing Claude config if it exists and user confirms
    if [[ -e "$HOME/.claude/CLAUDE.md" ]]; then
      read -p "Would you like to back up your existing Claude configuration first? (y/n): " backup_choice
      if [[ "$backup_choice" =~ ^[Yy]$ ]]; then
        # Create backup directory
        local backup_dir="$HOME/.dotfiles_backup/$(date +%Y%m%d%H%M%S)/claude"
        mkdir -p "$backup_dir"
        cp "$HOME/.claude/CLAUDE.md" "$backup_dir/CLAUDE.md"
        echo "Backed up existing Claude configuration to $backup_dir/CLAUDE.md"
      fi
    fi
    
    # Ensure .claude directory exists
    mkdir -p ~/.claude
    
    # Create symlink for Claude config file
    ln -sf "$(pwd)/claude/.claude/CLAUDE.md" ~/.claude/CLAUDE.md
    
    echo "Claude configuration linked to ~/.claude/CLAUDE.md"
  else
    echo "Skipping Claude configuration."
  fi
}

# Check if there are any XDG-style configurations that might conflict
check_xdg_configs() {
  echo "Checking for XDG-style configurations in ~/.config that might need special handling..."
  
  # Keep track of found configurations
  local found_configs=()
  
  # Check for common tools using XDG_CONFIG_HOME
  [[ -d "$HOME/.config/nvim" ]] && found_configs+=("Neovim (in ~/.config/nvim)")
  [[ -d "$HOME/.config/doom" ]] && found_configs+=("Doom Emacs (in ~/.config/doom)")
  [[ -d "$HOME/.config/alacritty" ]] && found_configs+=("Alacritty terminal (in ~/.config/alacritty)")
  [[ -d "$HOME/.config/kitty" ]] && found_configs+=("Kitty terminal (in ~/.config/kitty)")
  [[ -d "$HOME/.config/fish" ]] && found_configs+=("Fish shell (in ~/.config/fish)")
  [[ -d "$HOME/.config/starship" ]] && found_configs+=("Starship prompt (in ~/.config/starship)")
  [[ -d "$HOME/.config/tmux" ]] && found_configs+=("Tmux (in ~/.config/tmux)")
  
  # If any XDG configs were found, display a notice
  if [ ${#found_configs[@]} -gt 0 ]; then
    echo ""
    echo "📋 NOTICE: Detected XDG-style configurations that might need special handling:"
    for config in "${found_configs[@]}"; do
      echo "   - $config"
    done
    echo "   These configurations follow the XDG Base Directory specification and store"
    echo "   settings in ~/.config/ rather than traditional dotfiles in ~/"
    echo "   Keep this in mind if you encounter any conflicts with your dotfiles."
    echo ""
    read -p "Press Enter to continue..." _
  fi
}

setup() {
  # Check for XDG-style configurations
  check_xdg_configs
  
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
  # Try to install homebrew, but continue if user chooses to skip
  install_homebrew || echo "Continuing without Homebrew installation."
  
  # Install essential packages if homebrew is available
  if command -v brew &> /dev/null; then
    install_packages
  else
    echo "Homebrew not available, skipping package installation."
    # Check if stow is available
    if ! command -v stow &> /dev/null; then
      echo "WARNING: GNU stow is required but not found."
      echo "Please install it manually before continuing."
      read -p "Continue anyway? (y/n): " cont
      [[ "$cont" =~ ^[Yy]$ ]] || exit 1
    fi
  fi
  
  # Continue with stow and setup
  stow_files
  setup
}

main

echo ""
echo "Setup completed. Restart the terminal app for the new config to take effect."
