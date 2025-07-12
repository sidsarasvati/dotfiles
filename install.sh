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
    read -p "Install Homebrew? (y/N): " choice
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
  echo "No external packages needed for installation."
}

# Git configuration symlinks
link_git_config() {
  echo "Setting up Git configuration..."
  # Use absolute paths for reliability
  ln -sf "$(pwd)/git/.gitconfig" "$HOME/.gitconfig"
  ln -sf "$(pwd)/git/.gitignore_global" "$HOME/.gitignore_global"
  echo "Git configuration linked."
}

# Emacs configuration symlinks
link_emacs_config() {
  echo "Setting up Emacs configuration..."
  # Create .emacs.d directory if it doesn't exist
  mkdir -p "$HOME/.emacs.d"
  # Link specific files with absolute paths
  ln -sf "$(pwd)/emacs/config.org" "$HOME/.emacs.d/config.org"
  ln -sf "$(pwd)/emacs/CLAUDE.md" "$HOME/.emacs.d/CLAUDE.md"
  echo "Emacs configuration linked."
}

# Zsh configuration setup
link_zsh_config() {
  echo "Setting up Zsh configuration..."
  
  # Create .zshenv file that points to the dotfiles directory
  # This file is generated, not linked from the repo
  local DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  cat > "$HOME/.zshenv" << EOF
# This file redirects to the dotfiles-managed zsh configuration
export ZDOTDIR="${DOTFILES_DIR}/zsh"
EOF

  echo "Zsh configuration set up successfully."
  echo "  - Created .zshenv file to point to dotfiles"
  echo "  - All zsh configuration will be loaded from ${DOTFILES_DIR}/zsh"
}

# Claude configuration
link_claude_config() {
  echo "Setting up Claude Code configuration..."
  # Create .claude directory if it doesn't exist
  mkdir -p "$HOME/.claude"
  # Link Claude config files
  ln -sf "$(pwd)/claude/.claude/CLAUDE.md" "$HOME/.claude/CLAUDE.md"
  echo "Claude configuration linked."
}

# Bin scripts
link_bin_scripts() {
  echo "Setting up bin scripts..."
  # Create bin directory if it doesn't exist
  mkdir -p "$HOME/bin"
  # Link all executable files from bin directory
  for script in "$(pwd)"/bin/*; do
    if [[ -x "$script" && -f "$script" ]]; then
      script_name=$(basename "$script")
      ln -sf "$script" "$HOME/bin/$script_name"
      echo "  Linked $script_name"
    fi
  done
  echo "Bin scripts linked."
}

# Check if configuration exists and warn about replacements
check_existing_config() {
  local config_name="$1"
  local dir_name="$2"
  local file_paths=("${@:3}")
  
  local has_existing=0
  local existing_files=()
  # Explicitly initialize as empty arrays to avoid "unbound variable" errors
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
    if [[ ${#already_linked[@]} -gt 0 && ${#already_linked[@]} -eq ${#existing_files[@]} ]]; then
      echo "✅ $config_name configuration is already linked to your dotfiles."
      return 2  # Already configured correctly
    fi
    
    echo "⚠️  WARNING: Found existing $config_name configuration files:"
    for file in "${existing_files[@]}"; do
      if [[ ${#already_linked[@]} -gt 0 && " ${already_linked[@]} " =~ " $file " ]]; then
        echo "   - $file (already linked to dotfiles)"
      else
        echo "   - $file (will be replaced)"
      fi
    done
    
    if [[ ${#needs_update[@]} -gt 0 ]]; then
      echo "   Some files will be overwritten or modified if you proceed."
      
      # Only offer to show diffs if there are files that need updating
      read -p "Would you like to see diffs between existing and new configurations? (y/N): " show_diff
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

install_dotfiles() {
  # Ask which configurations to set up
  echo "Select configurations to set up:"
  
  # Check if existing Git symlinks match our dotfiles path
  local git_files=("$HOME/.gitconfig" "$HOME/.gitignore_global")
  local git_already_linked=true
  local dotfiles_path="$(pwd)"
  
  # Check if gitconfig is already linked to our dotfiles with absolute path
  if [[ -L "$HOME/.gitconfig" ]]; then
    local link_target=$(readlink "$HOME/.gitconfig")
    # Only consider it properly linked if using absolute path
    if [[ "$link_target" != "$dotfiles_path/git/.gitconfig" ]]; then
      git_already_linked=false
    fi
  else
    git_already_linked=false
  fi
  
  if [[ "$git_already_linked" == "true" ]]; then
    echo "✅ Git configuration is already linked to your dotfiles."
    read -p "Reinstall anyway? (y/N): " setup_git
    [[ "$setup_git" =~ ^[Yy]$ ]] && link_git_config
  else
    # Check for existing git files
    local existing_files=()
    for file in "${git_files[@]}"; do
      if [[ -e "$file" || -L "$file" ]]; then
        existing_files+=("$file")
      fi
    done
    
    if [[ ${#existing_files[@]} -gt 0 ]]; then
      echo "⚠️  WARNING: Found existing Git configuration files:"
      for file in "${existing_files[@]}"; do
        echo "   - $file (will be replaced)"
      done
      
      echo "   - If you proceed, your existing git configuration will be replaced."
      read -p "Would you like to back up your existing git configurations first? (y/N): " backup_choice
      if [[ "$backup_choice" =~ ^[Yy]$ ]]; then
        local backup_dir="$HOME/.dotfiles_backup/$(date +%Y%m%d%H%M%S)/git"
        mkdir -p "$backup_dir"
        for file in "${existing_files[@]}"; do
          cp -L "$file" "$backup_dir/$(basename "$file")"
        done
        echo "Backed up existing Git configuration to $backup_dir"
      fi
    fi
    
    read -p "Set up git configuration? (y/N): " setup_git
    [[ "$setup_git" =~ ^[Yy]$ ]] && link_git_config
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
    
    read -p "Do you want to continue anyway (not recommended)? (y/N): " continue_with_doom
    if [[ ! "$continue_with_doom" =~ ^[Yy]$ ]]; then
      echo "Skipping Emacs configuration to avoid conflicts with Doom Emacs."
      return
    fi
  fi
  
  # Check if existing Emacs symlinks match our dotfiles path
  local emacs_already_linked=true
  local dotfiles_path="$(pwd)"
  
  # Check if config.org is already linked to our dotfiles with absolute path
  if [[ -L "$HOME/.emacs.d/config.org" ]]; then
    local link_target=$(readlink "$HOME/.emacs.d/config.org")
    # Only consider it properly linked if using absolute path
    if [[ "$link_target" != "$dotfiles_path/emacs/config.org" ]]; then
      emacs_already_linked=false
    fi
  else
    emacs_already_linked=false
  fi
  
  if [[ "$emacs_already_linked" == "true" ]]; then
    echo "✅ Emacs configuration is already linked to your dotfiles."
    read -p "Reinstall anyway? (y/N): " setup_emacs
    [[ "$setup_emacs" =~ ^[Yy]$ ]] && link_emacs_config
  else
    # Check for existing emacs files
    local existing_files=()
    for file in "${emacs_files[@]}"; do
      if [[ -e "$file" || -L "$file" ]]; then
        existing_files+=("$file")
      fi
    done
    
    if [[ ${#existing_files[@]} -gt 0 ]]; then
      echo "⚠️  WARNING: Found existing Emacs configuration files:"
      for file in "${existing_files[@]}"; do
        echo "   - $file (will be replaced)"
      done
      
      echo "   - If you proceed, your existing emacs configuration will be replaced."
      echo "   - Your existing emacs packages won't be affected, but configuration will change."
      read -p "Would you like to back up your existing emacs configurations first? (y/N): " backup_choice
      if [[ "$backup_choice" =~ ^[Yy]$ ]]; then
        local backup_dir="$HOME/.dotfiles_backup/$(date +%Y%m%d%H%M%S)/emacs"
        mkdir -p "$backup_dir"
        for file in "${existing_files[@]}"; do
          cp -L "$file" "$backup_dir/$(basename "$file")"
        done
        echo "Backed up existing Emacs configuration to $backup_dir"
      fi
    fi
    
    read -p "Set up emacs configuration? (y/N): " setup_emacs
    [[ "$setup_emacs" =~ ^[Yy]$ ]] && link_emacs_config
  fi
  
  # Misc folder is kept for historical reference but not included in automatic installation
  
  # Directly call setup_zsh which handles its own checking for existing files
  setup_zsh
  
  # Bash configuration has been removed as macOS now uses zsh by default
}

setup_zsh() {
  # First check if existing .zshenv already points to our dotfiles
  local already_setup=false
  local dotfiles_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
  
  if [[ -e "$HOME/.zshenv" ]]; then
    # Check for any string that points ZDOTDIR to our zsh directory
    if grep -q "ZDOTDIR=.*${dotfiles_dir}/zsh" "$HOME/.zshenv"; then
      already_setup=true
    fi
  fi
  
  if [[ "$already_setup" == "true" ]]; then
    echo "✅ Zsh configuration is already set up to use your dotfiles."
    read -p "Reinstall anyway? (y/N): " choice
  else
    # Only show warnings if not already properly set up
    if [[ -e "$HOME/.zshenv" ]]; then
      echo "⚠️  WARNING: Found existing .zshenv file."
      echo "   Your current .zshenv will be replaced if you proceed."
      echo "   This might affect your existing Zsh configuration."
      
      # Offer to show content
      read -p "Would you like to see what's in your current .zshenv? (y/N): " show_content
      if [[ "$show_content" =~ ^[Yy]$ ]]; then
        echo ""
        echo "Content of current .zshenv:"
        echo "------------------------------------------------------------"
        cat "$HOME/.zshenv"
        echo "------------------------------------------------------------"
        echo ""
      fi
      
      # Back up existing .zshenv if user confirms
      read -p "Would you like to back up your existing .zshenv first? (y/N): " backup_choice
      if [[ "$backup_choice" =~ ^[Yy]$ ]]; then
        # Create backup directory
        local backup_dir="$HOME/.dotfiles_backup/$(date +%Y%m%d%H%M%S)/zshenv"
        mkdir -p "$backup_dir"
        cp "$HOME/.zshenv" "$backup_dir/.zshenv"
        echo "Backed up existing .zshenv to $backup_dir/.zshenv"
      fi
    fi
    
    read -p "Set up Zsh configuration? (y/N): " choice
  fi
  
  if [[ "$choice" =~ ^[Yy]$ ]]; then
    # Use dedicated function to create .zshenv
    link_zsh_config
    
    # Guide for oh-my-zsh users
    if [[ -d "$HOME/.oh-my-zsh" ]]; then
      echo ""
      echo "📝 IMPORTANT NOTE FOR OH-MY-ZSH USERS:"
      echo "   Detected existing oh-my-zsh installation."
      echo "   This setup replaces oh-my-zsh with a custom zsh configuration."
      echo "   Your previous oh-my-zsh settings will not be used."
      echo ""
      echo "   To remove oh-my-zsh completely, run: uninstall_oh_my_zsh"
    fi
  else
    echo "Skipping Zsh configuration."
  fi
}

setup_claude() {
  # Check if Claude configuration is already linked to our dotfiles
  local claude_already_linked=false
  local dotfiles_path="$(pwd)"
  
  # Check if CLAUDE.md is already linked to our dotfiles
  if [[ -L "$HOME/.claude/CLAUDE.md" ]]; then
    local link_target=$(readlink "$HOME/.claude/CLAUDE.md")
    if [[ "$link_target" == "$dotfiles_path/claude/.claude/CLAUDE.md" ]]; then
      claude_already_linked=true
    fi
  fi
  
  if [[ "$claude_already_linked" == "true" ]]; then
    echo "✅ Claude configuration is already linked to your dotfiles."
    read -p "Reinstall anyway? (y/N): " choice
  elif [[ -e "$HOME/.claude/CLAUDE.md" ]]; then
    echo "⚠️  WARNING: Found existing Claude configuration file."
    echo "   Your current Claude configuration will be replaced if you proceed."
    
    # Offer to show diff
    read -p "Would you like to see the difference between your current and new Claude configuration? (y/N): " show_diff
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
    
    read -p "Set up Claude Code configuration? (y/N): " choice
  else
    read -p "Set up Claude Code configuration? (y/N): " choice
  fi
  if [[ "$choice" =~ ^[Yy]$ ]]; then
    # Only offer backup if it's not already linked (no need to backup our own symlink)
    if [[ "$claude_already_linked" != "true" && -e "$HOME/.claude/CLAUDE.md" ]]; then
      read -p "Would you like to back up your existing Claude configuration first? (y/N): " backup_choice
      if [[ "$backup_choice" =~ ^[Yy]$ ]]; then
        # Create backup directory
        local backup_dir="$HOME/.dotfiles_backup/$(date +%Y%m%d%H%M%S)/claude"
        mkdir -p "$backup_dir"
        cp "$HOME/.claude/CLAUDE.md" "$backup_dir/CLAUDE.md"
        echo "Backed up existing Claude configuration to $backup_dir/CLAUDE.md"
      fi
    fi
    
    # Use the dedicated function to link Claude configuration
    link_claude_config
  else
    echo "Skipping Claude configuration."
  fi
}

# Setup bin scripts
setup_bin() {
  echo ""
  echo "=========================================="
  echo "Bin Scripts Setup"
  echo "=========================================="
  
  # Check for existing scripts in ~/bin
  local existing_scripts=()
  local our_scripts=()
  
  # Get list of our scripts
  for script in "$(pwd)"/bin/*; do
    if [[ -x "$script" && -f "$script" ]]; then
      our_scripts+=($(basename "$script"))
    fi
  done
  
  # Check which ones already exist
  for script_name in "${our_scripts[@]}"; do
    if [[ -e "$HOME/bin/$script_name" ]]; then
      existing_scripts+=("$script_name")
    fi
  done
  
  if [[ ${#existing_scripts[@]} -gt 0 ]]; then
    echo "⚠️  Found existing scripts in ~/bin:"
    for script in "${existing_scripts[@]}"; do
      echo "  - $script"
    done
    echo ""
  fi
  
  echo "Available scripts to install:"
  for script in "${our_scripts[@]}"; do
    echo "  - $script"
  done
  echo ""
  
  read -p "Set up bin scripts? (y/N): " choice
  if [[ "$choice" =~ ^[Yy]$ ]]; then
    link_bin_scripts
  else
    echo "Skipping bin scripts."
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
  
  # Setup Claude configuration
  setup_claude
  
  # Setup bin scripts
  setup_bin
  
  # Note: zsh setup is already handled in install_dotfiles
  
  # NOTE: The previous org-mode file symlink linking iCloud to ~/.org has been removed for two reasons:
  # 1. It created a hardcoded path dependency that only worked on macOS with iCloud configured
  # 2. The dotfiles now use a more portable approach with org-directory variable in Emacs config
  # If you need to sync org files, consider doing so outside this script with a platform-agnostic approach
}

main() {
  # Try to install homebrew, but continue if user chooses to skip
  install_homebrew || echo "Continuing without Homebrew installation."
  
  # Install packages if needed (no external dependencies now)
  install_packages
  
  # Continue with dotfiles installation and setup
  install_dotfiles
  setup
}

main

echo ""
echo "Setup completed. Restart the terminal app for the new config to take effect."
