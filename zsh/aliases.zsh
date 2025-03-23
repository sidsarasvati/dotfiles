##############################################################################
#                                                                            #
#                            Shell Aliases                                  #
#                                                                            #
#                 Organized command shortcuts by category                    #
#                                                                            #
##############################################################################

# === Directory Navigation ===
#
# These aliases make navigating the filesystem faster and more convenient,
# reducing keystrokes for common operations.

# Quick directory traversal
alias ..='cd ..'                  # Go up one directory
alias ...='cd ../..'              # Go up two directories
alias ....='cd ../../..'          # Go up three directories

# Enhanced directory listing
alias la='ls -A'                  # List all files (including hidden)
alias ll='ls -Alh'                # Long listing with human-readable sizes
alias l='ls -Alh'                 # Same as ll (convenient shorthand)

# Specialized directory views
alias lsd="ls -R | grep \":$\" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'" # Directory tree using ls
alias lst='tree'                  # Directory tree using 'tree' command
alias lsu='du -sh *'              # Human-readable disk usage for files/folders

# Create and navigate to directory in one command
function md() {
  mkdir -p "$1"                   # Create directory (including parents)
  cd "$1"                         # Change to the new directory
}

# === Git Version Control ===
#
# Git operations simplified with meaningful shortcuts.
# These aliases save a lot of typing for common git operations.

alias g='git'                     # Git shorthand
alias gs='git status'             # Check repository status
alias dif='git diff'              # Show unstaged changes
alias difc='git diff --cached'    # Show staged changes
alias ss='git status'             # Status shorthand (alternative to gs)
alias sl='git log'                # Show commit history
alias glog='git log --graph --all --decorate --color' # Visual commit history
alias st='git status -uall --procelain' # Machine-readable status  
alias ga='git add'                # Stage files
alias gc='git commit'             # Commit changes
alias gg='git grep'               # Search in repository
alias gp='git push'               # Push to remote
alias gcl='git clone'             # Clone repository

# === Search & Process Management ===
#
# Improved search tools with sensible defaults.

alias g="grep -i"                 # Case-insensitive grep
alias eg="grep -E -i"             # Case-insensitive extended regexp grep
alias pg="ps aux | grep -i"       # Search in running processes

# === System & Environment ===
#
# Tools for inspecting and managing system state.

alias env="env | sort"            # Show environment variables (sorted)
alias path='echo $PATH | tr ":" "\n"' # Display PATH entries one per line
alias ip="ifconfig | grep 'inet ' | grep -v 127.0.0.1" # Display IP addresses
alias pd="pushd"                  # Push directory onto stack
alias mkcd='mkdir -p "$@" && cd "$_"' # Create directory and cd into it

# === Editors ===
#
# Quick access to text editors with appropriate options.

# Emacs aliases for macOS
# Launch Emacs GUI, properly detached from terminal
function em() {
  # Simple macOS-only implementation
  if [[ $# -eq 0 ]]; then
    # No files specified
    open -a Emacs.app
  else
    # Files specified
    open -a Emacs.app "$@"
  fi
}

# Make 'emacs' launch terminal Emacs by default
alias emacs='emacs -nw'          # Launch Emacs in terminal mode

alias e=$EDITOR                   # Use the default editor
alias nv="nvim"                   # Launch Neovim

# === Containers & Orchestration ===
#
# Docker and Kubernetes shortcuts.

alias d='docker'                  # Docker commands
alias dc='docker-compose'         # Docker Compose
alias kb='kubectl'                # Kubernetes control

# === Node.js Development ===
#
# NPM package management and development shortcuts.

alias ni='npm install'            # Install packages
alias nrd='npm run dev'           # Run development server

# === Platform-Specific Settings ===
#
# Different platforms need different settings for some commands.

if [[ $(uname) == 'Darwin' ]]; then
  # --- macOS Specific ---
  
  # Set colored ls output (macOS style)
  alias ls="ls -G"
  
  # Color scheme optimized for dark backgrounds
  export LSCOLORS=gxfxcxdxbxegedabagacad
  
  # Finder visibility toggles
  alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder'
  alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder'
  
else
  # --- Linux/Other Platform Specific ---
  
  # Set colored ls output (GNU style)
  alias ls="ls --color=auto"
  
  # Color scheme optimized for dark backgrounds
  export LS_COLORS='no=00:fi=00:di=00;36:ln=00;35:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=00;31:'
fi