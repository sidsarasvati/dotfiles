##############################################################################
#                                                                            #
#                 Environment Variables and Path Configuration               #
#                                                                            #
#            Sets up the shell environment, paths, and tool-specific         #
#                      configurations for development tools                  #
#                                                                            #
##############################################################################

# === Platform Detection ===
# 
# Detect the operating system to conditionally apply settings.
# This takes OSTYPE (like "darwin21.0") and removes numbers and dots to get just "darwin".
OS=${OSTYPE//[0-9.]/}

# === Homebrew Detection ===
#
# Detect Homebrew location based on platform and architecture
if [[ "$OS" == "darwin" ]]; then
  # Check for Apple Silicon Mac 
  # Use safe command execution with fallback
  arch=$(uname -m 2>/dev/null || echo "unknown")
  if [[ "$arch" == "arm64" ]]; then
    HOMEBREW_PREFIX="/opt/homebrew"
  else
    # Intel Mac or architecture detection failed
    HOMEBREW_PREFIX="/usr/local"
  fi
else
  # Default for other platforms
  HOMEBREW_PREFIX="/usr/local"
fi

# === PATH Management ===
#
# The PATH environment variable determines where the shell looks for commands.
# We add various directories to it, ensuring no duplicates with typeset -U.

# Ensure no duplicate entries in PATH
typeset -U path

# Add directories to PATH in order of precedence (first has highest priority)
path=(
  "$HOME/bin"                          # User's personal bin directory for custom scripts
  "$HOMEBREW_PREFIX/bin"               # Homebrew on any platform
  "/usr/local/bin"                     # System binaries and other user programs
  "$HOME/.local/bin"                   # Python tools installed by pipx
  "$HOME/.lmstudio/bin"                # LM Studio CLI tools
  $path                                # Existing path entries
)

# --- Language-Specific Path Additions ---

# Language-specific paths can be added here as needed

# === Homebrew Configuration ===
#
# Homebrew is a package manager for macOS and Linux.
# This adds Homebrew to PATH and sets up its environment variables.

if command -v brew &>/dev/null; then
  eval "$(brew shellenv)"
fi

# === Programming Language Environments ===

# --- Python: Pyenv ---
# Pyenv allows managing multiple Python versions
if [ -d "$HOME/.pyenv" ]; then
  export PYENV_ROOT="$HOME/.pyenv"
  path=("$PYENV_ROOT/shims" $path)
  if command -v pyenv &>/dev/null; then 
    eval "$(pyenv init -)"
  fi
fi

# --- Node.js: NVM ---
# Node Version Manager for managing multiple Node.js versions
export NVM_DIR="$HOME/.nvm"
if [ -s "$HOMEBREW_PREFIX/opt/nvm/nvm.sh" ]; then
  source "$HOMEBREW_PREFIX/opt/nvm/nvm.sh"
fi
if [ -s "$HOMEBREW_PREFIX/opt/nvm/etc/bash_completion.d/nvm" ]; then
  source "$HOMEBREW_PREFIX/opt/nvm/etc/bash_completion.d/nvm"
fi


# === Editor Configuration ===
#
# Set the default editor for commands like git commit, crontab, etc.

if command -v emacs &>/dev/null; then
  export EDITOR=$(which emacs)
fi

# === Command Enhancement ===

# --- Grep: Colors and Default Options ---
# Make grep output more readable with color and exclude common unwanted directories
export GREP_COLOR='01;31'  # Red color for matching text
export GREP_OPTIONS='--exclude-dir=.svn --exclude-dir=.svn_base --exclude-dir=.temp --exclude-dir=build/ --color=auto'

# === History Control ===
#
# These variables control how bash history behaves (used by some scripts that source this file)
export HISTCONTROL=erasedups   # Don't store duplicate commands
export HISTSIZE=10000          # Store 10000 commands in memory
export HISTFILESIZE=10000      # Store 10000 commands on disk