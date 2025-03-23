# Source OS detection and Homebrew prefix from env.zsh to avoid duplication
# This ensures we use the same detection logic across files
if [[ -f "$HOME/.zsh/env.zsh" ]]; then
  # Extract just the platform detection and Homebrew prefix parts
  source "$HOME/.zsh/env.zsh"
else
  # Fallback in case env.zsh isn't available
  #Platform detection
  OS=${OSTYPE//[0-9.]/}

  # === Homebrew Detection ===
  # Detect Homebrew location based on platform and architecture
  if [[ "$OS" == "darwin" ]]; then
    # Check for Apple Silicon Mac
    if [[ "$(uname -m)" == "arm64" ]]; then
      HOMEBREW_PREFIX="/opt/homebrew"
    else
      # Intel Mac
      HOMEBREW_PREFIX="/usr/local"
    fi
  else
    # Default for other platforms
    HOMEBREW_PREFIX="/usr/local"
  fi
fi

#Add local bins to path to use non-default system tools
PATH="$HOME/bin:$HOMEBREW_PREFIX/bin:/usr/local/bin:$PATH"

# Add homebrew and associated env variables to path
# Check for brew before running
if command -v brew &>/dev/null; then
  eval "$(brew shellenv)"
fi

# Pyenv: Python
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PATH"
if which pyenv >/dev/null; then eval "$(pyenv init -)"; fi

# NVM configuration
export NVM_DIR="$HOME/.nvm"
[ -s "$HOMEBREW_PREFIX/opt/nvm/nvm.sh" ] && \. "$HOMEBREW_PREFIX/opt/nvm/nvm.sh"                                       # This loads nvm
[ -s "$HOMEBREW_PREFIX/opt/nvm/etc/bash_completion.d/nvm" ] && \. "$HOMEBREW_PREFIX/opt/nvm/etc/bash_completion.d/nvm" # This loads nvm bash_completion
# -- nvm

# Note: Java configuration removed as it's no longer used

#default editor
export EDITOR=$(which emacs)

#GREP
export GREP_COLOR='01;31'
export GREP_OPTIONS='--exclude-dir=.svn --exclude-dir=.svn_base --exclude-dir=.temp --exclude-dir=build/ --color=auto'

#bash history commands
export HISTCONTROL=erasedups
export HISTSIZE=10000
export HISTFILESIZE=10000

#  BASH ALIASES from bash_aliases #
#OS Specific

#todo: need fix for *nix
if [[ "$OS" == 'darwin' ]]; then
  alias em='open -a /Applications/Emacs.app'
else
  alias em=emacs
fi

############  GENERAL  #############
alias g="grep -i"
alias eg="grep -E -i"
alias env="env | sort"
alias pd="pushd"

if [[ $(uname) == 'Darwin' ]]; then
  alias ls="ls -G"
  # good for dark backgrounds
  export LSCOLORS=gxfxcxdxbxegedabagacad
else
  alias ls="ls --color=auto"
  # good for dark backgrounds
  export LS_COLORS='no=00:fi=00:di=00;36:ln=00;35:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=00;31:'
  # For LS_COLORS template: $ dircolors /etc/DIR_COLORS
fi
alias la='ls -A'
alias ll='ls -Alh'
alias l='ls -Alh'

# print directory structure as tree
alias lsd="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
alias lst='tree' # should be availabe on most variants

# list human readable disk usage for files and folder
alias lsu='du -sh *'

# mkdir -p && cd
function md() {
  mkdir -p $1
  cd $1
}

# GIT
alias dif='git diff'
alias difc='git diff --cached'
alias ss='git stat'
alias sl='git log'
alias glog='log --graph --all --decorate --color'
alias st='status -uall --procelain'
alias gg='git grep'

# DOCKER & KUB
alias d='docker'
alias dc='docker-compose'
alias kb='kubectl'
