#Platform detection
OS=${OSTYPE//[0-9.]/}

# echo ${OS}

#Add local bins to path to use non-dafault system tools (like grep latest version)
PATH="$HOME/bin:/usr/local/bin:$PATH"

# Addd homebrew and associated env variables to path
# TODO - check for brew before running
eval "$(/opt/homebrew/bin/brew shellenv)"


# Homebrew: Python
export PATH="/opt/homebrew/opt/python/libexec/bin:$PATH"

# ruby version from brew
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/ruby/lib"
export CPPFLAGS="-I/opt/homebrew/opt/ruby/include"
# -- ruby

# copied from brew install nvm
export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
# -- nvm

export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools

export JAVA_HOME=$(/usr/libexec/java_home -v 11)

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

if [[ `uname` == 'Darwin' ]]; then
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

