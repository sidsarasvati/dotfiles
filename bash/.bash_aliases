#OS Specific

#todo: need fix for *nix
if [ "$OS" == 'darwin' ]; then
alias em='open -a /Applications/Emacs.app'
else
alias em=emacs
fi


############  GENERAL  #############
alias g="grep -i"
alias eg="grep -E -i"
alias env="env | sort"
alias pd=pushd

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
md() {
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

#VAGRANT
alias vag='vagrant'
