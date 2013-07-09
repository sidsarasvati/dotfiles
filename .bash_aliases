#OS detection 
#TODO - try usign uname instead... confirm if it works on windows and linux
OS=${OSTYPE//[0-9.]/}

#OS Specific
if [ "$OS" == 'darwin' ]; then
alias em='open -a /Applications/Emacs.app'
fi


#GENERAL
alias em=emacs
alias g=grep

#LIST#
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

#SVN
alias dif='svn diff | less'
alias ss='svn stat'

#GIT

#VIVOX SPECIFICS