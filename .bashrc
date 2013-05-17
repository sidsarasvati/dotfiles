#Platform detection 
OS=${OSTYPE//[0-9.]/}

# echo ${OS}

#Add local bin to path to use non-dafault system tools (like grep latest version)
PATH="/usr/local/bin:$PATH"

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
. ~/.bash_aliases
fi

#default editor
export EDITOR=$(which emacs)

#GREP
export GREP_COLOR='01;31'
export GREP_OPTIONS='--exclude-dir=.svn --exclude-dir=.svn_base --exclude-dir=.temp --exclude-dir=build/ --color=auto'

#auto corrects case
shopt -s nocaseglob

#bash history commands
export HISTCONTROL=erasedups
export HISTSIZE=10000
shopt -s histappend

# Colorize the prompt
export CLICOLOR=1
export LSCOLORS=ExFxCxDxbxegedabagacad

PS1="\`if [ \$? != 0 ]; then echo \[\e[33m\]---=== \[\e[31m\]Oh noes, bad command \[\e[33m\]===---; fi\`\n\[\e[0;37m\][\[\e[1;31m\]\@\[\e[0;37m\]] \[\e[0;37m\][\[\e[1;34m\]\w\[\e[0;37m\]] \[\e[0;32m\]\$ \[\e[0m\] "
