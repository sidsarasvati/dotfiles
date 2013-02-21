#Platform detection 
OS=${OSTYPE//[0-9.]/}

# echo ${OS}

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
. ~/.bash_aliases
fi

#default editor
export EDITOR=$(which emacs)

#grep highlighting 
export GREP_OPTIONS='--color=auto' GREP_COLOR='01;31'

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
