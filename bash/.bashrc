#Platform detection
OS=${OSTYPE//[0-9.]/}

# echo ${OS}

#Add local bins to path to use non-dafault system tools (like grep latest version)
PATH="$HOME/bin:$PATH"

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
. ~/.bash_aliases
fi

# git auto complete
if [ -f ~/.git.d/git-completion.bash ]; then
. ~/.git.d/git-completion.bash
fi

# host completetion
if [ -f ~/.bash.d/host_completion.sh ]; then
. ~/.bash.d/host_completion.sh
fi
# Org. Specific definitions.
#N.B This should called last to override any general behavior


# Company specific settings

############  JOKR  #############
export NVM_DIR="$HOME/.nvm"
  [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && . "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && . "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

#default editor
export EDITOR=$(which emacs)
if [ "$OS" == 'cygwin' ]; then
export EDITOR=emacs
fi

#GREP
export GREP_COLOR='01;31'
export GREP_OPTIONS='--exclude-dir=.svn --exclude-dir=.svn_base --exclude-dir=.temp --exclude-dir=build/ --color=auto'

#auto corrects case
shopt -s nocaseglob

#bash history commands
export HISTCONTROL=erasedups
export HISTSIZE=10000
export HISTFILESIZE=10000
shopt -s histappend

# Colorize the prompt
export CLICOLOR=1
export LSCOLORS=ExFxCxDxbxegedabagacad

PS1_CLOCK="\[\e[37m\][\[\e[1;31m\]\@\\[\e[37m\]]"
PS1_PATH="\[\e[37m\][\[\e[1;34m\]\w\[\e[37m\]]"
PS1_HOST="\[\e[37m\][\[\e[1;31m\]\h\\[\e[37m\]]"
PS1_ERROR_CHECK="\`if [ \$? != 0 ]; then echo \[\e[33m\]---=== \[\e[31m\]Oh noes, bad command \[\e[33m\]===---; fi\`"


# __git_ps1 should be in .git.d/git-completion.bash script
#PS1_GIT='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
PS1_GIT='\[\e[37m\]{\[\e[32m\]$(__git_ps1)\[\e[37m\]}'
GIT_PS1_SHOWDIRTYSTATE=1

#################v1: [clock][SCM][pwd]
#PS1="$PS1_ERROR_CHECK\n$PS1_CLOCK $PS1_GIT $PS1_PATH\n\[\e[0;32m\]\$\[\e[0m\] "

#################v2: [host][SCM][pwd]
PS1="$PS1_ERROR_CHECK\n$PS1_HOST $PS1_GIT $PS1_PATH\n\[\e[0;32m\]\$\[\e[0m\] "

#################v3: [host][pwd]
#PS1="$PS1_ERROR_CHECK\n$PS1_HOST $PS1_PATH\n\[\e[0;32m\]\$\[\e[0m\] "



