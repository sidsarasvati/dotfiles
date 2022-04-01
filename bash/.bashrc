#Platform detection
OS=${OSTYPE//[0-9.]/}

# echo ${OS}

#Add local bins to path to use non-dafault system tools (like grep latest version)
PATH="$HOME/bin:/usr/local/bin:$PATH"

# Addd homebrew and associated env variables to path
# TODO - check for brew before running
eval "$(/opt/homebrew/bin/brew shellenv)"

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

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools

export JAVA_HOME=$(/usr/libexec/java_home -v 11)

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



[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
