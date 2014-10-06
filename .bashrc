#Platform detection
OS=${OSTYPE//[0-9.]/}

# echo ${OS}

#Add local bin to path to use non-dafault system tools (like grep latest version)
PATH="/usr/local/bin:$PATH"

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
. ~/.bash_aliases
fi
# hg auto complete
if [ -f ~/.hgrc.d/hg_completion.bash ]; then
. ~/.hgrc.d/hg_completion.bash
fi
# Org. Specific definitions.
#N.B This should called last to override any general behavior
if [ -f ~/.bashrc_org ]; then
. ~/.bashrc_org
fi

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
shopt -s histappend

# Colorize the prompt
export CLICOLOR=1
export LSCOLORS=ExFxCxDxbxegedabagacad

PS1_CLOCK="\[\e[37m\][\[\e[1;31m\]\@\\[\e[37m\]]"
PS1_PATH="\[\e[37m\][\[\e[1;34m\]\h:\w\[\e[37m\]]"
PS1_ERROR_CHECK="\`if [ \$? != 0 ]; then echo \[\e[33m\]---=== \[\e[31m\]Oh noes, bad command \[\e[33m\]===---; fi\`"
function hg_ps1
{
    hg prompt "HG:{[+{incoming|count}]->}{root|basename}{/{branch}}{status|modified|unknown}{->[+{outgoing|count}]}{at {bookmark}}" 2> /dev/null
}
PS1_HG='\[\e[37m\]{\[\e[32m\]$(hg_ps1)\[\e[37m\]}'
PS1="$PS1_ERROR_CHECK\n$PS1_CLOCK $PS1_HG $PS1_PATH\n\[\e[0;32m\]\$\[\e[0m\] "
