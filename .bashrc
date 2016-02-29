#Platform detection
OS=${OSTYPE//[0-9.]/}

# echo ${OS}

#Add local bins to path to use non-dafault system tools (like grep latest version)
PATH="$HOME/bin:$PATH"

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
. ~/.bash_aliases
fi

# hg auto complete (disabled for now)
################################################
# if [ -f ~/.hgrc.d/hg_completion.bash ]; then #
# . ~/.hgrc.d/hg_completion.bash               #
# fi                                           #
################################################

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
export HISTFILESIZE=10000
shopt -s histappend

# Colorize the prompt
export CLICOLOR=1
export LSCOLORS=ExFxCxDxbxegedabagacad

PS1_CLOCK="\[\e[37m\][\[\e[1;31m\]\@\\[\e[37m\]]"
PS1_PATH="\[\e[37m\][\[\e[1;34m\]\w\[\e[37m\]]"
PS1_HOST="\[\e[37m\][\[\e[1;31m\]\h\\[\e[37m\]]"
PS1_ERROR_CHECK="\`if [ \$? != 0 ]; then echo \[\e[33m\]---=== \[\e[31m\]Oh noes, bad command \[\e[33m\]===---; fi\`"

# use this with caution, as this will painfully slow down you bash dir
# when you have network issues
function hg_ps1
{
    hg prompt "HG:{[+{incoming|count}]->}{branch}{status|modified|unknown}{->[+{outgoing|count}]}{ at {bookmark}}" 2> /dev/null
}
PS1_HG='\[\e[37m\]{\[\e[32m\]$(hg_ps1)\[\e[37m\]}'

# __git_ps1 should be in .git.d/git-completion.bash script
#PS1_GIT='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
PS1_GIT='\[\e[37m\]{\[\e[32m\]$(__git_ps1)\[\e[37m\]}'
GIT_PS1_SHOWDIRTYSTATE=1

#################v1: [clock][SCM][pwd]
#PS1="$PS1_ERROR_CHECK\n$PS1_CLOCK $PS1_HG $PS1_PATH\n\[\e[0;32m\]\$\[\e[0m\] "

#################v2: [host][SCM][pwd]
PS1="$PS1_ERROR_CHECK\n$PS1_HOST $PS1_GIT $PS1_PATH\n\[\e[0;32m\]\$\[\e[0m\] "

#################v3: [host][pwd]
#PS1="$PS1_ERROR_CHECK\n$PS1_HOST $PS1_PATH\n\[\e[0;32m\]\$\[\e[0m\] "

# PATH="/Users/ssarasvati/perl5/bin${PATH+:}${PATH}"; export PATH;
# PERL5LIB="/Users/ssarasvati/perl5/lib/perl5${PERL5LIB+:}${PERL5LIB}"; export PERL5LIB;
# PERL_LOCAL_LIB_ROOT="/Users/ssarasvati/perl5${PERL_LOCAL_LIB_ROOT+:}${PERL_LOCAL_LIB_ROOT}"; export PERL_LOCAL_LIB_ROOT;
# PERL_MB_OPT="--install_base \"/Users/ssarasvati/perl5\""; export PERL_MB_OPT;
# PERL_MM_OPT="INSTALL_BASE=/Users/ssarasvati/perl5"; export PERL_MM_OPT;
