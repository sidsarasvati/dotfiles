#OS detection 
OS=${OSTYPE//[0-9.]/}

#GENERAL
alias em=emacs
alias g=grep
alias l='ls -AG'
alias ll='ls -AGlh'
alias dif='svn diff | less'
alias ss='svn stat'

#OS Specific
if [ "$OS" == 'darwin' ]; then
alias em='open -a /Applications/Emacs.app'
fi

#VIVOX SPECIFICS