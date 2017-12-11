# Link other files
stow bash
stow git
stow emacs
stow misc # all other things that don't fit cleanly in a category


# Link .bash_profile -> .bashrc
rm -f ~/.bash_profile
ln -s ~/.bashrc ~/.bash_profile
