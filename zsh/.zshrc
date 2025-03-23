##############################################################################
#                                                                            #
#                      Main Zsh Configuration File                           #
#                                                                            #
#            A literate, standalone Zsh configuration without                #
#                     dependencies on external frameworks                    #
#                                                                            #
##############################################################################

# === History Configuration ===
# 
# Zsh's history system is powerful and configurable. These settings optimize it
# for daily use, avoiding duplicates and providing easy history navigation.

# Store history in this file
HISTFILE=~/.zsh_history

# Keep 10000 lines of history within the shell
HISTSIZE=10000       # Maximum events for internal history
SAVEHIST=10000       # Maximum events in the history file

# History options
setopt append_history         # Sessions append their history list to the history file, rather than replace it
setopt extended_history       # Save each command's beginning timestamp and the duration to the history file
setopt hist_expire_dups_first # Expire duplicate entries first when trimming history
setopt hist_ignore_dups       # Don't record an entry that was just recorded again
setopt hist_ignore_space      # Don't record an entry starting with a space
setopt hist_verify            # Show history expansion before executing it
setopt inc_append_history     # Add commands to history as they are entered, not at shell exit
setopt share_history          # Share history between all sessions (reading and writing)

# === Directory Navigation ===
#
# These options make directory navigation more efficient by reducing keystrokes
# and maintaining a directory stack you can navigate through.

setopt auto_cd                # If a command isn't found, but is a directory, cd to it
setopt auto_pushd             # Make cd push the old directory onto the directory stack
setopt pushd_ignore_dups      # Don't push multiple copies of the same directory onto the stack
setopt pushd_silent           # Don't print the directory stack after pushd or popd

# === Completion System ===
#
# Zsh's completion system is one of its strongest features. These settings
# make it more powerful and user-friendly.

# Load the completion system
autoload -Uz compinit && compinit

# Completion styling
zstyle ':completion:*' menu select                         # Use a menu for completion options
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # Case insensitive completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"    # Colored completion (like ls)
zstyle ':completion:*' verbose true                        # Verbosity helps understand what's happening

# Completion options
setopt complete_in_word       # Allow completion from within a word/phrase
setopt always_to_end          # Move cursor to end of word after completion
setopt auto_menu              # Show completion menu on a successive tab press
setopt complete_aliases       # Complete aliases too

# === Environment Variables ===
#
# Load environment variables from a separate file to keep this file cleaner
# and make environment changes easier to find.

if [[ -f ${ZDOTDIR:-$HOME/.dotfiles/zsh}/env.zsh ]]; then
  source ${ZDOTDIR:-$HOME/.dotfiles/zsh}/env.zsh
fi

# === Keybindings (Emacs-style) ===
#
# These keybindings make editing commands more efficient with Emacs-style shortcuts.
# If you're used to Emacs or macOS text editing, these will feel natural.

# Enable Emacs keybinding mode (rather than vi mode)
bindkey -e

# Navigation - moving by words, lines, history
bindkey "^[b" backward-word      # Alt + B → Move cursor back one word
bindkey "^[f" forward-word       # Alt + F → Move cursor forward one word
bindkey "^A" beginning-of-line   # Ctrl + A → Beginning of line
bindkey "^E" end-of-line         # Ctrl + E → End of line

# Editing - delete words backward/forward
bindkey "^[^H" backward-kill-word  # Alt + Backspace → Delete previous word
bindkey "^[d" kill-word            # Alt + D → Delete next word

# History searching - find commands as you type
bindkey "^P" history-search-backward  # Ctrl + P → Previous command matching current input
bindkey "^N" history-search-forward   # Ctrl + N → Next command matching current input
bindkey "^R" history-incremental-search-backward  # Ctrl + R → Incremental search backward in history

# === Prompt Configuration ===
#
# A clean, informative prompt that shows:
# - Username and hostname
# - Current directory
# - Git branch (when in a git repository)
# - Different colors for better visual parsing

# Load version control information
autoload -Uz vcs_info
precmd() { vcs_info }

# Configure which VCS info to show and how
zstyle ':vcs_info:git:*' formats '%b'  # Just show branch name
setopt prompt_subst                    # Allow substitution in prompt

# Prompt shows: username@host:directory(git_branch)$
# Colors: username/host (green), directory (blue), git branch (red)
PROMPT='%F{green}%n@%m%f:%F{blue}%~%f%F{yellow}${vcs_info_msg_0_:+(%f%F{red}${vcs_info_msg_0_}%f%F{yellow})}%f$ '

# === Custom Function Loading ===
#
# Load all custom functions from the functions directory.
# Using separate files for each function (or related functions)
# makes organization and maintenance easier.

if [[ -d ${ZDOTDIR:-$HOME/.dotfiles/zsh}/functions ]]; then
  for function_file in ${ZDOTDIR:-$HOME/.dotfiles/zsh}/functions/*.zsh; do
    [[ -r "$function_file" ]] && source "$function_file"
  done
  unset function_file  # Clean up after ourselves
fi

# === Aliases ===
#
# Load aliases from a separate file to keep this file cleaner and
# make alias changes easier to find.

if [[ -f ${ZDOTDIR:-$HOME/.dotfiles/zsh}/aliases.zsh ]]; then
  source ${ZDOTDIR:-$HOME/.dotfiles/zsh}/aliases.zsh
fi

# === Local Machine-Specific Configuration ===
#
# This file is NOT tracked by git and can be used for machine-specific settings,
# secrets, or other configurations that shouldn't be shared.

if [[ -f ${ZDOTDIR:-$HOME/.dotfiles/zsh}/local.zsh ]]; then
  source ${ZDOTDIR:-$HOME/.dotfiles/zsh}/local.zsh
fi

# === Platform-Specific Configuration ===
#
# Different operating systems may need different settings.
# This section detects the OS and applies appropriate settings.

case $(uname) in
  Darwin)
    # macOS specific settings
    # (Currently none - handled in env.zsh and aliases.zsh)
    ;;
  Linux)
    # Linux specific settings
    # (Currently none - handled in env.zsh and aliases.zsh)
    ;;
esac

# === End of Configuration ===
#
# This file loads other components:
# 1. env.zsh - Environment variables and path configuration
# 2. functions/*.zsh - Individual function definitions
# 3. aliases.zsh - Command shortcuts organized by category
# 4. local.zsh - Machine-specific settings (optional, not in git)