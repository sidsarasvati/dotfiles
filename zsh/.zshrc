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
# A collection of awesome prompts for different styles and needs.
# Uncomment the one you want to use and comment out the others.

# Load version control information
autoload -Uz vcs_info
setopt prompt_subst                    # Allow substitution in prompt

# Setup prompt timer functionality (for prompt option 4)
function preexec() {
  timer=$(date +%s)
}

# Get git branch for prompt - returns empty string if not in a git repo
function git_prompt_info() {
  local ref
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "${ref#refs/heads/}"
}

# Precmd function for prompt
function precmd() {
  # Calculate command execution time (for prompt option 4)
  if [ $timer ]; then
    now=$(date +%s)
    elapsed=$(($now - $timer))
    export RPROMPT="%F{cyan}${elapsed}s %(?:%F{green}✓:%F{red}✗)%f"
    unset timer
  fi
}

# ------------------------------
# PROMPT OPTIONS - Choose one by uncommenting
# ------------------------------

# === OPTION 1: Minimal Path-Focused with Lambda ===
# Clean, with focus only on what matters - the path and git status
# PROMPT='%F{cyan}%~%f$(branch=$(git_prompt_info); [[ -n $branch ]] && echo "%F{yellow} (%F{red}$branch%F{yellow})")
# %F{magenta}λ%f '

# === OPTION 2: Developer Pro Prompt (Default) ===
# More compact but super informative, with a dedicated line for commands
PROMPT=$'%F{blue}╭─%f %F{cyan}%~%f$(branch=$(git_prompt_info); [[ -n $branch ]] && echo "%F{yellow} (%F{red}$branch%F{yellow})")
%F{blue}╰─%f %F{green}❯%f '

# === OPTION 3: AI-Coder Vibe Prompt ===
# Modern feel with unique symbols, perfect for coding sessions
# PROMPT=$'%F{magenta}%~%f$(branch=$(git_prompt_info); [[ -n $branch ]] && echo "%F{yellow} (%F{cyan}$branch%F{yellow})")
# %F{green}⟩%f '

# === OPTION 4: Informativity + Style ===
# Commands on second line with execution time and return status shown at right
# PROMPT=$'%F{blue}%~%f$(branch=$(git_prompt_info); [[ -n $branch ]] && echo "%F{yellow} (%F{red}$branch%F{yellow})")
# %F{magenta}❯%f '

# === OPTION 5: Classic with Lambda ===
# Similar to the original style but with lambda and cleaner formatting
# PROMPT='%F{cyan}%~%f$(branch=$(git_prompt_info); [[ -n $branch ]] && echo "%F{yellow} (%F{red}$branch%F{yellow})") %F{magenta}λ%f '

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