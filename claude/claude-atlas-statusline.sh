#!/bin/bash
# Claude Atlas StatusLine
# Project-aware statusline for Claude Code with doom-modeline inspired design
#
# Features:
#   - Per-workspace Atlas project context (set by /wake-project)
#   - Git status with doom-style indicators (◆ modified, ● clean, ○ no-git)
#   - Context window usage %
#   - Unpushed commits count
#
# Design inspired by seagle0128/doom-modeline for Emacs
# State stored in: ~/.claude/project-state/${workspace}.project

input=$(cat)

# Colors (doom-modeline inspired)
YELLOW=$'\033[33m'
GREEN=$'\033[32m'
CYAN=$'\033[36m'
MAGENTA=$'\033[35m'
WHITE=$'\033[97m'
GRAY=$'\033[90m'
RED=$'\033[31m'
RESET=$'\033[0m'

# Extract from Claude Code JSON
cwd=$(echo "$input" | jq -r '.workspace.current_dir')
project_dir=$(echo "$input" | jq -r '.workspace.project_dir // .workspace.current_dir')
model=$(echo "$input" | jq -r '.model.display_name // "Claude"')
context_size=$(echo "$input" | jq -r '.context_window.context_window_size // 200000')
usage=$(echo "$input" | jq '.context_window.current_usage')

# Directory names
project=$(basename "$project_dir")
current=$(basename "$cwd")

# Atlas Project Awareness (PER-WORKSPACE state file)
# Each workspace has its own project context - dotfiles doesn't see logseq's Atlas
atlas_project=""
state_dir="$HOME/.claude/project-state"
state_file="$state_dir/${project}.project"

if [ -f "$state_file" ]; then
    atlas_project=$(cat "$state_file" 2>/dev/null | tr -d '[:space:]')

    # Empty file or "central" = no project (for backwards compat)
    if [ -z "$atlas_project" ] || [ "$atlas_project" = "central" ]; then
        atlas_project=""
    fi

    # Validate project still exists (only for secondbrain-logseq)
    # This prevents showing stale project names
    if [ -n "$atlas_project" ] && [ "$project" = "secondbrain-logseq" ]; then
        if [ ! -d "$project_dir/projects/$atlas_project" ]; then
            # Project folder doesn't exist - stale state, clear it
            atlas_project=""
        fi
    fi
fi

# Git status
cd "$cwd" 2>/dev/null
branch=$(git symbolic-ref --short HEAD 2>/dev/null)
git_segment=""

if [ -n "$branch" ]; then
    # Check for changes (doom's M+ indicator)
    status=$(git status --porcelain 2>/dev/null)
    if [ -n "$status" ]; then
        modified="${YELLOW}◆${RESET}"
        # Status indicator
        if echo "$status" | grep -qE '^ '; then
            change=" ${RED}✱${RESET}"
        elif [ -z "$(echo "$status" | grep -v '^??')" ]; then
            change=" ${YELLOW}✱${RESET}"
        else
            change=" ${GREEN}✱${RESET}"
        fi
    else
        modified="${GREEN}●${RESET}"
        change=""
    fi

    # Unpushed commits
    unpushed=$(git log @{u}..HEAD --oneline 2>/dev/null | wc -l | tr -d ' ')
    unpush_str=""
    [ "$unpushed" -gt 0 ] && unpush_str=" ${CYAN}+${unpushed}${RESET}"

    git_segment="${MAGENTA}${branch}${RESET}${unpush_str}${change}"
else
    modified="${GRAY}○${RESET}"
fi

# Context %
ctx=""
if [ "$usage" != "null" ]; then
    tokens=$(echo "$usage" | jq '.input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens')
    pct=$((tokens * 100 / context_size))
    ctx="${CYAN}${pct}%${RESET}"
fi

# Build output (doom-style layout)
# ◆ [project] workspace  42%    Model  branch +N✱
output="${modified}"

# Show Atlas project context if in project session
if [ -n "$atlas_project" ]; then
    output="${output} ${YELLOW}[${atlas_project}]${RESET}"
fi

# Workspace name
output="${output} ${WHITE}${project}${RESET}"

# Show current dir only if different from project
[ "$current" != "$project" ] && output="${output}  ${GRAY}${current}${RESET}"

# Context and model
output="${output}  ${ctx}    ${CYAN}${model}${RESET}"

# Git (right side)
[ -n "$git_segment" ] && output="${output}  ${git_segment}"

echo "$output"
