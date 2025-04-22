# Function to get git info with status indicators for prompt
#
# Example usage: $(git_prompt_with_status)
# Output examples: 
#   (master)           - clean branch
#   (master ↑2)        - 2 unpushed commits
#   (master ✱)         - local changes with color coding:
#                        - Yellow star: untracked files only
#                        - Red star: modified/unstaged files
#                        - Green star: all changes staged
#   (master ↑2 ✱)      - both unpushed commits and local changes
#
function git_prompt_with_status() {
  # Get current branch
  local branch
  branch=$(git symbolic-ref HEAD 2>/dev/null)
  if [[ -z "$branch" ]]; then
    return
  fi
  branch="${branch#refs/heads/}"
  
  # Get unpushed commit count
  local unpushed=""
  local count
  count=$(git log @{u}..HEAD --oneline 2>/dev/null | wc -l | tr -d ' ')
  if [[ $count -gt 0 ]]; then
    unpushed=" %F{magenta}↑%F{white}${count}"
  fi
  
  # Check for different types of changes with color coding:
  # - Yellow star (✱) for untracked files only
  # - Red star (✱) for modified/unstaged files
  # - Green star (✱) for all changes staged
  local changes=""
  local status_output=$(git status --porcelain 2>/dev/null)
  
  if [[ -n "$status_output" ]]; then
    # Check for untracked files only (lines starting with ??)
    if [[ $(echo "$status_output" | grep -v '^??') == "" && $(echo "$status_output" | grep '??') != "" ]]; then
      changes=" %F{yellow}✱"
    # Check for modified/unstaged files (any line starting with space)
    elif echo "$status_output" | grep -E '^ ' > /dev/null; then
      changes=" %F{red}✱"
    # Otherwise (only staged changes, no unstaged)
    else
      changes=" %F{green}✱"
    fi
  fi
  
  # Output formatted git info
  echo "%F{yellow}(%F{red}${branch}${unpushed}${changes}%F{yellow})"
}