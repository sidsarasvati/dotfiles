# Function to get git info with status indicators for prompt
#
# Example usage: $(git_prompt_with_status)
# Output examples: 
#   (master)           - clean branch
#   (master ↑2)        - 2 unpushed commits
#   (master ✱)         - local changes
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
  
  # Check for local changes
  local changes=""
  if ! git diff --quiet 2>/dev/null || ! git diff --staged --quiet 2>/dev/null; then
    changes=" %F{yellow}✱"
  fi
  
  # Output formatted git info
  echo "%F{yellow}(%F{red}${branch}${unpushed}${changes}%F{yellow})"
}