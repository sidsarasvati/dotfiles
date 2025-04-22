# Function to get git info with unpushed commits for prompt
#
# Example usage: $(git_prompt_with_status)
# Output: (master ↑2) - when there are 2 unpushed commits
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
  
  # Output formatted git info
  echo "%F{yellow}(%F{red}${branch}${unpushed}%F{yellow})"
}