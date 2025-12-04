#!/bin/bash
# Minimalist Claude Code StatusLine with Session Tracking
# Inspired by Steve Jobs design philosophy: extreme simplicity and elegance

# Read JSON input from Claude Code
input=$(cat)

# Extract essential information only
cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
model_name=$(echo "$input" | jq -r '.model.display_name // .model.id // "Claude"')
session_id=$(echo "$input" | jq -r '.session_id // ""')

# Get current directory (basename only for clean presentation)
current_dir=$(basename "$cwd")

# Git information (essential only)
cd "$cwd" 2>/dev/null || cd /
git_branch=$(git branch --show-current 2>/dev/null)
git_status=""

if [ -n "$git_branch" ]; then
    # Simple git status check
    if ! git diff-index --quiet HEAD -- 2>/dev/null; then
        git_status="*"  # Modified
    elif [ -n "$(git status --porcelain 2>/dev/null)" ]; then
        git_status="+"  # Untracked
    fi
fi

# Clean model name (first word only for brevity)
clean_model=$(echo "$model_name" | awk '{print $1}')

# Session tracking for Claude Max (5-hour blocks)
get_session_info() {
    local session_file="$HOME/.claude/session_tracking"
    local current_time=$(date +%s)
    local session_start=""
    local token_usage=""
    
    # Read ACTUAL token counts from ALL Claude projects (5-hour window)
    # Following ccusage's approach: sum input + output tokens only for rate limits
    local total_tokens=0
    local five_hours_ago=$((current_time - 18000))  # 5 hours in seconds
    
    # Scan ALL project directories for recent JSONL files
    for project_dir in "$HOME/.claude/projects"/*; do
        if [ -d "$project_dir" ]; then
            # Find JSONL files modified in last 5 hours
            for jsonl_file in "$project_dir"/*.jsonl; do
                if [ -f "$jsonl_file" ]; then
                    # Check if file was modified within 5-hour window
                    local file_mtime=$(stat -f %m "$jsonl_file" 2>/dev/null || stat -c %Y "$jsonl_file" 2>/dev/null || echo "0")
                    
                    if [ "$file_mtime" -gt "$five_hours_ago" ]; then
                        # Count ALL tokens including cache_read since Claude is warning about limits
                        # Documentation says "in some instances, cache_read_input_tokens are counted"
                        local session_tokens=$(cat "$jsonl_file" 2>/dev/null | \
                            jq -s '[.[] | select(.type == "assistant" and .message.usage != null) | .message.usage | 
                                   (.input_tokens // 0) + 
                                   (.output_tokens // 0) + 
                                   (.cache_creation_input_tokens // 0) + 
                                   (.cache_read_input_tokens // 0)] | add // 0' 2>/dev/null)
                        
                        if [ -n "$session_tokens" ] && [ "$session_tokens" != "null" ] && [ "$session_tokens" -gt 0 ]; then
                            total_tokens=$((total_tokens + session_tokens))
                        fi
                    fi
                fi
            done
        fi
    done
    
    # Calculate estimated cost (using approximate Opus 4 rates)
    if [ $total_tokens -gt 0 ]; then
        # Rough cost estimation for Opus 4: ~$3 per 1M tokens (mixed input/output)
        local cost_cents=$(( (total_tokens * 300) / 1000000 ))  # $3 = 300 cents
        local cost_dollars=$((cost_cents / 100))
        local cost_decimal=$((cost_cents % 100))
        
        # Warning thresholds based on your experience: $100 warning, $130-150 limit
        local warning_threshold=10000  # $100 in cents
        local limit_threshold=13000    # $130 in cents
        
        if [ $cost_cents -gt $limit_threshold ]; then
            token_usage="!\$${cost_dollars}.$(printf "%02d" $cost_decimal)"
        elif [ $cost_cents -gt $warning_threshold ]; then
            token_usage="~\$${cost_dollars}.$(printf "%02d" $cost_decimal)"
        else
            token_usage="\$${cost_dollars}.$(printf "%02d" $cost_decimal)"
        fi
        
        # Add percentage if we want to keep that
        local percentage=$(( (cost_cents * 100) / limit_threshold ))
        token_usage="${token_usage}/${percentage}%"
    else
        token_usage="--"
    fi
    
    # Calculate time remaining in 5-hour block (across ALL projects)
    local reset_timer=""
    local earliest_session=$current_time
    
    # Find earliest session start within 5-hour window across all projects
    for project_dir in "$HOME/.claude/projects"/*; do
        if [ -d "$project_dir" ]; then
            for jsonl_file in "$project_dir"/*.jsonl; do
                if [ -f "$jsonl_file" ]; then
                    local file_mtime=$(stat -f %m "$jsonl_file" 2>/dev/null || stat -c %Y "$jsonl_file" 2>/dev/null || echo "0")
                    
                    # If file is within 5-hour window and earlier than current earliest
                    if [ "$file_mtime" -gt "$five_hours_ago" ] && [ "$file_mtime" -lt "$earliest_session" ]; then
                        earliest_session="$file_mtime"
                    fi
                fi
            done
        fi
    done
    
    # Calculate remaining time in 5-hour block
    if [ "$earliest_session" -lt "$current_time" ]; then
        local elapsed=$((current_time - earliest_session))
        local block_duration=18000  # 5 hours in seconds
        local remaining=$((block_duration - elapsed))
        
        if [ $remaining -gt 0 ]; then
            local hours=$((remaining / 3600))
            local minutes=$(( (remaining % 3600) / 60 ))
            if [ $hours -gt 0 ]; then
                reset_timer="${hours}h ${minutes}m"
            else
                reset_timer="${minutes}m"
            fi
        else
            reset_timer="Reset!"
        fi
    fi
    
    echo "$token_usage|$reset_timer"
}

# Get session information directly (no subshell needed)
session_info=$(get_session_info)
token_usage=$(echo "$session_info" | cut -d'|' -f1)
reset_timer=$(echo "$session_info" | cut -d'|' -f2)

# Build the minimalist status line
# Using echo with -e flag for proper color interpretation
elements=""

# Directory (subtle blue-gray)
elements="${elements}$(printf '\033[38;5;67m')${current_dir}$(printf '\033[0m')"

# Git branch and status (if in git repo)
if [ -n "$git_branch" ]; then
    elements="${elements}  $(printf '\033[38;5;243m')${git_branch}$(printf '\033[0m')"
    if [ -n "$git_status" ]; then
        # Red for modified, yellow for untracked
        if [ "$git_status" = "*" ]; then
            elements="${elements}$(printf '\033[38;5;203m')${git_status}$(printf '\033[0m')"
        else
            elements="${elements}$(printf '\033[38;5;214m')${git_status}$(printf '\033[0m')"
        fi
    fi
fi

# Cost usage and reset timer combined (more elegant)
if [ -n "$reset_timer" ]; then
    # Check cost-based warning levels
    if [[ "$token_usage" == !* ]]; then
        # Red for over-limit ($130+)
        elements="${elements}  $(printf '\033[38;5;203m')${token_usage#!} · ${reset_timer}$(printf '\033[0m')"
    elif [[ "$token_usage" == ~* ]]; then
        # Yellow for warning ($100+)
        elements="${elements}  $(printf '\033[38;5;214m')${token_usage#~} · ${reset_timer}$(printf '\033[0m')"
    else
        # Normal gray for safe levels
        elements="${elements}  $(printf '\033[38;5;243m')${token_usage} · ${reset_timer}$(printf '\033[0m')"
    fi
elif [ -n "$token_usage" ] && [ "$token_usage" != "--" ]; then
    # Just cost if no timer yet
    if [[ "$token_usage" == !* ]]; then
        elements="${elements}  $(printf '\033[38;5;203m')${token_usage#!}$(printf '\033[0m')"
    elif [[ "$token_usage" == ~* ]]; then
        elements="${elements}  $(printf '\033[38;5;214m')${token_usage#~}$(printf '\033[0m')"
    else
        elements="${elements}  $(printf '\033[38;5;243m')${token_usage}$(printf '\033[0m')"
    fi
fi

# Model (very subtle gray)
elements="${elements}  $(printf '\033[38;5;246m')${clean_model}$(printf '\033[0m')"

# Output the final line with proper newline
echo -e "$elements"