# Claude Code Tools - Complete Reference

Comprehensive documentation for all built-in tools available in Claude Code.

## Table of Contents
- [File Operations](#file-operations)
- [Execution Tools](#execution-tools)
- [Search Tools](#search-tools)
- [Documentation Tools](#documentation-tools)
- [Research Tools](#research-tools)
- [Specialized Tools](#specialized-tools)

---

## File Operations

### Read Tool

**Purpose**: Read files from the filesystem

**Parameters**:
- `file_path` (required): Absolute path to file
- `offset` (optional): Line number to start reading from
- `limit` (optional): Number of lines to read

**Usage**:
```
Read file: /path/to/file.ts
Read with offset: /path/to/file.ts (lines 100-200)
```

**Notes**:
- Returns up to 2000 lines by default
- Lines longer than 2000 chars are truncated
- Results use `cat -n` format (line numbers starting at 1)
- Can read images (PNG, JPG) - presented visually
- Can read PDFs - processed page by page
- Can read Jupyter notebooks (.ipynb) - all cells with outputs
- Follows symlinks automatically
- You will receive a reminder if file exists but is empty

**Best Practices**:
- Always provide absolute paths
- For large files, use offset + limit to read in chunks
- Call multiple Read tools in parallel for multiple files
- Check if file exists before reading (use Glob or Bash ls)

### Write Tool

**Purpose**: Create new files or overwrite existing files

**Parameters**:
- `file_path` (required): Absolute path where file will be written
- `content` (required): Complete file content to write

**Critical Rules**:
- MUST use Read tool first if file already exists
- Will fail if you haven't read the file in current conversation
- ALWAYS prefer editing existing files over creating new ones
- NEVER create files unless absolutely necessary
- NEVER proactively create documentation files (*.md, README)
- Only use emojis if user explicitly requests

**Usage**:
```
Write new file at: /path/to/new-file.ts
Content: [full file content]
```

**Notes**:
- Overwrites existing files completely
- No partial writes - must provide full content
- Path must be absolute, not relative

### Edit Tool

**Purpose**: Perform exact string replacements in files

**Parameters**:
- `file_path` (required): Absolute path to file
- `old_string` (required): Exact text to find and replace
- `new_string` (required): Text to replace it with (must be different)
- `replace_all` (optional, default false): Replace all occurrences

**Critical Rules**:
- MUST use Read tool at least once before editing
- Preserve exact indentation from Read tool output (after line number prefix)
- Line number prefix format: spaces + number + tab
- Never include line number prefix in old_string or new_string
- old_string MUST be unique in file (or use replace_all)
- ALWAYS prefer editing existing files to creating new ones

**Usage**:
```
Edit: /path/to/file.ts
Find: "const oldValue = 42;"
Replace with: "const newValue = 100;"
```

**Notes**:
- Edit will FAIL if old_string appears multiple times (unless replace_all=true)
- Use replace_all for renaming variables throughout file
- Whitespace must match exactly
- Empty lines and spacing matter

### Glob Tool

**Purpose**: Fast file pattern matching using glob syntax

**Parameters**:
- `pattern` (required): Glob pattern to match
- `path` (optional): Directory to search in (defaults to current directory)

**Patterns**:
- `**/*.js` - All JavaScript files recursively
- `src/**/*.ts` - All TypeScript files in src/
- `*.{ts,tsx}` - All .ts and .tsx files in directory
- `test_*.py` - All Python test files

**Usage**:
```
Find pattern: **/*.js
Search in: /path/to/directory
```

**Notes**:
- Returns matching file paths sorted by modification time
- Works with any codebase size
- Much faster than find command
- Use this instead of Bash find
- Returns relative paths from search directory

**Best Practices**:
- Always use for finding files by name pattern
- Combine with Grep for "find files containing X"
- Call multiple Glob operations in parallel if independent

### Grep Tool

**Purpose**: Search file contents using regex patterns

**Parameters**:
- `pattern` (required): Regular expression to search for
- `path` (optional): File or directory to search
- `output_mode` (optional): "content" (default) | "files_with_matches" | "count"
- `-i` (optional): Case insensitive search
- `-n` (optional): Show line numbers (requires output_mode: "content")
- `-A` (optional): Lines of context after match (requires output_mode: "content")
- `-B` (optional): Lines of context before match (requires output_mode: "content")
- `-C` (optional): Lines of context before AND after match (requires output_mode: "content")
- `glob` (optional): Filter files by glob pattern (e.g., "*.js")
- `type` (optional): File type filter (e.g., "js", "py", "rust")
- `multiline` (optional, default false): Enable multiline matching where . matches newlines
- `head_limit` (optional): Limit output to first N lines/entries

**Usage Examples**:
```
Search for pattern: "function.*Error"
Output mode: content
Show line numbers: true
```

```
Find files containing: "TODO"
Output mode: files_with_matches
File filter: *.ts
```

**Notes**:
- Uses ripgrep (rg) syntax, not standard grep
- Literal braces need escaping: `interface\\{\\}` to find `interface{}`
- Multiline matching disabled by default (patterns match within lines)
- For cross-line patterns like `struct \\{[\\s\\S]*?field`, use multiline: true
- Default output mode is "files_with_matches" (just paths)

**Best Practices**:
- ALWAYS use Grep tool, never `grep` as Bash command
- Use output_mode: "files_with_matches" first to see which files
- Then use output_mode: "content" on specific files
- Combine with glob parameter to filter file types
- Use head_limit to avoid overwhelming output

### LS Tool

**Purpose**: List directory contents

**Usage**: Referenced in tool permission patterns, but primarily use Bash(ls:*)

**Notes**:
- Prefer Bash tool with ls command
- Useful for checking directory structure
- Auto-approved in most configurations

---

## Execution Tools

### Bash Tool

**Purpose**: Execute shell commands in persistent bash session

**Parameters**:
- `command` (required): Shell command to execute
- `description` (optional but recommended): Clear 5-10 word description
- `timeout` (optional): Timeout in milliseconds (default 120000ms, max 600000ms)
- `run_in_background` (optional): Run command in background, monitor with BashOutput

**Critical Rules for Bash**:
1. **Directory Verification**: Before creating files/dirs, verify parent exists with ls
2. **Quote Paths with Spaces**: Always use double quotes around paths containing spaces
3. **Avoid File Operations**: Use specialized tools (Read/Write/Edit/Grep/Glob) instead
4. **Chain Related Commands**: Use `&&` for sequential dependent commands
5. **Parallel Independent Commands**: Make multiple Bash calls in one message
6. **Never use interactive flags**: No `-i` flags (like `git rebase -i`)

**When to Use Bash**:
- Git operations (add, commit, push, status, diff, log)
- Package management (npm, pip, cargo)
- Process management (ps, kill, lsof)
- System operations (date, env, which)
- Docker/deployment commands

**When NOT to Use Bash**:
- File reading (use Read tool)
- File writing (use Write tool)
- File editing (use Edit tool)
- Content search (use Grep tool)
- File finding (use Glob tool)
- Communication with user (output text directly)

**Quoting Examples**:
```
✓ cd "/Users/name/My Documents"
✗ cd /Users/name/My Documents

✓ python "/path/with spaces/script.py"
✗ python /path/with spaces/script.py
```

**Chaining Commands**:
```
Sequential (depends on previous): git add . && git commit -m "msg" && git push
Parallel (independent): [Multiple Bash tool calls in single message]
Don't care about failures: cmd1 ; cmd2 ; cmd3
```

**Background Execution**:
```
Run in background: run_in_background=true
Monitor: Use BashOutput tool
Never use & at end of command when run_in_background=true
```

**Git Protocol** (when creating commits):
```
Phase 0: Run git status + git diff + git log (in parallel)
Phase 1: Analyze changes, draft commit message
Phase 2: git add relevant files + git commit with HEREDOC message + git status
Phase N: If pre-commit hook modifies files, check authorship & amend if safe

Commit message format using HEREDOC:
git commit -m "$(cat <<'EOF'
Commit message here.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>
EOF
)"
```

**Pull Request Protocol** (when creating PRs):
```
Phase 0: git status + git diff + git log + git diff [base]...HEAD (in parallel)
Phase 1: Analyze ALL commits that will be in PR (not just latest)
Phase 2: Draft PR summary with context, changes, test plan
Phase 3: Push if needed + gh pr create with HEREDOC body

PR body format:
gh pr create --title "title" --body "$(cat <<'EOF'
## Summary
<bullet points>

## Test plan
[Checklist of testing TODOs]

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

**Never Use Bash For**:
- Reading files (that's Read tool)
- Finding files (that's Glob tool)
- Searching content (that's Grep tool)
- Editing files (that's Edit tool)
- Communicating with user (that's text output)

### Task Tool

**Purpose**: Launch specialized subagents for complex multi-step tasks

**Parameters**:
- `subagent_type` (required): Which specialized agent to launch
- `prompt` (required): Detailed autonomous task description
- `description` (required): Short 3-5 word task description

**Available Subagent Types**:
- `general-purpose` - Research, code search, multi-step tasks
- `statusline-setup` - Configure Claude Code status line
- `output-style-setup` - Create Claude Code output styles
- `Explore` - Fast codebase exploration (thoroughness: quick/medium/very thorough)
- `pragmatic-lead` - Pragmatic programming for debug/architect/ship
- `qa` - Browser testing with Playwright MCP
- Plus user-defined agents in `.claude/agents/`

**When to Use Task Tool**:
- Multi-step tasks that require sustained focus
- Deep codebase exploration (use Explore agent)
- When you need specialized expertise
- Complex workflows that benefit from separation

**When NOT to Use Task Tool**:
- Single, direct operations (just do it)
- Simple file reads (use Read tool directly)
- Searching for specific class in 2-3 files (use Read directly)
- Tasks where you are more efficient than agent overhead

**Best Practices**:
- Launch multiple agents in parallel when possible (single message, multiple Task calls)
- Provide highly detailed autonomous prompts
- Specify exactly what information agent should return
- Each invocation is stateless - agent can't communicate back mid-task

**Usage**:
```
Task: Launch Explore agent
Thoroughness: medium
Prompt: "Find all API endpoint definitions in this codebase..."
```

### BashOutput Tool

**Purpose**: Read output from background bash shells

**Parameters**:
- `bash_id` (required): ID of background shell
- `filter` (optional): Regex to filter output lines

**Usage**:
```
Read output from shell: [bash_id]
Filter pattern: [optional regex]
```

**Notes**:
- Returns only NEW output since last check
- Use with Bash tool's run_in_background parameter
- Find bash_id using /bashes command
- Filtered lines are no longer available after filtering

### KillShell Tool

**Purpose**: Terminate running background bash shells

**Parameters**:
- `shell_id` (required): ID of shell to terminate

**Usage**: Kill background shell [shell_id]

**Notes**: Use /bashes command to find shell IDs

---

## Search Tools

(Covered above: Grep and Glob)

---

## Documentation Tools

### TodoWrite Tool

**Purpose**: Create and manage structured task lists for session

**Parameters**:
- `todos` (required): Array of todo objects

**Todo Object Structure**:
```json
{
  "content": "Task description in imperative form",
  "status": "pending" | "in_progress" | "completed",
  "activeForm": "Present continuous form (e.g., 'Running tests')"
}
```

**When to Use TodoWrite**:
- Complex multi-step tasks (3+ distinct steps)
- Non-trivial tasks requiring planning
- User explicitly requests todo list
- User provides multiple tasks (numbered or comma-separated)
- Track progress through complex implementation

**When NOT to Use TodoWrite**:
- Single straightforward task
- Trivial tasks (tracking provides no benefit)
- Purely conversational/informational tasks
- Can be completed in <3 trivial steps

**Critical Rules**:
- Exactly ONE task must be in_progress at any time
- Mark tasks completed IMMEDIATELY after finishing
- Don't batch multiple completions
- Remove tasks that become irrelevant entirely
- ONLY mark completed when FULLY accomplished
- If blocked/errored, keep in_progress and create new task for blocker

**Task States**:
- `pending`: Not started
- `in_progress`: Currently working (limit to ONE)
- `completed`: Finished successfully

**Best Practices**:
- Use two forms: "Run tests" (content) and "Running tests" (activeForm)
- Create specific, actionable items
- Break complex tasks into manageable steps
- Update in real-time as you work

### SlashCommand Tool

**Purpose**: Execute custom user-defined slash commands

**Parameters**:
- `command` (required): The slash command with arguments (e.g., "/wake", "/review-pr 123")

**How It Works**:
- Commands expand to full prompts from `.claude/commands/*.md` files
- See `<command-message>{name} is running…</command-message>` after invocation
- Expanded prompt appears in next message

**Available Commands**: Listed in system prompt (varies by project)

**Critical Rules**:
- ONLY use for custom slash commands in Available Commands list
- DO NOT use for built-in CLI commands (/help, /clear)
- DO NOT guess commands not in the list
- DO NOT invoke command that's already running
- Process expanded prompt after invocation

**Best Practices**:
- Check command list before use
- When user requests multiple commands, execute sequentially
- Wait for command-message confirmation before processing

### Skill Tool

**Purpose**: Execute user-defined skills within conversation

**Parameters**:
- `command` (required): Skill name only, no arguments (e.g., "pdf", "xlsx")

**How It Works**:
- Skills provide specialized capabilities and domain knowledge
- Loaded from skill marketplace or custom skills
- Claude auto-loads relevant skills (no explicit invoke needed)
- Can use fully qualified names (e.g., "ms-office-suite:pdf")

**Available Skills**: Listed in system message (varies by installation)

**Critical Rules**:
- Only use skills in available_skills list
- Do not invoke skill already running
- Do not use for built-in CLI commands
- Skill name only - no arguments in command parameter

**Notes**:
- Skills are the mechanism THIS skill uses
- Skills enable portable expertise across Claude environments

---

## Research Tools

### WebFetch Tool

**Purpose**: Fetch and analyze web content with AI processing

**Parameters**:
- `url` (required): Fully-formed valid URL
- `prompt` (required): What information to extract from page

**Usage**:
```
Fetch: https://example.com/article
Extract: "Summarize the key technical details"
```

**Features**:
- Fetches URL content
- Converts HTML to markdown
- Processes with fast AI model
- Returns analysis based on prompt

**Notes**:
- HTTP URLs auto-upgrade to HTTPS
- Read-only (doesn't modify files)
- Results may be summarized for large content
- 15-minute self-cleaning cache (repeated URLs fast)
- Handles redirects (will inform you, make new request)
- IMPORTANT: Prefer MCP web fetch tools if available (fewer restrictions)

**Best Practices**:
- Use specific prompts ("Extract API endpoints" not "Summarize")
- MCP fetch tools (mcp__*__fetch) often better when available
- Cache helps when repeatedly accessing same URL

### WebSearch Tool

**Purpose**: Search the web for current information

**Parameters**:
- `query` (required): Search query (min 2 characters)
- `allowed_domains` (optional): Only include results from these domains
- `blocked_domains` (optional): Never include results from these domains

**Usage**:
```
Search: "Claude Code new features 2025"
Domains: Allow anthropic.com, docs.claude.com
```

**Notes**:
- Provides up-to-date information beyond training cutoff
- Returns formatted search result blocks
- Only available in US
- Account for "Today's date" in env (don't search 2024 for "latest" in 2025)

**Best Practices**:
- Use for information beyond training knowledge
- Domain filtering helps focus results
- Check <env> for current date before forming queries

---

## Specialized Tools

### NotebookEdit Tool

**Purpose**: Edit specific cells in Jupyter notebooks (.ipynb files)

**Parameters**:
- `notebook_path` (required): Absolute path to .ipynb file
- `cell_id` (required for replace/delete): ID of cell to edit
- `new_source` (required): New content for the cell
- `cell_type` (optional): "code" or "markdown" (required for insert)
- `edit_mode` (optional): "replace" (default), "insert", or "delete"

**Usage**:
```
Edit notebook: /path/to/notebook.ipynb
Cell: [cell_id]
New content: [code or markdown]
```

**Edit Modes**:
- `replace`: Replace cell content entirely (default)
- `insert`: Add new cell after specified cell_id (or at beginning)
- `delete`: Remove the cell

**Notes**:
- Notebooks combine code, text, and visualizations
- Common for data analysis and scientific computing
- Cell IDs are 0-indexed
- Read notebook first to see structure

---

## MCP Integration Tools

Tools prefixed with `mcp__` come from Model Context Protocol servers.

**Common MCP Tools**:
- `mcp__linear__*` - Linear project management operations
- `mcp__google-calendar__*` - Google Calendar operations
- `mcp__stripe__*` - Stripe payment operations
- `mcp__appstore-connect__*` - App Store Connect metrics
- `mcp__atlassian__*` - Atlassian/Confluence operations
- `mcp__playwright__*` - Browser testing with Playwright

**MCP Tool Naming Pattern**:
```
mcp__[server-name]__[function-name]
```

**For detailed MCP information**, see mcp-integration.md

---

## Tool Selection Guide

**I need to...**
- Read a file → **Read tool**
- Create a file → **Write tool** (Read first if exists)
- Modify a file → **Edit tool** (Read first always)
- Find files by name → **Glob tool**
- Search file contents → **Grep tool**
- Run git command → **Bash tool**
- Install packages → **Bash tool**
- Launch complex task → **Task tool**
- Track progress → **TodoWrite tool**
- Fetch web page → **WebFetch tool** (or MCP if available)
- Search web → **WebSearch tool**
- Edit Jupyter notebook → **NotebookEdit tool**
- Use external service → **MCP tool** (mcp__service__function)

**General Rule**: Specialized tools > Bash commands for file operations

---

## Performance Optimization

**Parallel Execution**:
When multiple operations are independent, call tools in parallel:
```
✓ Read file1.ts + Read file2.ts + Read file3.ts (all at once)
✗ Read file1.ts → wait → Read file2.ts → wait → Read file3.ts
```

**Context Efficiency**:
- Use offset + limit for large files
- Use files_with_matches before reading contents
- Load only what you need
- Cache results mentally for session duration

**Tool Approval**:
- Check approval patterns before using tools
- Some tools auto-approved (Read, common Bash patterns)
- Reduces interruptions and speeds workflow

---

*Complete tool reference for Claude Code platform*
*Version 1.0 - Oct 16, 2025*
