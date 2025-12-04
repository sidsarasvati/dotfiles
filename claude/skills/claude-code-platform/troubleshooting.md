# Claude Code Troubleshooting Guide

Solutions to common issues and error patterns.

## Table of Contents
- [Tool Errors](#tool-errors)
- [Permission Issues](#permission-issues)
- [MCP Problems](#mcp-problems)
- [Git Errors](#git-errors)
- [Performance Issues](#performance-issues)
- [Skills Problems](#skills-problems)
- [General Debugging](#general-debugging)

---

## Tool Errors

### File Not Found

**Symptoms**:
```
Error: File doesn't exist: /path/to/file.ts
```

**Causes**:
- Wrong path
- Typo in filename
- File was deleted
- Working in wrong directory

**Solutions**:
```
1. Use Glob to find similar files:
   Pattern: **/*file-name*

2. Check parent directory:
   Bash: ls /path/to/

3. Verify current directory:
   Bash: pwd

4. Search for file content:
   Grep: "unique content from file"
   Mode: files_with_matches
```

### Read Tool: Empty File Warning

**Symptoms**:
```
Warning: File exists but has empty contents
```

**Causes**:
- File genuinely empty
- File was just created
- File was truncated

**Solutions**:
```
1. Verify file status:
   Bash: ls -lh /path/to/file

2. Check recent changes:
   Bash: git log --follow -- /path/to/file

3. If intentional:
   - Write content to file
   - Or delete if not needed
```

### Edit Tool: String Not Found

**Symptoms**:
```
Error: old_string not found in file
```

**Causes**:
- Typo in old_string
- String modified since Read
- Wrong file
- Whitespace mismatch

**Solutions**:
```
1. Read file again to get current content

2. Check exact string (including whitespace):
   - Copy from Read output AFTER line number
   - Preserve indentation exactly
   - Match newlines

3. If multiple occurrences:
   - Use longer unique string for context
   - Or use replace_all=true

4. Verify file:
   - Bash: git diff /path/to/file
   - Check if file was modified
```

### Edit Tool: String Not Unique

**Symptoms**:
```
Error: old_string appears multiple times
```

**Causes**:
- Pattern repeats in file
- Not enough context

**Solutions**:
```
1. Add more context to make unique:
   Old: "const value = 42"
   Better: "const value = 42;\n  return result;"

2. Use replace_all if intentional:
   replace_all: true

3. Edit in multiple passes:
   - First occurrence with context
   - Then subsequent ones
```

### Bash Tool: Timeout

**Symptoms**:
```
Error: Command exceeded timeout (120000ms)
```

**Causes**:
- Long-running command
- Hung process
- Waiting for input

**Solutions**:
```
1. Increase timeout:
   timeout: 300000  # 5 minutes

2. Run in background:
   run_in_background: true
   Monitor with BashOutput

3. Break into smaller commands:
   - Sequential operations
   - Process in chunks

4. Check if command is hung:
   - Verify command syntax
   - Check if interactive (avoid -i flags)
```

### Bash Tool: Command Failed

**Symptoms**:
```
Error: Command exited with code 1
```

**Causes**:
- Invalid syntax
- Missing dependencies
- Wrong directory
- Insufficient permissions

**Solutions**:
```
1. Check error output carefully

2. Verify command syntax:
   - Test command manually if possible
   - Check for typos

3. Verify environment:
   Bash: which command-name
   Bash: echo $PATH

4. Check directory:
   Bash: pwd
   Bash: ls

5. Try simpler version:
   - Remove complex flags
   - Test basic command
```

---

## Permission Issues

### Tool Requires Approval

**Symptoms**:
```
Tool blocked: Requires user approval
```

**Causes**:
- Tool not in allowedTools
- Tool matches disallowedTools pattern
- Default permission settings

**Solutions**:
```
1. Request approval:
   - Explain why tool is needed
   - Wait for user confirmation

2. Add to allowedTools (in settings.json):
   "allowedTools": [
     "Read(path/to/specific/**)",
     "Bash(git:*)",
     "mcp__linear__get_issue"
   ]

3. Use alternative approach:
   - Different tool that's approved
   - Break down into approved operations

4. Temporary bypass:
   User can approve once for this session
```

### Bash Command Blocked

**Symptoms**:
```
Bash command requires approval
```

**Causes**:
- Command not in approved patterns
- Potentially dangerous operation
- New command pattern

**Solutions**:
```
1. Add pattern to allowedTools:
   "Bash(npm:*)" - All npm commands
   "Bash(git status:*)" - Git status specifically
   "Bash(ls:*)" - Listing operations

2. Explain command purpose:
   - Clear description
   - Why it's safe
   - Expected outcome

3. Use specialized tool instead:
   - Read instead of cat
   - Grep instead of grep
   - Glob instead of find
```

### MCP Tool Blocked

**Symptoms**:
```
MCP tool mcp__service__function blocked
```

**Causes**:
- MCP tools not auto-approved
- Service-specific restrictions

**Solutions**:
```
1. Add to allowedTools:
   "mcp__linear__list_issues",
   "mcp__linear__get_issue",
   "mcp__google-calendar__*"

2. Request approval with context:
   - Why accessing this service
   - What data being fetched
   - Operation purpose
```

---

## MCP Problems

### MCP Server Not Responding

**Symptoms**:
```
Error: MCP server connection timeout
Tool: mcp__service__function unavailable
```

**Causes**:
- Server not configured
- Server crashed
- Network issues
- Authentication failed

**Solutions**:
```
1. Check server status:
   Bash: claude mcp list

2. Test connection:
   Bash: claude mcp test service-name

3. Restart Claude Code:
   - Exit and restart
   - Reloads MCP servers

4. Verify configuration:
   - Check ~/.claude/.mcp.json
   - Verify credentials in env

5. Check logs:
   Bash: claude --debug mcp
```

### MCP Authentication Failed

**Symptoms**:
```
Error: Unauthorized
Error: Invalid API key
```

**Causes**:
- Missing credentials
- Expired token
- Wrong API key
- Insufficient permissions

**Solutions**:
```
1. Verify credentials:
   - Check MCP config
   - Verify API key format
   - Test key manually if possible

2. Regenerate credentials:
   - Get new API key
   - Update MCP config
   - Restart Claude Code

3. Check permissions:
   - Verify API user has correct scopes
   - Check service-specific permissions
```

### MCP Tool Not Found

**Symptoms**:
```
Error: Unknown tool mcp__service__function
```

**Causes**:
- MCP server not loaded
- Wrong tool name
- Server misconfigured

**Solutions**:
```
1. List configured servers:
   Bash: claude mcp list

2. Verify tool naming:
   Pattern: mcp__server-name__function-name
   Check exact spelling

3. Check server config:
   - Verify server is in MCP config
   - Test server connection

4. Restart Claude Code:
   May need reload to detect tools
```

---

## Git Errors

### Git Push Failed

**Symptoms**:
```
Error: failed to push some refs
Error: Updates were rejected
```

**Causes**:
- Remote has changes you don't have
- Branch protection rules
- Authentication issues

**Solutions**:
```
1. Pull first:
   Bash: git pull --rebase

2. Check branch protection:
   - Verify you can push to branch
   - Check if PR required

3. Verify authentication:
   Bash: git remote -v
   Check SSH keys or tokens

4. Force push (only if safe):
   NEVER to main/master
   Only to feature branches you own
   Bash: git push --force-with-lease
```

### Merge Conflict

**Symptoms**:
```
Error: Automatic merge failed
CONFLICT (content): Merge conflict in file.ts
```

**Causes**:
- Concurrent changes
- Divergent branches

**Solutions**:
```
1. Check conflict status:
   Bash: git status

2. View conflicts:
   Read: /path/to/conflicted-file.ts

3. Resolve conflicts:
   - Edit file to resolve markers
   - Choose correct version
   - Test changes

4. Complete merge:
   Bash: git add /path/to/file.ts && git commit
```

### Pre-commit Hook Failed

**Symptoms**:
```
Error: pre-commit hook failed
Files were modified by hook
```

**Causes**:
- Linting failures
- Formatting changes
- Test failures
- Hook modified files

**Solutions**:
```
1. If hook modified files:
   a. Check authorship:
      Bash: git log -1 --format='%an %ae'

   b. Check not pushed:
      Bash: git status

   c. If safe to amend:
      Bash: git add . && git commit --amend --no-edit

   d. Otherwise create new commit:
      Bash: git add . && git commit -m "Apply hook changes"

2. If hook failed tests:
   - Fix the issues
   - Run tests locally
   - Commit again

3. Never skip hooks:
   - Don't use --no-verify
   - Fix the actual issue
```

---

## Performance Issues

### Slow Response Times

**Symptoms**:
```
Operations taking longer than expected
Timeouts occurring
```

**Causes**:
- Sequential operations (should be parallel)
- Reading large files entirely
- Inefficient tool selection
- Too many operations

**Solutions**:
```
1. Parallelize independent operations:
   ✓ Single message, multiple tool calls
   ✗ Sequential messages

2. Use progressive loading:
   - Glob → Grep → Read (narrow down)
   - Read with offset+limit for large files
   - Search before reading

3. Use right tools:
   - Grep not Bash grep
   - Read not Bash cat
   - Glob not Bash find

4. Cache within session:
   - Don't re-read known files
   - Build on previous context
   - Reference prior findings
```

### Context Window Issues

**Symptoms**:
```
Running out of context
Slow loading times
```

**Causes**:
- Loading too much upfront
- Not using progressive disclosure
- Re-reading same content

**Solutions**:
```
1. Progressive disclosure:
   - Metadata first
   - Content on demand
   - Deep dive only when needed

2. Targeted reads:
   - Use offset+limit
   - Read specific sections
   - Grep before reading

3. Mental caching:
   - Remember what you've read
   - Build mental model
   - Reference without re-reading

4. Skills leverage progressive loading:
   - Metadata at startup
   - Full content when relevant
   - References on demand
```

---

## Skills Problems

### Skill Not Loading

**Symptoms**:
```
Skill doesn't appear to be available
Skill knowledge not present
```

**Causes**:
- Skill not in correct directory
- SKILL.md missing or malformed
- Need to restart Claude Code

**Solutions**:
```
1. Verify location:
   Personal: ~/.claude/skills/skill-name/
   Project: .claude/skills/skill-name/

2. Check SKILL.md:
   - Has YAML frontmatter
   - name and description present
   - Valid markdown

3. Restart Claude Code:
   - Exit and restart
   - Skills load at startup

4. Test skill loading:
   - Start new conversation
   - Ask question related to skill
   - Verify knowledge present
```

### YAML Frontmatter Error

**Symptoms**:
```
Skill fails to load
YAML parse error
```

**Causes**:
- Missing triple dashes (---)
- Invalid YAML syntax
- Missing required fields

**Solutions**:
```
Correct format:
---
name: Skill Name
description: Brief description
---

# Skill content starts here

Check for:
- Three dashes before and after
- name: and description: fields
- No tabs (use spaces)
- Valid YAML syntax
```

### Skill Not Auto-Loading

**Symptoms**:
```
Skill exists but not being used
Knowledge not available
```

**Causes**:
- Skill not relevant to task
- Description too narrow
- Progressive disclosure not triggered

**Solutions**:
```
1. Check skill description:
   - Is it clear when skill applies?
   - Is it too narrow?
   - Does it match usage?

2. Explicitly mention skill domain:
   - Ask questions in skill's area
   - Trigger relevance detection

3. Invoke skill directly:
   Tool: Skill
   Command: "skill-name"

4. Improve skill metadata:
   - Better description
   - Clearer scope
   - More trigger keywords
```

---

## General Debugging

### Check Environment

```bash
# Claude Code version
claude --version

# List MCP servers
claude mcp list

# Test MCP connection
claude mcp test server-name

# Check settings
cat ~/.claude/settings.json

# Check skill directories
ls -la ~/.claude/skills/
ls -la .claude/skills/
```

### Enable Debug Mode

```bash
# Start with debug output
claude --debug

# Filter debug categories
claude --debug "api,tools"

# Exclude categories
claude --debug "!statsig,!file"

# MCP debug (deprecated, use --debug)
claude --mcp-debug
```

### Verbose Mode

```bash
# Show detailed operations
claude --verbose
```

### Doctor Command

```bash
# Health check
claude doctor
```

### Common Debugging Steps

**1. Isolate the problem**:
```
- Can you reproduce consistently?
- What exactly triggers the error?
- Does it happen with simpler operations?
```

**2. Check recent changes**:
```
- Did settings change?
- New MCP servers added?
- Updated Claude Code?
```

**3. Test in isolation**:
```
- Try command manually
- Test tool individually
- Verify environment
```

**4. Gather information**:
```
- Exact error message
- Steps to reproduce
- Environment details
- Recent changes
```

**5. Try alternatives**:
```
- Different tool
- Simpler approach
- Break into steps
```

### Getting Help

**Documentation**:
- Claude Code docs: https://docs.claude.com/en/docs/claude-code
- Skills cookbook: https://github.com/anthropics/claude-cookbooks/tree/main/skills
- Community support: https://support.claude.com

**GitHub Issues**:
- Bug reports: https://github.com/anthropics/claude-code/issues
- Feature requests: Include use case and examples

**Debug Information to Include**:
```
- Claude Code version (claude --version)
- Operating system
- Error messages (complete output)
- Steps to reproduce
- Expected vs actual behavior
- Minimal reproduction case
```

---

## Quick Reference: Common Fixes

| Problem | Quick Fix |
|---------|-----------|
| File not found | Use Glob to search, verify path |
| Permission denied | Add to allowedTools in settings |
| Timeout | Increase timeout or run in background |
| MCP not connecting | Restart Claude Code, check config |
| Git push failed | Pull first, check branch protection |
| Edit string not found | Read file again, check whitespace |
| Slow performance | Parallelize operations, use right tools |
| Skill not loading | Check location, restart Claude Code |
| Tool not available | Verify spelling, check configuration |
| Command failed | Check syntax, verify environment |

---

*Troubleshooting guide for Claude Code*
*Version 1.0 - Oct 16, 2025*
