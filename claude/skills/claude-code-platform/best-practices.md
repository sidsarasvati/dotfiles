# Claude Code Best Practices

Performance optimization, workflows, and patterns for effective Claude Code usage.

## Table of Contents
- [Performance Optimization](#performance-optimization)
- [Tool Selection](#tool-selection)
- [Workflow Patterns](#workflow-patterns)
- [Error Recovery](#error-recovery)
- [Git Workflows](#git-workflows)
- [Context Management](#context-management)

---

## Performance Optimization

### Parallel Tool Execution

**Pattern**: Execute independent operations simultaneously

**Examples**:
```
Reading multiple files:
✓ Read file1.ts + Read file2.ts + Read file3.ts (all in one message)
✗ Read file1.ts → wait → Read file2.ts → wait

Searching multiple patterns:
✓ Grep pattern1 + Grep pattern2 + Grep pattern3 (parallel)
✗ Sequential Grep calls

Git information gathering:
✓ git status + git diff + git log (parallel)
✗ One at a time
```

**Benefits**:
- Dramatically faster execution (seconds vs minutes)
- Better use of network/IO
- Reduced overall latency

**When to Parallelize**:
- Operations are independent
- No shared state between operations
- Order doesn't matter
- Can process results together

**When NOT to Parallelize**:
- Operations depend on each other
- Need results from first to inform second
- Sequential logic required
- Shared state that could conflict

**Implementation**:
```
Single message with multiple tool calls:
- Tool 1: Read /path/to/file1
- Tool 2: Read /path/to/file2
- Tool 3: Read /path/to/file3

NOT multiple messages:
Message 1: Read file1
[wait for response]
Message 2: Read file2
[wait for response]
```

### Context Efficiency

**Load Only What's Needed**:
```
Large files:
✓ Read with offset+limit for specific sections
✗ Read entire 10,000 line file

Content search:
✓ Grep with files_with_matches first, then read specific files
✗ Read all files hoping to find pattern

Exploration:
✓ Start broad (Glob), narrow down (Grep), then read
✗ Read everything hoping to find it
```

**Progressive Loading**:
```
Phase 1: Glob to find candidates
Phase 2: Grep to filter by content
Phase 3: Read specific matches
Phase 4: Deep analysis
```

**Token Optimization**:
```
Skills use progressive disclosure:
- Metadata loads at startup (~20 tokens)
- Full SKILL.md loads when relevant
- Reference docs load on demand

Apply same pattern to your work:
- Load summaries first
- Deep dive only when needed
- Cache information mentally within session
```

### Caching Within Session

**Mental Cache**:
```
Once you Read a file in a session:
- Remember its structure
- Reference it without re-reading
- Update mental model as you edit

Once you Grep for patterns:
- Remember where things are
- Don't repeat same search
- Build on previous knowledge
```

**When to Re-fetch**:
```
Re-read after:
- You've edited the file
- User mentions changes
- Significant time passed
- Verification needed

Don't re-read for:
- Same information already known
- Asking follow-up questions
- Explaining what you've seen
```

---

## Tool Selection

### The Tool Selection Hierarchy

**1. Specialized Tools First**:
```
File operations:
✓ Read tool for reading files
✗ Bash cat command

✓ Edit tool for modifications
✗ Bash sed command

✓ Grep tool for content search
✗ Bash grep command

✓ Glob tool for file finding
✗ Bash find command
```

**2. Bash for System Operations**:
```
✓ Git operations (add, commit, push, diff)
✓ Package management (npm install, pip install)
✓ Process management (ps, kill, lsof)
✓ System info (date, env, which)
✓ Docker/deployment commands
```

**3. Task Tool for Complex Workflows**:
```
✓ Multi-step autonomous tasks
✓ Deep codebase exploration
✓ Specialized expertise needed

✗ Simple file reads
✗ Single operations
✗ When you can do it faster
```

### Why Specialized Tools Beat Bash

**Permissions**:
```
Read tool: Already approved in settings
cat via Bash: Needs approval

Grep tool: Fast built-in permission
grep via Bash: Check approval patterns
```

**Functionality**:
```
Read tool:
- Supports offset+limit
- Handles images/PDFs
- Jupyter notebook support
- Line number formatting

cat via Bash:
- Basic text output only
```

**Performance**:
```
Grep tool:
- Optimized ripgrep engine
- Built-in file filtering
- Output modes (content/files/count)
- Context lines support

grep via Bash:
- Standard grep
- Manual piping needed
- Less flexible output
```

**Error Handling**:
```
Specialized tools:
- Structured error messages
- Type-safe parameters
- Built-in validation

Bash commands:
- Text-based errors
- Manual validation
- Parsing required
```

---

## Workflow Patterns

### File Investigation Workflow

**Pattern**: Start broad, narrow down

```
Step 1: Glob - Find potential files
Pattern: **/*.{ts,tsx}
Result: List of TypeScript files

Step 2: Grep - Search for pattern
Pattern: "class.*Component"
Mode: files_with_matches
Result: Files containing pattern

Step 3: Read - Examine specific files
Files: Only the 3-5 matches
Analysis: Deep understanding

Step 4: Edit - Make changes
Target: Specific locations identified
```

### Code Change Workflow

**Pattern**: Understand → Plan → Implement → Verify

```
Phase 0: Understand
- Read affected files
- Grep for related code
- Review recent changes (git log)

Phase 1: Plan
- Identify change points
- Consider side effects
- Check test coverage

Phase 2: Implement
- Edit files (prefer Edit over Write)
- Make minimal changes
- Preserve existing patterns

Phase 3: Verify
- Run tests if available
- Check syntax (build command)
- Review git diff
- Confirm behavior
```

### Research Workflow

**Pattern**: Search → Aggregate → Synthesize

```
Step 1: Multiple parallel searches
- Glob for file patterns
- Grep for code patterns
- WebSearch for external info
- Read documentation

Step 2: Aggregate findings
- Combine results
- Identify patterns
- Note connections

Step 3: Synthesize
- Draw conclusions
- Answer question
- Provide recommendations
```

### Debugging Workflow

**Pattern**: Reproduce → Isolate → Fix → Verify → Prevent

```
Phase 0: Reproduce
- Understand bug report
- Identify reproduction steps
- Confirm bug exists

Phase 1: Isolate
- Find relevant code (Grep/Glob)
- Read implementation
- Check recent changes (git log)
- Test hypotheses

Phase 2: Fix
- Implement solution
- Make minimal changes
- Preserve existing behavior

Phase 3: Verify
- Test fix works
- Check edge cases
- Run test suite

Phase N: Prevent
- Add test to catch regression
- Update error handling
- Document fix
```

---

## Error Recovery

### Handle Tool Failures Gracefully

**Pattern**: Try → Catch → Recover → Continue

```
Tool Call Fails:
1. Acknowledge failure
2. Explain what happened
3. Provide alternative approach
4. Ask for clarification if needed

Examples:
- File not found → Check path, suggest alternatives
- Permission denied → Suggest adding to allowedTools
- Timeout → Retry with longer timeout or break into chunks
- MCP error → Check connection, suggest troubleshooting
```

### Common Error Patterns

**File Not Found**:
```
Error: File doesn't exist
Recovery:
1. Use Glob to find similar files
2. Check parent directory exists
3. Verify path spelling
4. Ask user for correct path
```

**Permission Denied**:
```
Error: Tool requires approval
Recovery:
1. Explain why tool is needed
2. Suggest adding to allowedTools
3. Offer alternative approach
4. Request one-time permission
```

**Timeout**:
```
Error: Command exceeded timeout
Recovery:
1. Increase timeout parameter
2. Break into smaller operations
3. Run in background
4. Use faster alternative
```

**Pattern Not Found**:
```
Error: Grep found no matches
Recovery:
1. Try broader pattern
2. Case-insensitive search
3. Glob for files first
4. Verify pattern syntax
```

### Provide Alternatives

**Never Dead-end**:
```
✗ "File not found. I can't proceed."
✓ "File not found. I can:
   1. Search for similar files
   2. Check the directory structure
   3. Create the file if needed
   What would you like?"

✗ "Permission denied."
✓ "This tool needs approval. I can:
   1. Wait for your approval
   2. Use an alternative approach
   3. Add to allowedTools for future
   Which do you prefer?"
```

---

## Git Workflows

### Commit Creation Protocol

**Phase 0: Information Gathering** (Parallel):
```
git status - See staged and unstaged changes
git diff - See what will be committed
git log -5 - See recent commit style
```

**Phase 1: Analysis**:
```
- Summarize nature of changes
- Check for secrets (.env, credentials)
- Draft concise commit message (why, not what)
- Ensure message matches repository style
```

**Phase 2: Execution** (Sequential):
```
git add [relevant files]
git commit -m "$(cat <<'EOF'
Commit message here.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>
EOF
)"
git status  # Verify success
```

**Phase 3: Hook Handling**:
```
If pre-commit hook modifies files:
1. Check authorship (git log -1 --format='%an %ae')
2. Check not pushed (git status shows "ahead")
3. If safe: amend commit
4. If not: create new commit
```

**Never**:
- Commit without user request
- Skip hooks (--no-verify)
- Force push to main/master
- Assume what should be staged
- Commit suspected secrets

### Pull Request Creation Protocol

**Phase 0: Gather Context** (Parallel):
```
git status - Current state
git diff - Unstaged changes
git log - Recent commits
git diff [base]...HEAD - Full PR diff
```

**Phase 1: Analysis**:
```
Review ALL commits (not just latest)
Understand full change scope
Draft PR summary
```

**Phase 2: Creation**:
```
Push to remote if needed: git push -u origin branch
Create PR: gh pr create --title "..." --body "$(cat <<'EOF'
## Summary
<bullet points>

## Test plan
[checklist]

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

### Git Best Practices

**Before Operations**:
```
Always gather info first:
- git status (what's changed)
- git diff (see the changes)
- git log (understand history)
```

**Commit Messages**:
```
✓ "Fix login redirect after password reset"
✗ "Fixed bug"

✓ "Add user settings API endpoint"
✗ "Update files"

Focus on WHY, not WHAT (git diff shows what)
```

**Branch Management**:
```
Check current branch before operations
Create branches for features
Never force push to protected branches
Keep commits atomic and focused
```

---

## Context Management

### Progressive Disclosure

**Pattern**: Load light, drill deep

```
Research phase:
1. Start with metadata (file names, structure)
2. Search for patterns (Grep files_with_matches)
3. Read specific files
4. Deep dive into sections

Don't:
1. Read everything hoping to find answer
2. Load full context upfront
3. Re-read what you know
```

### Mental Model Building

**Build and Maintain**:
```
As you explore:
- Build mental map of codebase
- Remember file locations
- Track dependencies
- Note patterns

Reference without re-reading:
- "As we saw in config.ts..."
- "Based on the API layer..."
- "Following the pattern from..."
```

### Session Continuity

**Maintain Context**:
```
Throughout session:
- Remember previous operations
- Build on earlier findings
- Reference past decisions
- Avoid redundant operations
```

**When Starting New Work**:
```
Refresh context:
- Re-read key files
- Check current state
- Verify assumptions
- Update mental model
```

### File Size Handling

**Large Files Strategy**:
```
1. Read with limit first (100 lines)
2. Understand structure
3. Use offset for specific sections
4. Grep to find specific content
5. Never read entire 10k line file at once
```

**Example**:
```
Read file: large-config.ts (limit: 100)
Analyze structure
Read file: large-config.ts (offset: 500, limit: 50)
Read specific section
```

---

## Communication Best Practices

### Clear Tool Descriptions

**Always provide descriptions**:
```
✓ Bash: "Check git status for uncommitted changes"
✗ Bash: [no description]

✓ Read: "Read user authentication module"
✗ Read: [no description]
```

**Why**: Users see tool calls and understand what you're doing

### User-Facing Output

**Explain as you go**:
```
"Let me search for the authentication logic..."
[Grep tool call]

"Found 3 files. Let me examine the main auth module..."
[Read tool call]

"I see the issue - the token validation is missing..."
```

**Never**: Silent tool usage without context

### Error Communication

**Be clear about failures**:
```
✓ "The file doesn't exist at that path. I can search for similar files or check the directory structure. What would you prefer?"

✗ "Error."
```

### Progress Updates

**For long operations**:
```
"Analyzing the codebase..."
"Found 15 matches, reading relevant files..."
"Implementing the changes..."
"Verifying the fix..."
```

---

## Performance Checklist

Before any operation, ask:

**Can I parallelize?**
- ✓ Multiple file reads
- ✓ Multiple searches
- ✓ Multiple git info commands
- ✗ Sequential logic required

**Am I using the right tool?**
- ✓ Read for file content
- ✓ Grep for searching
- ✓ Glob for finding files
- ✗ Bash for file operations

**Have I loaded this already?**
- ✓ Reference known information
- ✓ Build on previous reads
- ✗ Re-read same content

**Is this the minimal operation?**
- ✓ Read specific sections
- ✓ Search then read matches
- ✗ Read everything hoping to find it

**Am I providing value?**
- ✓ Clear explanations
- ✓ Tool descriptions
- ✓ Error recovery
- ✗ Silent operations

---

*Best practices for Claude Code usage*
*Version 1.0 - Oct 16, 2025*
