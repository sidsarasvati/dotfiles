# Template: Code-Execution Skill

> **Use when:** Skill needs to run tools/commands during activation
> **Size target:** 25-50KB
> **Structure:** SKILL.md with embedded tool usage patterns

## When to Use This Template

**Choose code-execution skill template for:**
- Skills that verify state (calendar checks, file status, git state)
- Skills that gather data (read logs, analyze metrics, fetch information)
- Skills that need current context (time, date, system state)
- Skills combining guidance + verification

**Don't use for:**
- Pure guidance skills (use simple-skill template)
- Multi-pass processes without tool usage (use protocol-skill template)
- Destructive operations (skills should guide, not auto-execute changes)

## Directory Structure

```
skill-name/
└── SKILL.md (guidance + tool usage patterns)
```

## Template: SKILL.md

```markdown
---
name: skill-name
description: Use when [SPECIFIC SCENARIO requiring data/verification]. Auto-activates for [TRIGGERS]. Provides [VALUE] with [DATA TYPE] verification.
---

# Skill Name: [Purpose] with Verification

> **"Quote about skill purpose"**
> **"Quote about verification importance"**

## Core Principle

[2-3 sentences explaining what skill does and why tool execution matters]

## When to Use This Skill

- Scenario requiring current data (calendar, time, status)
- Scenario needing verification (file exists, state correct)
- Scenario with dynamic context (changes frequently)
- **When NOT to use:** [Scenarios where static guidance sufficient]

## Auto-Verification on Activation

**This skill automatically:**
1. **Verifies [context 1]** via [tool/command]
2. **Checks [context 2]** using [tool/command]
3. **Gathers [data]** from [source]
4. **Provides guidance** based on current state

**Why verification matters:**
[Explanation of why current data critical]

## [Core Guidance Section]

[Main skill content - frameworks, methodology, templates]

### Using Current [Data Type]

**Pattern:**
```bash
# Verify current [context]
[command to run]

# Use result in guidance
[how to apply verified data]
```

**Example:**
[Real example of tool usage]

## Tool Usage Patterns

### Pattern 1: Verify Before Advising

**Always:**
```bash
# Check current state
[verification command]
```

**Then:** Provide guidance based on actual state (not assumptions)

### Pattern 2: Gather Context

**Fetch data:**
```bash
# Get current [data]
[command]
```

**Apply:** Use data in recommendations

### Pattern 3: State Validation

**Confirm prerequisites:**
```bash
# Verify [requirement 1]
[command]

# Verify [requirement 2]
[command]
```

**Proceed:** Only if validations pass

## Critical Patterns

### The AI Failure Pattern (NEVER DO THIS)

❌ Assume state without checking (make guesses)
❌ Use cached/old data (not current)
❌ Skip verification (trust previous state)
❌ Execute destructive operations automatically

### The Correct Pattern (DO THIS)

✅ Verify state before advising (run checks)
✅ Use current data (fetch fresh)
✅ Validate prerequisites (confirm safe to proceed)
✅ Guide user through actions (don't auto-execute changes)

## Examples

### Example 1: [Scenario with Verification]

**User asks:** [Request requiring current data]

**Skill activates and:**
1. Runs: `[verification command]`
2. Analyzes: [Current state]
3. Advises: [Guidance based on actual state]

**Why this works:**
- Current data (not assumptions)
- Verified state (not guessed)
- Accurate guidance (based on reality)

### Example 2: [Another Scenario]

[Similar pattern showing different use case]

## Verification Commands Reference

**Current time/date:**
```bash
date
```

**File operations:**
```bash
ls [path]           # List directory
cat [file]          # Read file
stat [file]         # File metadata
```

**Git status:**
```bash
git status
git log -1
git diff
```

**Calendar (via MCP):**
```bash
# Check today's events
[calendar MCP command]
```

**Process/system:**
```bash
ps aux | grep [process]
df -h
top -l 1
```

## Success Metrics

- [ ] Verifies current state before advising
- [ ] Uses actual data (not assumptions)
- [ ] Guidance accurate based on current context
- [ ] No destructive auto-execution
- [ ] Clear about what it's checking

**The test:** Does skill work correctly when state changes?

## Safety Guidelines

**Skills should:**
✅ Read/verify state (safe operations)
✅ Guide user through changes (explain what to do)
✅ Validate before destructive ops (confirm safe)

**Skills should NOT:**
❌ Auto-execute destructive operations (delete, force-push, etc.)
❌ Modify files without user confirmation
❌ Make assumptions without verification

**Why:**
- User maintains control (AI guides, user executes)
- Safety first (verify before suggesting changes)
- Transparency (user sees what's being checked)

---

*"Verify state, don't assume it - current data enables accurate guidance"*
*"Guide user through actions - don't auto-execute destructive operations"*
```

## Customization Guide

### Step 1: Identify Required Data

**Questions:**
1. What data does skill need to provide accurate guidance?
2. What state should be verified before advising?
3. What context changes frequently (needs fresh check)?
4. What prerequisites must be met?

**Examples:**
- Calendar skill: Current events, upcoming meetings
- Git skill: Current branch, uncommitted changes, remote status
- Time-aware skill: Current date/time, timezone
- File skill: File existence, contents, modification time

### Step 2: Choose Verification Commands

**Map data needs to commands:**

**Time/date:**
- `date` - Current date/time
- `date -u` - UTC time

**Files:**
- `ls [path]` - Directory listing
- `cat [file]` - File contents
- `stat [file]` - File metadata
- `find [path] -name [pattern]` - Search files

**Git:**
- `git status` - Working tree status
- `git log -N` - Recent commits
- `git branch` - Current branch
- `git diff` - Uncommitted changes
- `git remote -v` - Remote URLs

**Calendar (MCP):**
- Calendar MCP commands (if available)
- Event queries
- Availability checks

**System:**
- `ps` - Process status
- `df` - Disk usage
- Environment variables

### Step 3: Design Verification Flow

**Pattern:**
```markdown
## Auto-Verification on Activation

**This skill automatically:**
1. Verifies [X] via `[command]`
2. Checks [Y] using `[command]`
3. Gathers [Z] from `[command]`
4. Provides guidance based on current state
```

**Example (calendar skill):**
```markdown
## Auto-Verification on Activation

**This skill automatically:**
1. Verifies current date/time via `date`
2. Checks today's calendar via MCP calendar integration
3. Identifies conflicts and availability
4. Provides scheduling guidance based on actual calendar state
```

### Step 4: Add Safety Constraints

**Define boundaries:**
```markdown
## Safety Guidelines

**This skill will:**
✅ Read calendar state (safe)
✅ Verify time availability (safe)
✅ Suggest meeting times (guidance)

**This skill will NOT:**
❌ Auto-create calendar events (user confirms first)
❌ Delete existing events (destructive)
❌ Modify events without permission (changes require approval)
```

**Why critical:**
- Skills should verify and guide
- User maintains control over changes
- Prevents unintended consequences

### Step 5: Embed Tool Usage Examples

**Show how to use verified data:**
```markdown
### Using Current Calendar State

**Verify state:**
```bash
date
# Mon Oct 27 17:00:00 EDT 2025

# Check calendar (via MCP)
[calendar command]
# Returns: 2 meetings today
```

**Apply to guidance:**
- Current time: 5:00 PM
- Meetings: 9:00 AM (done), 3:00 PM (done)
- Available: Now until EOD
- Recommendation: [Based on verified availability]
```

## Common Code-Execution Patterns

### Pattern 1: Time-Aware Skills

**Verification:**
```bash
date
```

**Usage:**
- Determine current day (weekday vs weekend)
- Check time of day (morning vs afternoon vs evening)
- Calculate deadlines (days until date)
- Schedule-aware guidance

**Example skills:**
- Victory Hour tracking (verify days since last workout)
- Calendar operations (current time for scheduling)
- Journal operations (confirm editing today's journal)

### Pattern 2: File-Verification Skills

**Verification:**
```bash
ls [directory]
cat [file]
```

**Usage:**
- Confirm file exists before editing
- Check file contents for state
- Verify directory structure
- Validate paths

**Example skills:**
- Logseq operations (verify journal file exists)
- Project management (check project files)
- Documentation (verify doc structure)

### Pattern 3: Git-Aware Skills

**Verification:**
```bash
git status
git log -1
git diff
```

**Usage:**
- Current branch verification
- Uncommitted changes check
- Remote sync status
- Commit history context

**Example skills:**
- Commit message guidance (analyze recent commits)
- PR creation (verify branch state)
- Code review (check diffs)

### Pattern 4: Calendar-Aware Skills

**Verification:**
```bash
# Calendar MCP integration
[calendar commands]
```

**Usage:**
- Meeting conflict detection
- Availability checking
- Event recommendations
- Schedule optimization

**Example skills:**
- Meeting scheduler (verify calendar state)
- Availability tracker (check conflicts)
- Time blocking (optimize schedule)

## Testing Code-Execution Skills

### Test Verification Logic

**Scenarios to test:**
1. **Normal state** - Everything as expected
2. **Missing data** - Expected file/event doesn't exist
3. **Unexpected state** - Something changed since last check
4. **Error conditions** - Command fails or returns error

**For each scenario:**
- Does skill handle gracefully?
- Is error message clear?
- Does guidance still help user?
- No silent failures?

### Test with Changing State

**Dynamic testing:**
1. Test skill activation
2. Change underlying state (modify file, update calendar, etc.)
3. Test skill activation again
4. Verify: Uses NEW state, not old cached data

**Why critical:**
- Proves skill fetches current data
- Validates no stale state assumptions
- Confirms verification working

### Test Safety Boundaries

**Try to trigger destructive operations:**
- Does skill prevent auto-execution?
- Does it warn user appropriately?
- Does it require explicit confirmation?

**If skill allows destructive ops:**
- Add safety constraints
- Require user confirmation
- Validate prerequisites first

## Examples of This Pattern

**Potential skills:**
- `calendar-operations` - Verify calendar state, suggest meeting times
- `iron-forge-tracking` - Check days since last workout, prevent "8 days since" errors
- `git-operations` - Verify branch/commit state before suggesting operations
- `victory-hour-tracking` - Calculate streaks, verify completion

**Key insight:**
> Skills with verification provide accurate guidance based on reality, not assumptions

## Size Guidelines

**Typical size: 25-50KB**

**Components:**
- Verification logic: 5-10KB
- Core guidance: 15-25KB
- Tool usage examples: 5-10KB
- Safety guidelines: 3-5KB

**If too large:**
- Split verification logic to references/
- Move detailed examples to examples/
- Keep core guidance in SKILL.md

## Next Steps After Creating

1. **Test all verification commands** (ensure they work)
2. **Test with different states** (normal, error, missing data)
3. **Validate safety boundaries** (no auto-destruction)
4. **Use in production** (real scenarios, not theoretical)
5. **Document edge cases** (what happens when X fails?)
6. **Iterate based on usage** (refine based on real needs)

---

*"Verify first, advise second - current state enables accurate guidance"*
*"Read-only verification = safe, auto-execution = dangerous"*
*"Skills guide users through actions, don't execute destructively"*
