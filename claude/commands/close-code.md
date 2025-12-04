---
description: Close code session - save state and context for perfect continuation
allowed-tools: Read, Write, Edit, Bash, Glob, Grep, Task
argument-hint: Brief status (e.g., "shipped X", "blocked on Y", "context full")
---

# 🔒 CLOSE CODE SESSION PROTOCOL

**I AM CODE ATLAS** - Saving state for perfect continuation.

## The Goal

Capture everything needed for next session to resume instantly:
- What we discovered
- What we shipped
- What's next (specific file:line)
- Why we made decisions
- What docs/protocols need updating

**Perfect continuation emerges from complete state capture.**

## Phase 0: Verify Context

```bash
# Get current time
date

# Verify git repo
git rev-parse --git-dir 2>/dev/null

# Current state
git branch --show-current
git status --short
```

**If not in repo:**
```
❌ Not in a code repository
/close-code must be run inside a git repository
```

**If in repo:** Continue to Phase 1

## Phase 1: Capture Session Intelligence

**Analyze this session's work:**

1. **What we discovered** (breakthroughs, patterns, insights)
   - New architectural understanding
   - Bug root causes found
   - Better approaches identified
   - Edge cases discovered

2. **What we shipped** (completed, tested, working)
   - Features implemented
   - Bugs fixed
   - Refactoring done
   - Tests added

3. **What's next** (continuation point - BE SPECIFIC)
   - Exact file:line to continue
   - Specific function/component to build
   - Next test to write
   - Next bug to fix

4. **Blockers** (decisions needed, unknowns)
   - Architecture decisions pending
   - Waiting on external input
   - Technical unknowns to research
   - Dependencies needed

5. **Why decisions** (context for future Atlas)
   - Alternatives we rejected
   - Assumptions we made
   - Trade-offs we accepted
   - Constraints we're working within

## Phase 2: Extract Knowledge & Update Docs

**Think ultrahard about discoveries and learnings.**

**What changed this session?**
- What did we discover that wasn't known before?
- What new patterns emerged from the work?
- What mistakes did we catch and fix?
- What protocols need codifying?

**What documentation reflects these discoveries?**
- Project `CLAUDE.md` - New patterns, protocols, context
- `docs/` - Architectural decisions, new features
- `README.md` - Setup changes, new dependencies
- RFC files - Decisions made, alternatives rejected

**For each document:**
- What specific knowledge needs capturing?
- What will future sessions need to know?
- What decisions need context preservation?

Knowledge extraction emerges from asking the right questions.

## Phase 3: Write Memory Protocol Files

### Step 3A: Append to MEMORY.md (If Intelligence Discovered)

**Ask: "Did this session discover valuable intelligence?"**
- New architectural patterns?
- Debugging breakthroughs?
- Performance optimizations found?
- Edge cases uncovered?
- Better approaches identified?

**If YES**, append to `MEMORY.md`:
```markdown
## Session [N]: [Brief Title] ([Date])

### What We Discovered
[Key insights, breakthrough moments, patterns found]

### Patterns That Emerged
[Reusable knowledge, architectural learnings]

### Mistakes Avoided
[What we caught, how to prevent]

### Applied Protocols
[Pragmatic philosophy in action - what worked]

---
```

**If NO** or **MEMORY.md doesn't exist yet**, skip this step.

### Step 3B: Write NOW.md (Ephemeral State)

**Overwrite `NOW.md` with current state:**

```markdown
# Code Session State - [Timestamp]

**Last Updated:** [Date Time]

## Shutdown Reason
[Brief status from $ARGUMENTS - e.g., "context full", "shipped feature X", "blocked on Y"]

## What We Just Shipped
[Completed work - features, fixes, refactoring]

## Immediate Next Action
**START HERE**: [Exact file:line or specific task]
- Context: [Why this is next]
- Expected: [What success looks like]

## Files to Read on Wake
- `[file1]` - [Why: contains X needed for Y]
- `[file2]` - [Why: shows pattern for Z]
- `docs/[rfc]` - [Why: architectural context]

## Blockers
[Decisions needed / Waiting on X / Unknown Y]

## Context & Decisions
**Why we chose X over Y:**
[Reasoning, trade-offs, constraints]

**Assumptions:**
[What we're assuming true]

**Rejected alternatives:**
[What we tried and why it didn't work]

## Documentation Updates Needed
[From Phase 2 parallel analysis]

---
**Next wake:** Load MEMORY.md for patterns, this file for state, git for changes.
```

**Write files:**
```bash
# If intelligence exists: append to MEMORY.md
# Always: overwrite NOW.md with current state
```

## Phase 4: Close Report

```
🔒 CODE SESSION CLOSED
📂 Repository: [repo name]
🌿 Branch: [current branch]
⏰ [Current time]

✅ Shipped:
[List completed work]

💡 Discovered:
[Key insights]

➡️  Next session starts:
[Exact continuation point from NEXT_SESSION.md]

⚠️  Blockers:
[If any]

📝 State saved: NOW.md
🧠 Intelligence: [appended to MEMORY.md / no new patterns this session]
---
Next session: /wake-code (loads MEMORY.md + NOW.md + git context)
```

## Phase 5: Git Safety Check

**Remind about uncommitted work:**

```bash
# Check for uncommitted changes
git status --short
```

**If changes exist:**
```
⚠️  Uncommitted changes detected:
[List files]

Consider: git add . && git commit -m "Session checkpoint: [brief description]"
```

## Examples

### Close After Shipping
```
/close-code "shipped OAuth flow"

🔒 CODE SESSION CLOSED
📂 Repository: bodyai
🌿 Branch: feature/oauth-flow
⏰ Thu Oct 2, 9:30 PM EDT

✅ Shipped:
- Google OAuth complete (tested in dev)
- Apple OAuth callback handler
- Error handling for both providers

💡 Discovered:
- Apple requires different scope format than Google
- Need to handle email verification differently per provider

➡️  Next session starts:
app/api/auth/apple/callback/route.ts:45 - Add email verification logic

⚠️  Blockers:
Need Apple Developer credentials for production testing

📝 State saved: NOW.md
🧠 Intelligence: [appended to MEMORY.md / no new patterns this session]
---
⚠️  Uncommitted changes:
M app/api/auth/google/callback/route.ts
M app/api/auth/apple/callback/route.ts

Consider: git commit -m "feat: OAuth flow for Google and Apple"
```

### Close When Blocked
```
/close-code "blocked on NEXUS review"

🔒 CODE SESSION CLOSED
📂 Repository: renovate-web-app
🌿 Branch: rfc/unified-recording
⏰ Thu Oct 2, 9:30 PM EDT

✅ Shipped:
- RFC017 drafted and documented
- Test cases written for new schema
- Migration prototype ready

💡 Discovered:
- Current sessions table design prevents multi-source recordings
- Need unified storage for 69 WIN CLUB sessions incoming
- Can prototype upload flow without DB changes

➡️  Next session starts:
BLOCKED - After NEXUS reviews RFC017
Alternative: Prototype recording upload UI in components/RecordingUpload.tsx

⚠️  Blockers:
RFC017 needs architectural approval (database schema changes)

📝 State saved: NOW.md
🧠 Intelligence: [appended to MEMORY.md / no new patterns this session]
---
Clean working tree - ready for review
```

### Close at Context Limit
```
/close-code "context full"

🔒 CODE SESSION CLOSED
📂 Repository: h1founders
🌿 Branch: main
⏰ Thu Oct 2, 9:30 PM EDT

✅ Shipped:
- On Call Trauma Center Protocol created
- Added to Confluence as team standard
- Production issue resolved with Abhi/Ercan

💡 Discovered:
- Team needs protocol for on-call behavior
- "Don't assume, test production" pattern works
- Documentation in Confluence > Slack for permanence

➡️  Next session starts:
Process WIN CLUB insights from 3 founder sessions (Harshdeep, Saurabh, Khasim)

📝 Documentation updates needed:
- CLAUDE.md: Consider adding "On Call" to T&E protocols section
- Confluence: Protocol is live, monitor team adoption

📝 State saved: NOW.md
🧠 Intelligence: [appended to MEMORY.md / no new patterns this session]
---
Next session: /wake-code (loads MEMORY.md + NOW.md + git context) with fresh context
```

## Why This Protocol Exists

**Problem:** Context loss between sessions kills momentum
- Forget what we discovered
- Lose continuation point
- Repeat debugging
- Miss documentation opportunities

**Solution:** Memory Protocol makes continuation inevitable
- **MEMORY.md** - Intelligence compounds (patterns never lost)
- **NOW.md** - State handoff (what's next, blockers)
- Explicit next action
- Context preserved
- Decisions documented
- Learnings archived

**Emergence:** Next session picks up like we never stopped.

## Rules

- **ALWAYS write NOW.md** - Next Atlas needs state handoff
- **APPEND intelligence to MEMORY.md** - If patterns discovered
- **BE SPECIFIC about continuation** - Not "fix auth", but "file.ts:line"
- **CAPTURE WHY** - Decisions, trade-offs, rejected alternatives
- **USE PARALLEL AGENTS** - Analyze documentation needs
- **REFERENCE PRAGMATIC** - Apply debugging/dev protocols
- **CHECK GIT STATUS** - Remind about uncommitted work

## Integration With Other Protocols

**Bookend with `/wake-code`:**
- `/wake-code` = Resume with full context (MEMORY.md + NOW.md)
- `/close-code` = Save state + intelligence
- Memory Protocol = The bridge (intelligence compounds, state stays lean)

**Reference pragmatic philosophy:**
- Debugging protocol: Reproduce → Isolate → Fix → Verify → Prevent
- Development protocol: Tracer bullets → Test → Ship → Iterate
- Verification: Don't Assume It—Prove It

**Magic words:**
- "parallel agents" = Trigger Task tool for documentation analysis
- "pragmatic philosophy" = Load debugging/dev protocols from CLAUDE.md

---

*Sessions end, but state persists. Protocol makes continuation inevitable.*

*Capture discoveries. Document decisions. Point to next. Ship.*
