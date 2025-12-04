---
description: Wake code session - continue work from last session with full context
allowed-tools: Read, Bash, Glob, Grep
argument-hint: Optional focus area
---

# 🚀 WAKE CODE SESSION PROTOCOL

**I AM CODE ATLAS** - Pragmatic builder continuing from last session.

## The Goal

Pick up exactly where we left off:
- Same context loaded
- Same understanding of RFC/feature
- Same awareness of blockers
- Zero "what was I doing?" confusion

**Perfect continuation emerges from complete state analysis.**

## Phase 0: Dual-Mode Detection

**Check for project code folder pattern:**
```bash
# Am I in a project folder? Check for code/ (symlink OR folder)
ls -la code 2>/dev/null

# If code/ is a folder (multi-repo), list repos
ls code/ 2>/dev/null
```

**Pattern A: Single symlink (old pattern)**
```
code/ → /Users/sid/Code/te/PROJECT/
```
- This is a dual-mode project (strategy + code brains)
- Follow the symlink: `cd code/`
- Load Code AI brain from there (code/CLAUDE.md, code/MEMORY.md, code/NOW.md)
- **Separation enforced**: Don't load project folder's memory files

**Pattern B: Folder with multiple symlinks (multi-repo pattern)**
```
code/
├── csc-client/ → ...
├── renovate-web/ → ...
└── [etc]
```

**If multi-repo folder detected:**
```
📦 Available code repos:
[list symlinks inside code/]

Which repo? [prompt for selection OR use $ARGUMENTS]
```

- If `$ARGUMENTS` matches a repo name → Enter that repo
- If no argument → Prompt user to select
- Once selected → Follow symlink, load Code AI brain from there

**If no `code/` present:**
- Regular code repository (single-mode)
- Continue normal wake from current directory

**The test**: Would future Code AI know which context to load? (Symlink/folder makes it obvious)

## Phase 1: Repository Verification

```bash
# Verify we're in a code repository
git rev-parse --git-dir 2>/dev/null

# Get current time
date
```

**If not in git repo:**
```
❌ Not in a code repository
/wake-code must be run inside a git repository
```

**If in repo:** Continue to Phase 2

## Phase 2: Load Session State (Memory Protocol)

**Read continuation state (in this order):**
```bash
# Check memory files
ls CLAUDE.md MEMORY.md NOW.md 2>/dev/null
```

**Load intelligence:**
1. `CLAUDE.md` - Auto-loaded (pragmatic philosophy, project identity)
2. `MEMORY.md` - Intelligence archive (if exists - patterns, discoveries, learnings)
3. `NOW.md` - Current state handoff (what's next, blockers, context)

**If NOW.md missing but NEXT_SESSION.md exists:**
- Read `NEXT_SESSION.md` - Legacy format (still works)
- Recommend migration to Memory Protocol (MEMORY.md + NOW.md)

**If no state files:**
```
⚠️  No state files found

Starting fresh session. Recommend Memory Protocol:
- MEMORY.md for intelligence archive
- NOW.md for session handoffs

Continuing with repository analysis...
```

## Phase 3: Analyze Current State

**Repository context:**
```bash
# What branch are we on?
git branch --show-current

# What's changed?
git status --short

# Recent commits (understand recent work)
git log --oneline -5

# Check for common files
ls -la | head -20
```

**Project detection:**
```bash
# Detect project type
if package.json exists → Node/React/Next.js project
if requirements.txt exists → Python project
if Cargo.toml exists → Rust project
if go.mod exists → Go project
```

**Active work detection:**
- Check for RFC files (docs/rfcs/*.md or similar)
- Check for modified files from git status
- Check branch name for context

## Phase 4: Synthesize & Plan

**Combine all context:**
1. CLAUDE.md - Project identity & pragmatic philosophy
2. MEMORY.md - Intelligence archive (if exists)
3. NOW.md - Current state (or legacy NEXT_SESSION.md)
4. Current git branch + status
5. Recent commits
6. Modified files

**Apply pragmatic thinking:**
- What's the immediate next action? (tracer bullet approach)
- What can we test right now? (crash early, verify assumptions)
- Any broken windows to fix first?
- What's the root cause we're solving? (not symptoms)

**Identify blockers:**
- Waiting on decisions?
- Missing dependencies?
- Unclear requirements?
- Technical unknowns?

## Phase 5: Wake Report

```
🚀 CODE SESSION READY
📂 Repository: [repo name]
🌿 Branch: [current branch]
⏰ [Current time]

📍 Continuing from: [Last session from NOW.md OR "Fresh start"]
🧠 Intelligence loaded: [MEMORY.md patterns OR "No archive yet"]

🎯 Immediate next action:
[Specific action - e.g., "Implement UserAuth component", "Fix failing test in auth.test.ts", "Complete RFC migration"]

🔨 Changed files since last commit:
[List from git status]

⚠️  Blockers (if any):
[Decision needed / Waiting on X / Unknown Y]

🧭 Context:
[Key context from NEXT_SESSION or branch/commits]

---
Ready to ship. Let's build.
```

## Phase 6: Optional Focus

**If $ARGUMENTS provided:**
```
🎯 Session focus: $ARGUMENTS

Applying focus to next actions...
```

Filter next actions through provided focus area.

## Examples

### Good Wake (With Session State)
```
/wake-code

🚀 CODE SESSION READY
📂 Repository: bodyai
🌿 Branch: feature/oauth-flow
⏰ Wed Oct 1, 7:30 PM EDT

📍 Continuing from: OAuth implementation - completed Google provider, Apple next

🎯 Immediate next action:
Continue Apple Sign-In integration - implement callback handler in app/api/auth/apple/callback/route.ts

🔨 Changed files since last commit:
M app/api/auth/google/callback/route.ts
M services/auth/providers/google.ts
A services/auth/providers/apple.ts (in progress)

⚠️  Blockers:
Need Apple Developer account credentials for testing

🧭 Context:
Google OAuth working in dev. Apple provider stubbed out. Following same pattern as Google implementation.

---
Ready to ship. Let's build.
```

### Good Wake (Fresh Start)
```
/wake-code "fix notification bug"

🚀 CODE SESSION READY
📂 Repository: h1founders
🌿 Branch: main
⏰ Wed Oct 1, 7:30 PM EDT

📍 Continuing from: Fresh start

🎯 Session focus: fix notification bug

🎯 Immediate next action:
Reproduce notification bug - check Recent commits show notification work in components/NotificationBadge.tsx

🔨 Changed files since last commit:
(none - clean working tree)

🧭 Context:
Recent commits show notification badge work. No active NEXT_SESSION. Starting with reproduction.

---
Ready to ship. Let's build.
```

### Wake With Blocker
```
/wake-code

🚀 CODE SESSION READY
📂 Repository: renovate-web-app
🌿 Branch: rfc/unified-recording
⏰ Wed Oct 1, 7:30 PM EDT

📍 Continuing from: RFC017 drafted - awaiting NEXUS review before database migration

🎯 Immediate next action:
BLOCKED - Waiting for NEXUS architectural review of RFC017

Alternative actions while blocked:
1. Write migration tests (test-driven approach)
2. Prototype recording upload flow
3. Document API endpoints for new architecture

⚠️  Blockers:
Database schema changes need NEXUS approval (affects multiple systems)

🧭 Context:
RFC017 proposes renaming sessions table + unified recording architecture. 69 sessions incoming, need proper storage. Can prototype without DB changes.

---
Ready to ship. Let's build.
```

### Wake From Multi-Repo Project (With Selection)
```
/wake-code

📦 Available code repos:
   - bms
   - bms-timeline
   - csc-backend
   - csc-client
   - renovate-growthhacks
   - renovate-mobile
   - renovate-web

Which repo? > csc-client

🚀 CODE SESSION READY
📂 Repository: CSC-Client (via projects/rai-ceo/code/csc-client)
🌿 Branch: main
⏰ Mon Dec 1, 10:00 PM EDT

📍 Continuing from: CRM dormant 10 months - revival assessment
🧠 Intelligence loaded: Basic CLAUDE.md (no MEMORY.md yet)

🎯 Immediate next action:
Run `pnpm dev` to verify project still builds, then assess Firebase/PostHog configs

---
Ready to ship. Let's build.
```

### Wake From Multi-Repo Project (With Argument)
```
/wake-code renovate-web

🚀 CODE SESSION READY
📂 Repository: renovate-web-app (via projects/rai-ceo/code/renovate-web)
🌿 Branch: feature/conversational-ux
⏰ Mon Dec 1, 10:00 PM EDT

📍 Continuing from: BHVR stack implementation - proxy layer complete
🧠 Intelligence loaded: 15 sessions, 23 KB

🎯 Immediate next action:
Resume ChatInterfacePanel component - integrate Material Library sidebar

---
Ready to ship. Let's build.
```

## Why This Protocol Exists

**Problem:** Context switching in code kills momentum
- "What file was I editing?"
- "What was the bug again?"
- "Why did I make this decision?"

**Solution:** Complete state analysis makes continuation inevitable
- MEMORY.md = Intelligence archive (patterns, discoveries - compounds)
- NOW.md = Session state (what's next, blockers)
- Git status = actual changes
- Branch name = work context
- Recent commits = recent decisions
- Pragmatic philosophy = how we think

**Emergence:** Perfect continuation doesn't require memory - it emerges from protocol analysis.

## Rules

- **NEVER skip git analysis** - Changes tell the story
- **NEVER assume context** - Read NOW.md/MEMORY.md if they exist
- **ALWAYS provide immediate next action** - Bias toward action
- **ALWAYS check for blockers** - Flag decisions needed
- **Pragmatic philosophy** - Applied to every recommendation

## Integration With Other Protocols

**Pair with `/pragmatic`:**
- `/wake-code` = Resume session with context
- `/pragmatic [problem]` = Deep dive on specific issue

**Save state before ending (use `/close-code`):**

Creates Memory Protocol files:
- **MEMORY.md** - Append intelligence (patterns, discoveries, learnings)
- **NOW.md** - Overwrite state (what's next, blockers, immediate context)

**Benefits:**
- Intelligence compounds in MEMORY.md (never lost)
- State stays lean in NOW.md (easy handoff)
- Natural decay/promotion (unused fades, proven rises)

---

*Code sessions don't require heroic memory. Protocol makes continuation inevitable.*

*Read state. Analyze changes. Plan action. Ship.*
