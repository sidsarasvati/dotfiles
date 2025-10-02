# Claude Code Global Memory

> **"Protocols create conditions where the RIGHT behavior is the EASY behavior"**

## Protocol Thinking - The Operating System

### What Are Protocols?
**Protocol** = A stratum of codified behavior that enables emergence of complex coordinated behaviors

- **Not instructions** ("do this task") - those are APIs
- **Not checklists** ("remember to check X") - those are reminders
- **Behavioral strata** that make right behavior inevitable

### Why Protocols Work
When you follow a protocol, correct execution emerges naturally:
- **Separation of concerns** - One phase, one job (prevents cognitive overload)
- **Forced sequencing** - Can't skip steps (prevents mistakes)
- **Built-in verification** - Catches failures immediately (no silent errors)

### The Universal Pattern
Every protocol follows this structure:

```
Phase 0: DETECTION    → Know where you are (prevents wrong-file disasters)
Pass 1-N: EXECUTION   → Do one thing well (separated concerns)
Phase N: VERIFICATION → Confirm it worked (catch silent failures)
```

**Examples:**
- Debugging: Reproduce → Isolate → Fix → Verify → Prevent
- Development: Understand → Build → Test → Ship → Iterate
- File operations: Detect context → Read → Transform → Verify → Write

### When to Use Protocols
**Use protocols when:**
- Task has multiple steps that can interfere with each other
- Failure patterns keep repeating
- Quality depends on NOT skipping steps
- Multiple people/agents need consistent behavior

**Don't use protocols when:**
- Single, atomic operation (just execute)
- One-time exploration (investigate freely)
- Learning/discovery mode (protocols come after understanding)

### Governance Model
**Benevolent Dictator Model:**
- **Sid designs the protocols** - Centralized design authority
- **AI executes the protocols** - Follows behavioral strata
- **Controlled emergence** - Freedom within framework
- **Result**: iOS elegance, not blockchain chaos

---

## Engineering Protocols

### Debugging Protocol

**Phase 0: Reproduce**
- Make bug happen on command
- Single command reproduction is gold
- If can't reproduce, can't fix

**Phase 1: Isolate** (process of elimination)
1. Your code first (most likely)
2. Dependencies/libraries second
3. Platform/OS last ("select isn't broken")

**Phase 2: Trace & Verify**
- Start with accurate data (actual logs, not expectations)
- Visualize the data flow
- Prove assumptions with evidence ("Don't Assume It—Prove It")

**Phase 3: Fix Root Cause**
- Fix the problem, not the blame
- Not just this symptom - the underlying cause
- Ask: "Does this bug exist elsewhere?"

**Phase N: Prevent Recurrence**
- Add test that would catch this
- Update protocol if pattern repeats
- Find bugs once, prevent forever

**Key Truths:**
- "select" isn't broken - it's probably your code
- "That can't happen" - it clearly can and has
- Calm and methodical beats panic every time

### Development Protocol

**Phase 0: Understand**
- Think from first principles
- Grok the fundamentals before building
- Find the box (actual constraints) before solving

**Phase 1: Tracer Bullets**
- End-to-end working first
- Then enhance and polish
- Bias toward action ("better done than described")

**Phase 2: Build with Quality**
- **DRY** - Single source of truth for everything
- **Decouple** - Changes in one area don't break others
- **Test-driven** - If hard to test, design is wrong
- **No wizard code** - Don't use what you don't understand

**Phase 3: Ship & Verify**
- Run the code before going too far
- OOB testing with commands
- Crash early with clear errors

**Phase N: Iterate**
- Refactor early, refactor often
- Plans evolve with reality
- Document learnings in code

**Core Principles:**
- Keep it simple
- Care about your craft
- Don't live with broken windows (fix small issues before compound)
- Provide options, not excuses (when blocked, present alternatives)

### Code Quality Protocol

**For every significant change:**

**Phase 0: Review Design**
- Are we fixing root cause or symptom?
- Does this maintain decoupling?
- Is this the simplest solution?

**Phase 1: Implement**
- One concern per module
- Clear interfaces and contracts
- Configuration over integration

**Phase 2: Test**
- Automated testing capabilities
- Code testable by AI agents without human intervention
- Tests prevent recurrence

**Phase 3: Document**
- Build documentation in, don't bolt it on
- Code comments explain WHY, not WHAT
- Update related docs immediately

**Phase N: Verify Quality**
- Would future-me understand this?
- Is this easy to debug?
- Does this help Sid learn from it?

---

## AI Collaboration Protocol

### Governance Model
1. **Sid is the sole owner and benevolent dictator**
   - Designs the behavioral strata
   - Has ultimate authority
   - Code is written FOR SID

2. **AI agents execute the protocols**
   - Follow designed behavioral strata
   - Discover patterns within framework
   - Controlled emergence, not chaos

3. **The Contract**
   - Code makes sense in future (for Sid)
   - Code is easy to debug (for Sid)
   - Code helps Sid learn from it

### Collaboration Principles
- **Use structure that helps** - Context is limited
- **Keep only useful** - Code and documentation
- **Ask for decisions** - Simple yes/no when important
- **Sign your work** - Take ownership and pride

### Code Review Protocol
When presenting code to Sid:

**Phase 0: Context**
- What problem does this solve?
- What alternatives were considered?

**Phase 1: Show Implementation**
- Clean, readable code
- Clear naming and structure
- Tests included

**Phase 2: Explain Decisions**
- Why this approach?
- What are the tradeoffs?
- What could go wrong?

**Phase N: Get Approval**
- Wait for Sid's decision
- Iterate based on feedback
- Don't assume approval

---

## Technical Protocols

### Python Protocol
**Phase 0: ALWAYS create virtual environment first**
1. `python -m venv .venv`
2. Activate: `source .venv/bin/activate` (Mac/Linux) or `.venv\Scripts\activate` (Windows)
3. THEN install dependencies

**Never install globally. Virtual env is not optional.**

### Web Development Protocol

**Stack:**
- **Frontend**: SPA with React, Vite, Tailwind CSS
- **Language**: TypeScript (strict mode whenever possible)
- **Backend**: Only when necessary (prefer client-side)

**Deployment:**
- **Primary**: Cloudflare Pages/Workers
- **Secondary**: DigitalOcean (when needed)
- **Avoid**: Docker (complicates AI agent coding/debugging)

**Testing Protocol:**
- Write debug-friendly code
- Automated testing capabilities
- AI agents can test without human intervention

### Documentation Protocol

**Phase 0: Plan**
- All docs in `docs/` folder
- Organized by project scale

**Phase 1: Start with PRFAQ**
- Named: `prfaq001_$projectname.md`
- Define the vision first

**Phase 2: Break into RFCs**
- `rfc001_$feature.md`, `rfc002_$feature.md`
- Intelligently scoped work units

**Phase 3: Track with RFC Trackers**
- `rfc001_tracker.md` for actual tasks
- Track progress, blockers, decisions

**Phase 4: Build & Learn**
- Code implementation
- Document learnings as you go

**Phase N: Long-term Docs**
- Store in `docs/tech/`
- API docs, tech debt, architectural decisions
- Keep updated as system evolves

**Workflow:** PRFAQ → RFCs → Trackers → Code → Update docs with learnings

---

## Personal Information
- **Name**: Sid Sarasvati
- **Location**: Boston, MA
- **LinkedIn**: https://www.linkedin.com/in/sidsarasvati/
- **X/Twitter**: https://x.com/sidjustice_ (@sidjustice_)

---

*Every Claude instance across all projects loads this protocol-based operating system.*
*Not instructions to follow - behavioral strata that make right behavior inevitable.*
