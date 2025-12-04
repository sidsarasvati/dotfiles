# Case Study: garden-protocol Skill

> **Working Example:** Sid's actual production skill
> **Pattern:** Multi-pass protocol with separation of concerns
> **Size:** ~35KB single file
> **Success:** Used daily, reduces 200+ line journals to 40-60 lines of essence

## Overview

The garden-protocol skill demonstrates the protocol pattern for skills - a systematic multi-pass methodology where each pass has ONE job. This is Sid's most-used skill, proven in daily production.

## Complete Structure

```
garden-protocol/
└── SKILL.md (7-pass protocol, all content)
```

**Single-file protocol skill** - All methodology in one place

## YAML Frontmatter

```yaml
---
name: garden-protocol
description: Use when closing or reviewing daily journal entries in Logseq to apply the 7-pass garden tending protocol. Ensures journals are zen gardens (curiosity-driven, scannable) not databases (verbose, complete). Auto-activates for journal closing, pruning, or quality checks.
---
```

**What makes this excellent:**

**Specific scenario:**
- "closing or reviewing daily journal entries in Logseq"
- Not "all Logseq work" or "journal tasks"
- Precise boundary: daily journals, not all pages

**Clear methodology:**
- "7-pass garden tending protocol" (specific approach)
- Not vague "helps with journals"

**Outcome articulated:**
- "zen gardens (curiosity-driven, scannable)"
- "not databases (verbose, complete)"
- Transformation clearly stated

**Multiple triggers:**
- "journal closing" (primary use)
- "pruning" (maintenance)
- "quality checks" (review)

## The 7-Pass Protocol Structure

### Phase 0: Context Detection
- Verify current date/time
- Confirm editing today's journal
- Load gold standard examples
- Output confirmation before proceeding

**Why Phase 0:** Prevents editing wrong journal (AI failure mode)

### Pass 1: Walk the Garden (READ ONLY)
- Read entire journal
- List issues (field reports, duplicates, bloat, chronology)
- OUTPUT what found
- **DO NOT FIX ANYTHING**

**Why read-only:** Separation of concerns - observe before acting

### Pass 2: Absorb Field Reports
- Move #atlasfieldreport markers into time blocks
- Delete floating reports
- **ONLY** integrate reports, nothing else

**Why focused:** One concern per pass (field report integration)

### Pass 3: Merge Duplicates
- Combine duplicate project blocks
- Merge adjacent time blocks
- Fix chronology
- **ONLY** merging operations

**Why focused:** Consolidation separate from pruning

### Pass 4: Prune Bloat (CURIOSITY LENS)
- Apply "makes me curious" test
- If knowledge has home, prune to 1 line + link
- If no home, flag for Pass 5
- **ONLY** pruning, no extraction

**Why focused:** Pruning separate from extracting

### Pass 5: Extract Homeless Knowledge (CONDITIONAL)
- **ONLY IF** Pass 4 flagged content
- Suggest extraction, ASK SID
- Don't auto-extract

**Why conditional:** Not every journal needs extraction

### Pass 6: Connect (REFERENCES ONLY)
- Add Seeds Planted (block refs, 3-4 max)
- Write Moon's Reflection (narrative, 2-3 paragraphs)
- Add page links
- Generate block IDs for insights

**Why focused:** Connection separate from content work

### Pass 7: Color (EMPHASIS ONLY)
- Add strategic highlights (3-5 max)
- Bold metrics and results
- Remove excessive bold

**Why last:** Emphasis after content complete

### Phase N: Final Verification
- 12-point checklist
- Confirm journal is "zen garden"
- Output summary (X→Y lines, essence captured)

**Why Phase N:** Catches failures before declaring done

## Key Architectural Decisions

### Decision 1: Phase 0 and Phase N Bookends

**Pattern:**
```
Phase 0: DETECTION (context verification)
Pass 1-N: EXECUTION (systematic transformation)
Phase N: VERIFICATION (quality confirmation)
```

**Why:**
- Phase 0 prevents wrong-file disasters
- Passes execute with confidence (context verified)
- Phase N catches silent failures

**Applied to skill creation:**
```
Phase 0: What kind of skill? (detection)
Pass 1-6: Build skill (execution)
Phase N: Test auto-activation (verification)
```

### Decision 2: One Pass, One Purpose

**Anti-pattern Claude tends toward:**
❌ "I'll be efficient and fix structure while adding links"

**Correct pattern enforced:**
✅ Pass 3: Merge duplicates ONLY
✅ Pass 6: Add links ONLY
✅ No combining operations

**Why this works:**
- AI can't handle multiple concerns simultaneously
- Separation prevents forgotten steps
- Each pass verifiable independently

**Evidence:**
> "You can't plant and prune at the same time" - 4-Pass Technique discovery

### Decision 3: Output Confirmations

**Every pass ends with:**
```markdown
**Output**: "Pass X complete. [Summary of changes]"
```

**Why:**
- Forces completion of current pass
- Prevents skipping to next pass
- Creates audit trail
- User sees progress

**Example:**
> "Pass 4 complete. Pruned: 3 sections (127→41 lines). Flagged: Victory Hour metrics"

### Decision 4: Seeds vs Moon Separation

**Architecture:**
- **Seeds Planted** = WHAT was planted (block refs, pure data)
- **Moon's Reflection** = HOW it felt (narrative, meaning)

**Why separate:**
- Prevents duplication (WHAT doesn't repeat in HOW)
- Clear purpose per section (data vs story)
- Scannable structure (seeds = facts, moon = feeling)

**Anti-pattern prevented:**
```markdown
Seeds:
- ((uuid-of-breakthrough))

Moon:
- ((uuid-of-breakthrough)) - this was amazing! <-- DUPLICATION
```

**Correct pattern:**
```markdown
Seeds:
- ((uuid-of-breakthrough))
- [[Framework]] launched

Moon:
Palm-sweating excitement launching [[Framework]]. The breakthrough ((uuid)) 
unlocked this. Every Claude gets this power now. <-- NEW CONTEXT
```

### Decision 5: Curiosity Lens (Pass 4)

**The Test:**
> "Would this make Sid curious or bored?"
> "Can you read in 10 seconds and want more?"
> "Does it link to where knowledge lives?"

**Applied:**
- ❌ 13 lines of metrics → ✅ "**158.5 lbs** @ **21.6% BF**, plank **65s** PR"
- ❌ 18 lines of notes → ✅ "**7.0/10** (perfectionist patterns) - fear theme"

**Why this works:**
- Outcome-driven (curiosity) vs process-driven (completeness)
- Measurable test ("10 seconds")
- Clear boundary (knowledge has home elsewhere)

## What Makes This Skill Exceptional

### Success Metric 1: Daily Production Usage

**Reality:**
- Used every single day
- Closes journals in ~2 minutes
- Transforms 200+ lines → 40-60 lines
- Never skipped or worked around

**Why it works:**
- Solves real pain (verbose journals)
- Fast execution (~2 min)
- Proven methodology (7 passes tested)
- Auto-activates correctly

### Success Metric 2: Behavioral Strata, Not Instructions

**Not:**
```markdown
## Steps
1. Read the journal
2. Fix any problems
3. Make it better
```

**Actually:**
```markdown
### Pass 1: Walk the Garden (READ ONLY)
1. Read entire journal top to bottom
2. OUTPUT: "Pass 1 complete. Found: [summary]"
3. **DO NOT FIX ANYTHING YET**
```

**Why this works:**
- Forces read-only pass (prevents premature fixing)
- Output confirmation (can't skip to next pass)
- Explicit prohibition (DO NOT FIX)
- Right behavior emerges inevitably

### Success Metric 3: Self-Correcting AI

**Pattern observed:**
- AI starts combining operations (natural tendency)
- Skill structure prevents it (forced separation)
- AI self-corrects ("Wait, Pass 3 is merge only")
- Right behavior emerges from architecture

**Example:**
> "Starting Pass 4... pruning bloat and adding links—"
> **STOP.** Pass 4 is pruning ONLY. Links in Pass 6.

### Success Metric 4: Proven Protocol Evolution

**History:**
- Oct 2, 2025: Seeds vs Moon architecture emerged
- Sep 22, 2025: 4-Pass Technique discovered
- Sep 19, 2025: Garden Monk Protocol added
- Continuous refinement based on usage

**Why this matters:**
- Skill improves through execution
- Learnings captured and integrated
- Protocol thinking applied to protocol evolution
- Meta-improvement enabled

## Lessons for Protocol Skills

### Lesson 1: Phase 0 and Phase N Are Mandatory

**Always:**
- Phase 0: Detect context/type before executing
- Phase N: Verify completion before declaring done

**Why:**
- AI makes context mistakes (wrong file, wrong date)
- AI makes silent failures (thinks done, but incomplete)
- Bookends catch both failure modes

### Lesson 2: One Pass, One Job

**Pattern:**
```
Pass X: [ACTION] ([QUALIFIER])
- Do specific thing
- Output confirmation
- DO NOT do other things
```

**Why:**
- AI combines operations naturally (efficiency bias)
- Structure forces separation
- Each pass independently verifiable

### Lesson 3: Read-Only Pass First

**Pattern:**
```
Pass 1: Walk/Read (READ ONLY)
- Observe everything
- List issues
- DO NOT FIX
```

**Why:**
- Prevents premature optimization
- Forces complete observation
- Better plan from full context

### Lesson 4: Output Confirmations

**After every pass:**
```
**Output**: "Pass X complete. [Summary]"
```

**Why:**
- Forces pass completion
- Creates audit trail
- User sees progress
- Prevents skipping

### Lesson 5: The Test (Measurable Success)

**Define clear test:**
- Curiosity Lens: "10-second scan test"
- Time: "~2 minutes for full protocol"
- Size: "200+ lines → 40-60 lines"
- Quality: "Zen garden, not database"

**Why:**
- Measurable outcome
- Know when done
- Continuous improvement baseline

## Applying Protocol Pattern

### When to Use Protocol Pattern

**Use when:**
- Multi-step systematic process
- Each step has clear purpose
- Order matters (dependencies between steps)
- Separation of concerns needed
- Repeatable methodology

**Examples:**
- Journal tending (garden-protocol)
- Skill creation (skill-creator) ← THIS SKILL
- Weekly review (multi-pass analysis)
- Code refactoring (read → analyze → refactor → verify)

### Protocol Skill Template

```markdown
---
name: protocol-name
description: Use when [scenario] to apply [X-pass protocol]. Auto-activates for [triggers].
---

# Protocol Name

> **Core principle**

## When to Use This Skill

## The X-Pass Protocol

### Phase 0: Detection (MANDATORY FIRST)
1. Verify context
2. Confirm prerequisites
3. Output: "Phase 0 complete"

### Pass 1: [ACTION] ([QUALIFIER])
1. Specific steps
2. Output: "Pass 1 complete. [Summary]"
3. DO NOT [other things]

### Pass 2: [ACTION] ([QUALIFIER])
[Repeat pattern]

### Phase N: Verification (MANDATORY LAST)
1. Quality checks
2. Success criteria
3. Output: "Phase N complete. [Summary]"

## Critical Patterns

### The AI Failure Pattern (NEVER DO THIS)
❌ Combining operations

### The Correct Pattern (DO THIS)
✅ One pass, one purpose

## Success Metrics
- Measurable outcomes
```

### Size Target for Protocol Skills

**garden-protocol:** 35KB (7 passes + examples + philosophy)

**Typical protocol skill:**
- Simple protocol (3-4 passes): 20-35KB
- Medium protocol (5-7 passes): 30-50KB
- Complex protocol (8+ passes): 45-70KB

**When to split:**
- If exceeds 80KB, consider progressive disclosure
- Move detailed examples to examples/
- Move methodology deep-dive to references/

## Comparison to Other Skills

**Protocol skills:**
- garden-protocol (journal tending)
- skill-creator (this skill)
- Potential: weekly-review, code-refactor

**Simple skills:**
- business-writing (templates)
- coaching-frameworks (pattern extraction)

**Platform skills:**
- claude-code-platform (comprehensive knowledge)

**Key difference:**
- Protocol = systematic multi-pass process
- Simple = templates/guidelines/voice
- Platform = comprehensive reference knowledge

## Production Evidence

**From Sid's journals:**
- Daily usage (every journal close)
- Consistent results (40-60 line journals)
- Fast execution (~2 min proven)
- Natural voice preserved
- Curiosity maintained

**From MEMORY.md:**
> "The 4-Pass Technique discovery validated separation of concerns for AI"
> "Garden protocol proven daily - transforms verbose entries to scannable narratives"

**From NOW.md learnings:**
> "7-pass protocol working perfectly - used during every journal close"

---

*"Protocol pattern = Phase 0 → Pass 1-N (one job each) → Phase N"*
*"Separation of concerns makes right behavior inevitable for AI"*
*"garden-protocol = proven daily in production, gold standard protocol skill"*
