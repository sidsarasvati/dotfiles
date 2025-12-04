# Template: Protocol Skill

> **Use when:** Multi-pass systematic process, separation of concerns needed
> **Size target:** 30-70KB
> **Structure:** SKILL.md with Phase 0 → Pass 1-N → Phase N pattern

## When to Use This Template

**Choose protocol skill template for:**
- Multi-step systematic processes (journal tending, refactoring, analysis)
- Clear separation of concerns needed (each pass has ONE job)
- Repeatable methodology (same steps every time)
- Order-dependent operations (Pass 2 depends on Pass 1 results)

**Don't use for:**
- Simple templates/guidelines (use simple-skill template)
- Single-pass operations (not multi-step)
- Flexible workflows (protocol implies fixed sequence)

## Directory Structure

```
protocol-name/
└── SKILL.md (all protocol content)
```

**Optional expansion:**
```
protocol-name/
├── SKILL.md (protocol overview)
├── references/
│   └── detailed-methodology.md (deep dive on each pass)
└── examples/
    └── gold-standards.md (example executions)
```

## Template: SKILL.md

```markdown
---
name: protocol-name
description: Use when [SPECIFIC SCENARIO] to apply the [X]-pass [PROTOCOL NAME]. Auto-activates for [TRIGGER 1], [TRIGGER 2], or [TRIGGER 3]. Ensures [DESIRED OUTCOME] not [ANTI-PATTERN].
---

# Protocol Name: [X]-Pass [Type]

> **"Quote capturing protocol essence"**
> **"Second quote about separation of concerns"**

## Core Principle

[2-3 sentences explaining the transformation this protocol achieves and why multi-pass approach works]

## When to Use This Skill

- Specific scenario 1 (when to apply protocol)
- Specific scenario 2
- Specific scenario 3
- **When NOT to use:** [Scenarios where protocol doesn't apply]

## The [X]-Pass Protocol

### Phase 0: DETECTION (MANDATORY FIRST)

**Purpose:** [What Phase 0 verifies before executing]

1. **Verify [context requirement 1]**
2. **Check [context requirement 2]**
3. **Confirm [context requirement 3]**
4. **Output**: "Phase 0 complete. [What's confirmed]"
5. **STOP**: Don't proceed until context verified

**Why Phase 0:** [What failure this prevents]

### Pass 1: [ACTION VERB] ([QUALIFIER])

**Purpose:** [What this pass accomplishes - ONE thing only]

**Steps:**
1. Specific step 1
2. Specific step 2
3. Specific step 3
4. **Output**: "Pass 1 complete. [Summary of changes]"
5. **DO NOT [other actions]** (enforces separation)

**Why [qualifier]:** [Why this constraint matters]

### Pass 2: [ACTION VERB] ([QUALIFIER])

**Purpose:** [What this pass accomplishes]

**Steps:**
1. Specific step 1
2. Specific step 2
3. **Output**: "Pass 2 complete. [Summary]"
4. **DO NOT [other actions]**

**Why [qualifier]:** [Reasoning]

### Pass 3: [ACTION VERB] ([QUALIFIER])

[Continue pattern for all passes]

### Pass N: [FINAL ACTION] ([QUALIFIER])

**Purpose:** [What final pass accomplishes]

**Steps:**
[Steps for final pass]

**Output**: "Pass N complete. [Summary]"

### Phase N: VERIFICATION (MANDATORY LAST)

**Purpose:** Confirm protocol achieved desired outcome

**Checklist:**
- [ ] Verification item 1 (what to check)
- [ ] Verification item 2
- [ ] Verification item 3
- [ ] Overall quality test: [The key test]

**Success criteria:**
1. Criterion 1 (measurable)
2. Criterion 2 (measurable)
3. Criterion 3 (measurable)

**Output**: "Phase N complete. [Outcome summary: before→after metrics]"

**Why Phase N:** [What failure this catches]

## Critical Patterns

### The AI Failure Pattern (NEVER DO THIS)

❌ "I'll be efficient and combine Pass X and Pass Y"
❌ "Let me fix everything at once to save time"
❌ "I can see all issues so I'll handle them together"

**Why this fails:**
- Cognitive overload (AI can't handle multiple concerns)
- Forgotten steps (combining operations = missing things)
- No verification (can't confirm individual pass completion)

### The Correct Pattern (DO THIS)

✅ One pass, one purpose (complete it fully)
✅ Output confirmation after each pass (audit trail)
✅ STOP between passes (don't rush to next)
✅ Complete protocol start to finish (no skipping)

**Why this works:**
- Separation of concerns (each pass independently verifiable)
- Forced sequencing (can't skip Phase 0 or Phase N)
- Built-in verification (catches failures immediately)
- Right behavior emerges from structure

## Success Metrics

**Execution time:** [Estimated time for full protocol]
- Phase 0: [X seconds]
- Pass 1: [X seconds]
- Pass 2: [X seconds]
- [Continue for all passes]
- Phase N: [X seconds]
- **Total: [X minutes] methodical > [Y minutes] confused**

**Quality outcomes:**
- [ ] [Measurable outcome 1]
- [ ] [Measurable outcome 2]
- [ ] [Measurable outcome 3]

**The test:** [One clear question to verify protocol success]

## Key Principles

1. **Phase 0 mandatory** - Verify context before executing
2. **One pass, one job** - No combining operations
3. **Output confirmations** - After every pass
4. **Explicit prohibitions** - "DO NOT [X]" prevents AI combining
5. **Phase N mandatory** - Verify completion before declaring done
6. **Read-only first** (if applicable) - Observe before acting
7. **Separation of concerns** - Each pass independently testable

## [Optional: Detailed Methodology]

[If protocol needs more explanation, add deep dive sections here]

## [Optional: Examples]

### Example Execution

**Before:**
[State before protocol]

**After applying protocol:**
[State after protocol]

**Transformation:**
- Metric 1: [Before → After]
- Metric 2: [Before → After]
- Metric 3: [Before → After]

---

*"Phase 0 → Pass 1-N (one job each) → Phase N = right behavior emerges"*
*"Separation of concerns prevents AI failure modes"*
```

## Customization Guide

### Step 1: Define Your Passes

**Questions to answer:**
1. How many passes needed? (3-7 typical, 8+ consider splitting)
2. What's the natural order? (dependencies between passes)
3. What's ONE job per pass? (keep focused)
4. What's the qualifier for each? (READ ONLY, MERGE ONLY, etc.)

**Pass naming pattern:**
```
Pass X: [ACTION VERB] ([QUALIFIER])
```

**Examples:**
- Pass 1: Walk the Garden (READ ONLY)
- Pass 2: Absorb Field Reports (INTEGRATION ONLY)
- Pass 3: Merge Duplicates (CONSOLIDATION ONLY)
- Pass 4: Prune Bloat (CURIOSITY LENS)
- Pass 5: Connect (REFERENCES ONLY)
- Pass 6: Color (EMPHASIS ONLY)

### Step 2: Design Phase 0

**Phase 0 must verify:**
- Context is correct (right file, right date, right state)
- Prerequisites met (data available, tools ready)
- Safe to proceed (no destructive operations about to happen)

**Common Phase 0 checks:**
- Verify current date/time
- Confirm target file exists and is correct
- Check state/status matches expectations
- Load examples or reference data
- Output confirmation before proceeding

**Why critical:**
- AI makes context mistakes (edits wrong file, wrong date)
- Phase 0 catches these before damage done
- Explicit STOP prevents rushing to execution

### Step 3: Design Phase N

**Phase N must verify:**
- All passes completed successfully
- Desired outcome achieved
- Quality metrics met
- No silent failures

**Common Phase N checks:**
- Checklist of completion criteria (8-12 items typical)
- Before/after metrics (X→Y lines, A→B quality)
- The test (one clear success question)
- Output summary with transformation

**Why critical:**
- AI thinks it's done when it's not (silent failures)
- Phase N catches incomplete work
- Explicit verification before declaring success

### Step 4: Write Output Confirmations

**Every pass MUST end with:**
```markdown
**Output**: "Pass X complete. [Specific summary of changes]"
```

**Good outputs:**
- "Pass 2 complete. Absorbed: 3 field reports into time blocks"
- "Pass 4 complete. Pruned: 4 sections (178→52 lines). Flagged: metrics for extraction"
- "Phase N complete. Journal is zen garden: 203→47 lines. Essence: 5🍅 victory day"

**Bad outputs:**
- "Pass 2 done" (no detail)
- "Made some changes" (vague)
- [No output] (AI skips confirmation)

**Why critical:**
- Forces pass completion (can't output until done)
- Creates audit trail (user sees what happened)
- Prevents skipping (must confirm before next pass)

### Step 5: Add Prohibitions

**Each pass should have:**
```markdown
**DO NOT [other actions]** 
```

**Examples:**
- "DO NOT FIX ANYTHING YET" (Pass 1: Read-only)
- "ONLY integrate reports, nothing else" (Pass 2: Focused action)
- "DO NOT add links or emphasis" (Pass 3: Merging only)

**Why critical:**
- AI naturally combines operations (efficiency bias)
- Explicit prohibition prevents this
- Enforces separation of concerns

## Sizing Guidelines

**Pass count vs size:**
- 3-4 passes: 25-40KB
- 5-7 passes: 35-60KB
- 8-10 passes: 50-80KB
- 10+ passes: Consider splitting or progressive disclosure

**When to expand structure:**
If protocol exceeds 70KB:
```
protocol-name/
├── SKILL.md (protocol overview, always loaded)
├── references/
│   └── pass-details.md (deep dive on each pass)
└── examples/
    └── execution-examples.md (gold standards)
```

## Testing Your Protocol Skill

### Test Full Execution

1. **Apply to real scenario** (not theoretical)
2. **Follow protocol exactly** (all passes, no shortcuts)
3. **Note any confusion** (unclear steps, missing guidance)
4. **Measure time** (actual execution time per pass)
5. **Verify outcome** (Phase N checklist all passed?)

### Test Separation of Concerns

**Try to break it:**
- Can you combine passes? (should be prevented)
- Can you skip Phase 0? (should fail)
- Can you skip Phase N? (should be blocked)
- Do prohibitions work? (AI respects DO NOT?)

**If AI combines operations:**
- Add stronger prohibitions
- Make output confirmations more explicit
- Clarify qualifier for each pass

### Test Auto-Activation

**Trigger phrases (5+):**
- Direct request: "[Action] using [protocol name]"
- Implicit task: "[Task] for [scenario]"
- Related question: "How do I [task]?"

**Verify:**
- Loads when should (all valid triggers)
- Doesn't load when shouldn't (out of scope)
- Description clear about when to activate

## Examples of This Pattern

**From Sid's skills:**
- `garden-protocol` - 7-pass journal tending (gold standard)
- `skill-creator` - 6-pass skill creation (this skill)

**Potential applications:**
- `weekly-review` - Multi-pass pattern analysis
- `code-refactor` - Read → Analyze → Refactor → Verify
- `writing-polish` - Draft → Structure → Clarity → Emphasis

## Common Variations

### Variation 1: Conditional Passes

**Pattern:**
```markdown
### Pass 5: Extract Homeless Knowledge (CONDITIONAL)

**ONLY IF** Pass 4 flagged content with no home

[Steps if condition met]
[Skip if condition not met]
```

**Use when:** Not every execution needs this pass

### Variation 2: Loop-Back Passes

**Pattern:**
```markdown
### Pass 4: Refine and Test

[Refinement steps]

**If tests fail:** Return to Pass 2 for fixes
**If tests pass:** Continue to Pass 5
```

**Use when:** Quality gate determines next action

### Variation 3: Parallel Passes

**Pattern:**
```markdown
### Pass 3A and 3B: Parallel Operations

**3A: [Action 1] (can run simultaneously)**
[Steps]

**3B: [Action 2] (can run simultaneously)**
[Steps]

**After both complete:** Continue to Pass 4
```

**Use when:** Independent operations, no sequential dependency

## Execution Time Estimation

**Calculate total time:**
```
Phase 0:     [X sec]
Pass 1:      [X sec]
Pass 2:      [X sec]
...
Pass N:      [X sec]
Phase N:     [X sec]
-----------------------
Total:       [Y min]
```

**Compare to alternatives:**
```
Protocol approach:    [Y min] methodical, complete
Ad-hoc approach:      [Z min] confused, incomplete
```

**Include in skill:**
Helps user understand time investment and value

## Next Steps After Creating

1. **Test with real usage** (not theoretical scenario)
2. **Measure actual execution time** (update estimates)
3. **Note where AI tries to combine** (strengthen prohibitions)
4. **Refine pass descriptions** (clarity based on usage)
5. **Document learnings** (MEMORY.md insights)
6. **Iterate based on failures** (protocol evolution)

---

*"Protocol pattern = behavioral strata that make right behavior inevitable"*
*"One pass, one job, one confirmation = methodical excellence over chaos"*
*"Phase 0 prevents wrong-context disasters, Phase N catches silent failures"*
