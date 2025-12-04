# Description Writing: Auto-Activation Trigger Design

> **"Description is the most critical field - it determines when skill loads"**
> **"Good description = precise auto-activation, bad description = noise or silence"**

## Core Principle

The `description` field in YAML frontmatter is analyzed by Claude to determine when your skill is relevant. Write it well, and the skill auto-activates exactly when needed. Write it poorly, and it either never loads or loads constantly.

## The Auto-Activation Mechanism

### How Claude Decides

When processing a conversation, Claude:

1. **Analyzes user message** (words, intent, context)
2. **Checks available skills** (reads description fields)
3. **Matches relevance** (does description align with task?)
4. **Loads matching skills** (adds to conversation context)
5. **Uses expertise** (follows loaded skill instructions)

### What Claude Looks For

**User intent:**
- What is user trying to do?
- "create a skill" → skill-creator
- "close this journal" → garden-protocol
- "write email to prospect" → business-writing

**Domain keywords:**
- Specific tools mentioned (Logseq, Claude Code, MCP)
- Task types (journal, email, protocol)
- Actions (create, close, write, review)

**Context signals:**
- Files being edited (journals/2025_10_27.md → garden-protocol)
- Tools being used (Bash, Read, etc.)
- Previous messages in thread

## Description Formula

### Template Structure

**Option 1: Task-focused**
```
Use when [SPECIFIC SCENARIO] to [PURPOSE]. Auto-activates for [TRIGGER EXAMPLES]. Provides [CONCRETE VALUE].
```

**Option 2: Value-focused**
```
Provides [SPECIFIC VALUE] for [USE CASES]. Auto-activates when [CONDITIONS]. Includes [KEY FEATURES].
```

**Option 3: Hybrid**
```
Use when [SCENARIO]. Provides [VALUE]. Auto-activates for [TRIGGERS] or [CONDITIONS].
```

### Component Breakdown

**[SPECIFIC SCENARIO]:**
- Not: "for Logseq work"
- Yes: "closing or reviewing daily journal entries in Logseq"
- Precision matters - be specific about WHEN

**[PURPOSE]:**
- Not: "to make it better"
- Yes: "to apply the 7-pass garden tending protocol"
- Clear goal - what are we achieving?

**[TRIGGER EXAMPLES]:**
- Not: "journal stuff"
- Yes: "journal closing", "pruning entries", "quality checks"
- Phrases users actually say

**[CONCRETE VALUE]:**
- Not: "helpful guidance"
- Yes: "6-step creation protocol with embedded docs and templates"
- Specific deliverables

**[CONDITIONS]:**
- Not: "when needed"
- Yes: "when discussing or using Claude Code functionality"
- Observable criteria

## Writing Effective Triggers

### Trigger Term Types

**1. Action Verbs (what user is doing)**
- create, build, design, write, draft
- close, review, analyze, refactor, prune
- setup, configure, integrate, implement

**2. Domain Keywords (subject area)**
- Logseq, journal, skill, protocol, framework
- Claude Code, MCP, tool, bash, command
- B2B, email, proposal, message, communication

**3. Task Indicators (specific activities)**
- "closing daily journal"
- "creating a skill"
- "writing to prospect"
- "reviewing code patterns"

**4. Tool/Context Mentions**
- "in Logseq"
- "for Claude Code"
- "using MCP"
- "with bash tool"

### Combining Triggers

**Use multiple trigger types:**
```yaml
description: Use when creating, designing, or building Claude Code skills. Auto-activates for "create a skill", "build a skill", or "make this into a skill".
```

**Why it works:**
- Action verbs: creating, designing, building
- Domain keyword: Claude Code skills
- Explicit phrases: "create a skill", "build a skill"
- Multiple entry points increase match probability

### Trigger Examples to Include

**Quoted phrases (high signal):**
```yaml
Auto-activates for "create a skill", "build a skill", "skill for X"
```

**General patterns (broader match):**
```yaml
Auto-activates when closing or reviewing journal entries
```

**Combined approach (best):**
```yaml
Auto-activates for "close journal", "review entry", or daily journal quality checks
```

## Scope Control

### Too Broad (Anti-Pattern)

❌ `description: For all Logseq operations`
- Activates constantly
- Noise overwhelms signal
- User gets irrelevant skills

❌ `description: Helps with work tasks`
- Meaningless - everything is "work"
- Never relevant because always relevant
- No useful filtering

### Too Narrow (Anti-Pattern)

❌ `description: Only when user types exactly "please close today's journal entry for Oct 27, 2025"`
- Never activates (too specific)
- Brittle - breaks with slight variation
- Misses valid use cases

❌ `description: For journal entries on Sundays`
- Arbitrary restriction
- Misses 6/7 valid cases
- No real benefit to narrowness

### Just Right (Pattern)

✅ `description: Use when closing or reviewing daily journal entries in Logseq`
- Specific scenario (daily journals, not all Logseq)
- Multiple valid triggers (closing, reviewing)
- Clear boundary (not all journal work, just daily entries)
- Activates when needed, not when not

✅ `description: Use when creating, designing, or building Claude Code skills`
- Specific domain (skills, not all Claude Code)
- Multiple action verbs (create, design, build)
- Clear scope (skills, not commands or protocols)
- Focused but not brittle

## Value Proposition

### What Value Means

**Not value:**
- "provides help" (vague)
- "makes it better" (unmeasurable)
- "useful for X" (unclear benefit)

**Real value:**
- "6-step creation protocol" (concrete methodology)
- "embedded Anthropic documentation" (no fetching needed)
- "working examples and templates" (immediate usability)
- "7-pass garden tending protocol" (specific process)

### Articulating Value

**Formula:**
```
Provides [METHODOLOGY/FRAMEWORK] with [SPECIFIC FEATURES/RESOURCES]
```

**Examples:**

"Provides complete 6-step creation protocol, embedded Anthropic documentation, working examples, and templates"
- Methodology: 6-step protocol
- Features: embedded docs, examples, templates

"Ensures journals are zen gardens (curiosity-driven, scannable) not databases (verbose, complete)"
- Outcome: zen gardens vs databases
- Qualities: curiosity-driven, scannable

"Provides comprehensive knowledge about Claude Code platform features, tools, MCP integration, and advanced capabilities"
- Scope: comprehensive
- Topics: platform features, tools, MCP, advanced capabilities

## Testing Descriptions

### The 5-Phrase Test

**Write 5 realistic user phrases:**
1. "Help me create a skill"
2. "I want to build a skill for X"
3. "How do I make this into a skill?"
4. "Can you help design a Claude skill?"
5. "I need to create a new skill"

**Test activation:**
- Should activate: Phrases 1, 2, 3, 5 (directly mention skill creation)
- Should activate: Phrase 4 (Claude skill = skill for Claude Code)
- Should NOT activate: "Help me close journal" (different skill)

**Refine description based on results.**

### The Boundary Test

**Should activate:**
- Phrases directly related to skill purpose
- Variations using synonyms
- Implicit tasks in domain

**Should NOT activate:**
- Unrelated domains
- Generic conversation
- Different tools/contexts

**Test both sides of boundary:**

```yaml
description: Use when closing or reviewing daily journal entries in Logseq
```

**Should activate:**
- "Close today's journal"
- "Review my journal entry"
- "Clean up this Logseq journal"

**Should NOT activate:**
- "Create new journal page" (different task)
- "Search all journals" (not daily entry work)
- "Help me write email" (different domain)

### Iteration Process

1. **Write v1 description** (best guess)
2. **Test 5 phrases** (realistic triggers)
3. **Note false positives** (activated when shouldn't)
4. **Note false negatives** (didn't activate when should)
5. **Refine description** (adjust scope/triggers)
6. **Test again** (verify improvements)
7. **Repeat until clean** (usually 2-3 iterations)

## Real Examples Analyzed

### Example 1: garden-protocol

```yaml
description: Use when closing or reviewing daily journal entries in Logseq to apply the 7-pass garden tending protocol. Ensures journals are zen gardens (curiosity-driven, scannable) not databases (verbose, complete). Auto-activates for journal closing, pruning, or quality checks.
```

**Breakdown:**
- **Scenario:** "closing or reviewing daily journal entries in Logseq"
- **Purpose:** "to apply the 7-pass garden tending protocol"
- **Triggers:** "journal closing, pruning, or quality checks"
- **Value:** "zen gardens (curiosity-driven, scannable) not databases (verbose, complete)"

**Why it works:**
- Specific task (daily journals, not all Logseq)
- Clear methodology (7-pass protocol)
- Multiple triggers (closing, pruning, quality checks)
- Outcome articulated (zen gardens vs databases)
- Domain bounded (Logseq journal entries)

**Test phrases that activate:**
- "Close today's journal"
- "Review my journal entry quality"
- "Prune this journal for readability"
- "Apply garden protocol to journal"

### Example 2: skill-creator

```yaml
description: Use when creating, designing, or building Claude Code skills. Auto-activates for "create a skill", "build a skill", "skill for X", or "make this into a skill". Provides complete 6-step creation protocol, embedded Anthropic documentation, working examples, and templates.
```

**Breakdown:**
- **Actions:** "creating, designing, or building"
- **Domain:** "Claude Code skills"
- **Explicit triggers:** "create a skill", "build a skill", "skill for X", "make this into a skill"
- **Value:** "6-step creation protocol, embedded Anthropic documentation, working examples, and templates"

**Why it works:**
- Multiple action verbs (create, design, build)
- Specific domain (skills, not general Claude Code)
- Quoted trigger phrases (high signal)
- Comprehensive value statement (4 concrete deliverables)

**Test phrases that activate:**
- "Help me create a skill"
- "I want to build a skill for X"
- "Design a new Claude skill"
- "Make this into a skill"

### Example 3: business-writing

```yaml
description: Use when writing B2B emails, proposals, or business communications in Sid's voice. Auto-activates for "write email", "draft message", or business correspondence tasks. Provides templates and voice guidelines.
```

**Breakdown:**
- **Use cases:** "B2B emails, proposals, or business communications"
- **Context:** "in Sid's voice"
- **Triggers:** "write email", "draft message", "business correspondence tasks"
- **Value:** "templates and voice guidelines"

**Why it works:**
- Clear domain (business writing, not personal)
- Specific voice (Sid's, not generic)
- Common triggers (write email, draft message)
- Practical value (templates + guidelines)

**Test phrases that activate:**
- "Write email to prospect"
- "Draft message for customer"
- "Help me write B2B proposal"
- "Business communication in Sid's voice"

### Example 4: claude-code-platform

```yaml
description: Provides comprehensive knowledge about Claude Code platform features, tools, MCP integration, and advanced capabilities. Auto-activates when discussing or using Claude Code functionality.
```

**Breakdown:**
- **Value:** "comprehensive knowledge"
- **Topics:** "platform features, tools, MCP integration, and advanced capabilities"
- **Triggers:** "discussing or using Claude Code functionality"

**Why it works:**
- Broad but bounded (Claude Code platform, not all AI)
- Lists key topics (helps matching)
- Natural activation (discussing or using)
- Platform-specific (clear scope)

**Test phrases that activate:**
- "How does MCP work in Claude Code?"
- "What tools does Claude Code have?"
- "Using bash tool in Claude"
- "Claude Code advanced features"

## Common Mistakes & Fixes

### Mistake 1: No Triggers

❌ `description: Helpful for journal work`

**Problem:** No specific triggers, too vague
**Fix:** Add explicit trigger phrases and scenarios

✅ `description: Use when closing or reviewing daily journal entries. Auto-activates for "close journal", "review entry", or journal quality checks.`

### Mistake 2: Too Generic

❌ `description: For all coding tasks`

**Problem:** Activates constantly, noise
**Fix:** Narrow to specific domain

✅ `description: Use when creating, debugging, or refactoring Python scripts. Auto-activates for Python development tasks.`

### Mistake 3: No Value Statement

❌ `description: Use when working with skills`

**Problem:** Doesn't explain what skill provides
**Fix:** Add concrete value proposition

✅ `description: Use when creating Claude Code skills. Provides 6-step creation protocol, embedded documentation, and templates.`

### Mistake 4: One Trigger Only

❌ `description: Use when user says "create skill"`

**Problem:** Too narrow, misses variations
**Fix:** Include multiple trigger patterns

✅ `description: Use when creating, designing, or building skills. Auto-activates for "create a skill", "build skill", or "make this a skill".`

### Mistake 5: Abstract Language

❌ `description: Enhances productivity through optimized workflows`

**Problem:** Corporate jargon, no clear triggers
**Fix:** Use concrete language and specific tasks

✅ `description: Use when closing daily journal entries in Logseq to apply 7-pass tending protocol. Auto-activates for journal closing or pruning tasks.`

## Advanced Techniques

### Technique 1: Quoted Phrases (High Signal)

**Pattern:**
```yaml
Auto-activates for "exact phrase 1", "exact phrase 2", or "phrase 3"
```

**Why it works:**
- Quotes signal high-confidence triggers
- Multiple variations catch different phrasings
- User naturally says these phrases

**Example:**
```yaml
Auto-activates for "create a skill", "build a skill", or "make this into a skill"
```

### Technique 2: Context Qualifiers

**Pattern:**
```yaml
Use when [task] in [context] for [purpose]
```

**Why it works:**
- Bounds activation to specific context
- Prevents false positives in other domains
- Clear when skill applies

**Example:**
```yaml
Use when closing or reviewing daily journal entries in Logseq to apply garden protocol
```

### Technique 3: Value-First

**Pattern:**
```yaml
Provides [specific value]. Auto-activates when [conditions].
```

**Why it works:**
- Leads with benefit
- Clear what skill delivers
- Activation conditions explicit

**Example:**
```yaml
Provides 6-step skill creation protocol with embedded docs and templates. Auto-activates for "create skill" or skill development tasks.
```

### Technique 4: Outcome Statement

**Pattern:**
```yaml
Ensures [desired outcome] not [anti-pattern]. Auto-activates for [task].
```

**Why it works:**
- Articulates transformation
- Clear before/after state
- Motivates usage

**Example:**
```yaml
Ensures journals are zen gardens (scannable, curiosity-driven) not databases (verbose, complete). Auto-activates for journal closing or quality checks.
```

## Quick Reference

**Description checklist:**
- [ ] 1-3 sentences (concise)
- [ ] Specific scenario or use case
- [ ] Multiple trigger terms/phrases
- [ ] Concrete value statement
- [ ] Not too broad (activates constantly)
- [ ] Not too narrow (never activates)
- [ ] Tested with 5+ realistic phrases
- [ ] Boundary tested (should/shouldn't activate)

**Formula:**
```
Use when [SPECIFIC SCENARIO] to [PURPOSE]. Auto-activates for [TRIGGER 1], [TRIGGER 2], or [TRIGGER 3]. Provides [CONCRETE VALUE with FEATURES].
```

**Testing process:**
1. Write description
2. Test 5 phrases (should activate)
3. Test 3 phrases (should NOT activate)
4. Refine based on results
5. Repeat until accurate

---

*"Description determines activation - make every word count"*
*"Specific triggers + clear value = perfect auto-activation"*
