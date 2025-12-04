# Skill Creation Protocol: Detailed Methodology

> **"Protocol thinking applied to skill creation"**
> **"Phase 0 → Pass 1-6 → Phase N = separation of concerns"**

## The Universal Pattern

Every skill creation follows this structure:

```
Phase 0: DETECTION → Know what you're building
Pass 1-6: EXECUTION → Build one aspect at a time
Phase N: VERIFICATION → Confirm it works
```

## Why This Works

**Separation of concerns:**
- Each phase has ONE job
- No cognitive overload from multitasking
- Clear progress markers
- Easy to debug if something fails

**Forced sequencing:**
- Can't skip Phase 0 (must know skill type first)
- Can't rush to Pass 6 (must build foundation)
- Each pass builds on previous

**Built-in verification:**
- Phase N catches failures before deployment
- Testing confirms auto-activation works
- No silent errors

## Phase 0: DETECTION (What Kind of Skill?)

### Purpose
Understand what you're building before writing any code.

### Questions to Ask

**1. What domain expertise does this package?**
- Specific area (journal tending, B2B emails)
- Broad knowledge (Claude Code platform, protocol design)
- Single pattern (one methodology)
- Multiple patterns (collection of techniques)

**2. When should it auto-activate?**
- User says specific phrases ("create a skill")
- Working on specific tasks (closing journal)
- Using specific tools (Logseq operations)
- Asking about specific topics (MCP integration)

**3. Is this simple or complex?**
- **Simple:** One pattern, straightforward guidance (business-writing)
- **Protocol:** Multi-pass methodology (garden-protocol)
- **Complex:** Multiple domains, extensive knowledge (claude-code-platform)

**4. Will it execute tools/code?**
- **Passive:** Provides guidance only
- **Active:** Runs bash, reads files, checks state
- **Hybrid:** Guidance + verification commands

### Output

"Building [TYPE] skill: [NAME]"

**Examples:**
- "Building PROTOCOL skill: garden-protocol"
- "Building SIMPLE skill: business-writing"
- "Building COMPLEX skill: claude-code-platform"
- "Building ACTIVE skill: calendar-operations"

### Decision Tree

```
Domain expertise identified?
├─ Yes → Continue
└─ No → STOP, research more

Auto-activation scenario clear?
├─ Yes → Continue
└─ No → STOP, clarify triggers

Skill type determined?
├─ Simple → Use simple-skill template
├─ Protocol → Use protocol-skill template
├─ Complex → Plan multiple supporting files
└─ Unclear → STOP, analyze examples

Tool execution needed?
├─ Yes → Use code-execution-skill template
└─ No → Passive guidance skill
```

### Common Mistakes

❌ Rushing past Phase 0 ("I know what to build")
❌ Unclear activation scenario ("when user needs help")
❌ Mixed types ("this is both simple and complex")

✅ Spend 2-3 minutes on Phase 0
✅ Clear activation trigger ("when closing journal entries")
✅ Single type determined (can evolve later)

## Pass 1: Architecture Design

### Purpose
Plan the skill structure before writing content.

### Key Decisions

**1. What goes in SKILL.md?**
- Core protocol overview
- Quick reference
- When to use / when not to use
- Examples of typical usage
- Pointers to references/

**Size target:** 10-40KB for SKILL.md

**2. What goes in references/?**
- Detailed methodology
- Advanced patterns
- Embedded external docs
- Edge cases and troubleshooting

**Size target:** 10-30KB per reference file

**3. What goes in examples/?**
- Working examples from real usage
- Templates for common patterns
- Case studies showing why it works

**Size target:** 5-15KB per example

**4. Overall size target?**
- Simple skill: 20-40KB total
- Medium skill: 40-70KB total
- Complex skill: 70-120KB total

### Architecture Patterns

**Pattern 1: Simple Skill**
```
skill-name/
└── SKILL.md (all content in one file)
```
Use when: Single pattern, minimal content

**Pattern 2: Skill with References**
```
skill-name/
├── SKILL.md (overview)
└── references/
    ├── detailed-guide.md
    └── advanced-topics.md
```
Use when: Core + advanced knowledge needed

**Pattern 3: Skill with Examples**
```
skill-name/
├── SKILL.md (overview)
└── examples/
    ├── example-1.md
    └── templates/
        └── template.md
```
Use when: Showing patterns helps

**Pattern 4: Full Structure**
```
skill-name/
├── SKILL.md (entry point)
├── references/
│   ├── methodology.md
│   └── embedded-docs.md
└── examples/
    ├── case-study.md
    └── templates/
        └── starting-point.md
```
Use when: Complex skill with many aspects

### Output

**Document structure:**
```
Architecture for [SKILL-NAME]:

SKILL.md (25KB estimated)
- Core protocol
- Quick reference
- Pointers to deep docs

references/ (40KB estimated)
- detailed-methodology.md (20KB)
- embedded-external-docs.md (15KB)
- troubleshooting.md (5KB)

examples/ (15KB estimated)
- working-example.md (8KB)
- templates/simple-template.md (7KB)

Total: ~80KB
Progressive disclosure: SKILL.md loads first
```

### Common Mistakes

❌ Everything in SKILL.md (100KB file)
❌ Too many small files (fragmentation)
❌ No size estimates (uncontrolled growth)

✅ Balance: core in SKILL.md, details in references/
✅ Logical grouping (2-5 files in references/)
✅ Target sizes documented

## Pass 2: Write YAML Frontmatter

### Purpose
Define skill metadata for auto-activation.

### Required Fields

**name:**
- Lowercase with hyphens
- Matches folder name exactly
- Descriptive and concise
- Example: `garden-protocol`, `business-writing`, `skill-creator`

**description:**
- Triggers auto-activation (CRITICAL)
- 1-3 sentences
- Starts with "Use when..." or "Provides..."
- Includes key trigger terms

### Description Writing Formula

**Template:**
```
Use when [SCENARIO] to [PURPOSE]. Auto-activates for [TRIGGERS]. Provides [VALUE].
```

**Examples:**

```yaml
description: Use when closing or reviewing daily journal entries in Logseq to apply the 7-pass garden tending protocol. Ensures journals are zen gardens (curiosity-driven, scannable) not databases (verbose, complete). Auto-activates for journal closing, pruning, or quality checks.
```

```yaml
description: Use when creating, designing, or building Claude Code skills. Auto-activates for "create a skill", "build a skill", "skill for X", or "make this into a skill". Provides complete 6-step creation protocol, embedded Anthropic documentation, working examples, and templates.
```

```yaml
description: Provides comprehensive knowledge about Claude Code platform features, tools, MCP integration, and advanced capabilities. Auto-activates when discussing or using Claude Code functionality.
```

### Trigger Term Strategy

**Good trigger terms:**
- Specific actions: "closing journal", "create a skill"
- Domain keywords: "Logseq", "Claude Code", "B2B email"
- Task indicators: "reviewing", "designing", "writing"
- Tool mentions: "MCP", "bash tool", "calendar"

**Bad trigger terms:**
- Too generic: "work", "help", "task"
- Too narrow: "exactly this phrase only"
- Jargon: Internal terms users won't say

### Testing Description

**Test by:**
1. Reading aloud - does it make sense?
2. Checking triggers - do they match real usage?
3. Trying phrases - would user say these?
4. Verifying scope - not too broad or narrow?

### Output

**Valid YAML:**
```yaml
---
name: skill-name
description: Use when [clear scenario] to [specific purpose]. Auto-activates for [realistic triggers]. Provides [concrete value].
---
```

### Common Mistakes

❌ Description too broad ("for all Logseq work")
❌ No trigger terms ("provides help")
❌ Vague value ("makes things better")

✅ Specific scenario ("when closing daily journal entries")
✅ Clear triggers ("create a skill", "build a skill")
✅ Concrete value ("6-step creation protocol with templates")

## Pass 3: Core Instructions (SKILL.md body)

### Purpose
Write the main skill content that always loads.

### Standard Structure

```markdown
# Skill Name: One-Line Description

> **Quote capturing essence**
> **Second quote if helpful**

## Core Principle

[What makes this skill valuable in 2-3 sentences]

## When to Use This Skill

- Specific scenario 1
- Specific scenario 2
- Specific scenario 3
- Avoid using for X (anti-pattern)

## Main Protocol / Framework

[Step-by-step methodology - the core value]

### Phase 0 / Step 1 / Section 1

[Detailed instructions for this part]

### Pass 1 / Step 2 / Section 2

[Next part of methodology]

[Continue pattern...]

## Critical Patterns

### The AI Failure Pattern (NEVER DO THIS)

❌ Common mistake 1
❌ Common mistake 2

### The Correct Pattern (DO THIS)

✅ Right approach 1
✅ Right approach 2

## Success Metrics

- [ ] Checklist item 1
- [ ] Checklist item 2
- [ ] Verification method

## [Additional Sections as Needed]

[Quick reference, examples, troubleshooting, etc.]

## Deep Dive Resources

**For complete details, see:**
- `references/detailed-methodology.md` - [What's there]
- `references/advanced-patterns.md` - [What's there]
- `examples/working-example.md` - [What's there]

---

*Closing quote*
*Second quote if helpful*
```

### Writing Style

**Voice:**
- Sid's natural style (direct, clear, example-driven)
- Protocol thinking (behavioral strata)
- Active voice ("Do X" not "X should be done")
- Conversational but precise

**Structure:**
- Scannable (headers, bullets, code blocks)
- Progressive (simple → complex)
- Complete (everything needed to execute)
- Concise (no fluff)

**Examples:**
- Show real patterns
- Include good/bad comparisons
- Demonstrate why it works
- Keep examples brief (2-5 lines)

### Output

Complete SKILL.md body with:
- [ ] Title + essence quotes
- [ ] Core principle stated
- [ ] When to use (explicit scenarios)
- [ ] Main protocol (step-by-step)
- [ ] Critical patterns (do/don't)
- [ ] Success metrics (verification)
- [ ] References to supporting files
- [ ] Closing quotes

### Common Mistakes

❌ Abstract theory without examples
❌ Vague instructions ("do it well")
❌ No structure (wall of text)
❌ Everything in one section

✅ Concrete examples throughout
✅ Specific instructions ("Pass 1: Read entire journal")
✅ Clear hierarchy (headers, sub-headers)
✅ Logical sections with single purpose

## Pass 4: Supporting References

### Purpose
Create detailed documentation loaded on-demand.

### Reference File Types

**1. Methodology Deep Dive**
- Detailed explanation of each step
- Edge cases and variations
- Advanced techniques
- When to deviate from standard

**2. Embedded External Docs**
- Anthropic documentation
- Third-party resources
- Official guides
- Prevents repeated fetching

**3. Troubleshooting Guide**
- Common problems
- Solutions and fixes
- Debugging steps
- Prevention strategies

**4. Advanced Patterns**
- Expert-level techniques
- Optimizations
- Complex scenarios
- Power user features

### File Organization

**One concern per file:**
```
references/
├── detailed-methodology.md (the "how" in depth)
├── embedded-docs.md (external knowledge captured)
├── troubleshooting.md (problems + solutions)
└── advanced-patterns.md (expert techniques)
```

**Not:**
```
references/
└── everything.md (100KB mess)
```

### Content Structure

Each reference file should have:

```markdown
# Topic Name

> **Context for this document**

## Overview

[What this covers in 2-3 sentences]

## [Main Sections]

[Organized, detailed content]

## When to Use This Reference

[Explicit scenarios for loading this file]

---

*Closing thought*
```

### Progressive Disclosure

**SKILL.md points to references:**
```markdown
For detailed methodology, see `references/detailed-methodology.md`
For troubleshooting, see `references/troubleshooting.md`
```

**User asks specific question:**
- Claude loads relevant reference file
- Provides detailed answer
- User gets deep knowledge on-demand

### Output

Reference files created:
- [ ] Each file focused (one concern)
- [ ] Logical naming (descriptive, clear)
- [ ] Proper size (10-30KB each)
- [ ] Referenced from SKILL.md
- [ ] Complete content (no TODOs)

### Common Mistakes

❌ Dumping everything in one file
❌ Generic names (guide.md, info.md)
❌ No pointers from SKILL.md
❌ Incomplete content ("TBD")

✅ Focused files (one topic each)
✅ Descriptive names (yaml-guide.md)
✅ Clear references from main skill
✅ Complete, usable content

## Pass 5: Examples & Templates

### Purpose
Show working patterns and provide starting points.

### Example Types

**1. Working Examples**
- Real usage from existing skills
- Demonstrate successful patterns
- Explain why they work
- Show before/after if applicable

**2. Case Studies**
- Detailed analysis of specific skills
- Breakdown of structure
- Highlight key decisions
- Extract lessons learned

**3. Templates**
- Starting points for common patterns
- Placeholder text where needed
- Clear structure
- Easy to copy and modify

### Example Structure

**Working example format:**
```markdown
# Example: [Skill Name]

## Context

[What this skill does and why]

## Structure

[File organization]

## Key Decisions

[What made this effective]

## Lessons Learned

[What worked well, what to emulate]

## Full Example

[Relevant code/content snippets]
```

**Template format:**
```markdown
# Template: [Pattern Type]

## When to Use

[Scenarios for this template]

## Structure

[File organization to create]

## Template Content

```markdown
---
name: [skill-name]
description: [Your description here]
---

# [Skill Title]

[Template structure with placeholders]
```

## Customization

[What to change for your use case]
```

### Templates to Provide

**1. Simple Skill Template**
- Single SKILL.md file
- Basic structure
- For straightforward patterns

**2. Protocol Skill Template**
- Phase 0 → Pass 1-N → Phase N structure
- Multi-pass methodology
- For systematic processes

**3. Code-Execution Skill Template**
- Includes tool usage
- State verification
- For active skills

### Output

Examples and templates created:
- [ ] 2-3 working examples
- [ ] 2-3 templates for common patterns
- [ ] Clear explanations of why they work
- [ ] Easy to copy and modify
- [ ] Organized in examples/ folder

### Common Mistakes

❌ Abstract examples (not real usage)
❌ No explanation (just code dump)
❌ Complex templates (hard to customize)
❌ Missing templates folder

✅ Real examples from working skills
✅ Clear explanations of decisions
✅ Simple, customizable templates
✅ Organized folder structure

## Pass 6: Testing & Verification

### Purpose
Confirm skill works before declaring done.

### Test Auto-Activation

**Manual testing:**

1. **Open new conversation** (fresh context)
2. **Use trigger phrases** from description
   - "Help me create a skill"
   - "Close this journal entry"
   - "Write B2B email to prospect"
3. **Verify skill loads** (check context usage indicator)
4. **Confirm expertise provided** (correct guidance given)
5. **Refine description** if needed

**Test scenarios:**
- Direct request ("create a skill")
- Implicit task (starting journal work)
- Related question ("how do skills work?")
- Wrong context (shouldn't activate)

### Verify Structure

**Checklist:**
- [ ] YAML valid (name + description present)
- [ ] SKILL.md complete (all sections)
- [ ] References organized (if applicable)
- [ ] Examples demonstrate patterns (if applicable)
- [ ] Size reasonable (within target)
- [ ] Voice consistent (Sid's style throughout)
- [ ] No TODOs or placeholders
- [ ] Links between files work
- [ ] Progressive disclosure clear

### Test with Real Usage

**Best test:**
1. Use skill for actual task
2. Follow protocol as written
3. Note any confusion or gaps
4. Refine based on experience
5. Test again with improvements

### Common Issues & Fixes

**Skill doesn't auto-activate:**
- Check description trigger terms
- Make more specific
- Add key phrases users would say

**Wrong skill loads:**
- Description too broad
- Add specificity to narrow scope
- Remove generic terms

**Content unclear:**
- Add examples
- Simplify language
- Break into smaller steps

### Output

Testing complete:
- [ ] Auto-activation confirmed (3+ trigger tests)
- [ ] Structure verified (all checklist items)
- [ ] Real usage tested (actual task completed)
- [ ] Issues documented and fixed
- [ ] Ready for deployment

### Common Mistakes

❌ Skipping testing ("looks good to me")
❌ Only testing once (not enough scenarios)
❌ Not fixing issues found (leaving broken)

✅ Multiple trigger phrase tests
✅ Real usage scenario validation
✅ Issues fixed before deployment

## Phase N: VERIFICATION (Final Checks)

### Purpose
Final confirmation before declaring skill complete.

### Verification Checklist

**Functionality:**
- [ ] Auto-activates correctly on trigger phrases
- [ ] Provides immediate, actionable value
- [ ] Progressive disclosure works (references load on-demand)
- [ ] Examples demonstrate key patterns
- [ ] Templates ready to use

**Structure:**
- [ ] YAML frontmatter valid
- [ ] SKILL.md complete and scannable
- [ ] References organized logically
- [ ] Examples clear and helpful
- [ ] Size within target range

**Quality:**
- [ ] Voice consistent (Sid's style)
- [ ] No external dependencies (docs embedded)
- [ ] Examples from real usage (not theoretical)
- [ ] Protocol thinking applied (behavioral strata)
- [ ] Clear when to use / when not to use

**Integration:**
- [ ] Added to skills inventory
- [ ] Documented in MEMORY.md (if significant learning)
- [ ] Related skills updated (if applicable)
- [ ] Project CLAUDE.md mentions (if project-specific)

### Success Criteria

**The skill is complete when:**
1. Auto-activates on realistic trigger phrases (tested 3+ times)
2. Provides value immediately (user can execute without asking questions)
3. Size reasonable (20-120KB depending on complexity)
4. Voice authentic (sounds like Sid wrote it)
5. No external fetching needed (all docs embedded)
6. Examples work (tested with real usage)
7. Progressive disclosure functions (core loads first, details on-demand)

### Documentation

**Update files:**
- [ ] MEMORY.md (capture learnings from skill creation)
- [ ] NOW.md (if more work pending)
- [ ] Skills inventory (list of all skills)
- [ ] Project journal (document completion)

### Output

"Phase N complete. Skill verified and deployed."

**Summary includes:**
- Final size (KB)
- Auto-activation triggers tested
- Value provided (what it does)
- Lessons learned
- Integration complete

### Common Mistakes

❌ Skipping verification ("it's done")
❌ Not testing triggers (assumes it works)
❌ No documentation of completion

✅ Full checklist completion
✅ Multiple activation tests
✅ Learnings documented

## Execution Time Estimates

**Simple skill:**
- Phase 0: 2-3 min
- Pass 1: 3-5 min (architecture)
- Pass 2: 2-3 min (YAML)
- Pass 3: 8-12 min (core content)
- Pass 4: Skip or 3-5 min (if reference needed)
- Pass 5: 3-5 min (1-2 examples)
- Pass 6: 3-5 min (testing)
- Phase N: 2-3 min (verification)
- **Total: 25-40 minutes**

**Protocol skill:**
- Phase 0: 3-5 min
- Pass 1: 5-8 min (plan passes)
- Pass 2: 3-5 min (YAML)
- Pass 3: 15-25 min (protocol details)
- Pass 4: 8-12 min (methodology reference)
- Pass 5: 5-8 min (examples + templates)
- Pass 6: 5-8 min (testing)
- Phase N: 3-5 min (verification)
- **Total: 45-75 minutes**

**Complex skill:**
- Phase 0: 5-8 min (thorough analysis)
- Pass 1: 8-12 min (extensive architecture)
- Pass 2: 3-5 min (YAML)
- Pass 3: 20-30 min (comprehensive core)
- Pass 4: 15-25 min (multiple references)
- Pass 5: 10-15 min (many examples)
- Pass 6: 8-12 min (thorough testing)
- Phase N: 5-8 min (verification)
- **Total: 75-120 minutes**

## Key Principles

1. **One pass, one purpose** - Don't combine operations
2. **Complete before moving** - Finish current pass fully
3. **Test before declaring done** - Verification required
4. **Protocol thinking** - Behavioral strata, not instructions
5. **Separation of concerns** - Each file/pass has single job
6. **Progressive disclosure** - Core first, details on-demand
7. **Embed knowledge** - No external fetching
8. **Real examples** - Show working patterns

---

*"Phase 0 → Pass 1-6 → Phase N = right behavior emerges"*
*"One pass, one job, one confirmation = methodical excellence"*
