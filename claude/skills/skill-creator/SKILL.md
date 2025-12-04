---
name: skill-creator
description: Use when creating, designing, or building Claude Code skills. Auto-activates for "create a skill", "build a skill", "skill for X", or "make this into a skill". Provides complete 6-step creation protocol, embedded Anthropic documentation, working examples, and templates. Never need to fetch external docs again.
---

# Skill Creator: Meta-Skill for Building Claude Code Skills

> **"Skills = Crystallized expertise that auto-activates"**
> **"Package domain knowledge once, use everywhere"**

## Core Principle

Skills are behavioral strata for AI - not instructions to follow, but expertise that auto-activates when needed. Progressive disclosure prevents context bloat while enabling 20+ skills without overwhelming.

## When to Use This Skill

- Creating new Claude Code skills
- Converting CLAUDE.md sections to focused skills
- Extracting project expertise into portable skills
- Understanding Skills architecture
- Writing auto-activation descriptions
- Debugging skill activation issues
- Following best practices for skill design

## The 6-Step Creation Protocol

### Phase 0: DETECTION (What Kind of Skill?)

**Questions to ask:**
1. What domain expertise does this package?
2. When should it auto-activate vs user-invoke?
3. Is this simple (one pattern) or complex (multi-pass protocol)?
4. Will it execute tools/code or just provide guidance?

**Output:** "Building [type] skill: [name]"

**Skill types:**
- **Simple skills**: Single pattern, straightforward guidance (see `examples/templates/simple-skill.md`)
- **Protocol skills**: Multi-pass methodology like garden-protocol (see `examples/templates/protocol-skill.md`)
- **Code-execution skills**: Run bash/tools during activation (see `examples/templates/code-execution-skill.md`)

### Pass 1: Architecture Design

**Define structure:**
```
skill-name/
├── SKILL.md (entry point - YAML + core protocol)
├── references/ (deep docs - progressive disclosure)
│   ├── detailed-guide.md
│   └── advanced-patterns.md
└── examples/ (working examples + templates)
    ├── example-1.md
    └── example-2.md
```

**Key decisions:**
- What goes in SKILL.md (always loaded)?
- What goes in references/ (loaded on-demand)?
- What examples demonstrate the pattern?
- Size target? (aim 20-80KB)

**Output:** Architecture documented

### Pass 2: Write YAML Frontmatter

**Required fields:**
- `name:` (lowercase-with-hyphens, matches directory name)
- `description:` (triggers auto-activation - SEE `references/description-writing.md`)

**Description guidelines:**
- Start with "Use when..." or "Provides..."
- Include trigger phrases ("create a skill", "skill for X")
- Explain what it does (not how)
- 1-3 sentences, clear and specific

**Example:**
```yaml
---
name: skill-creator
description: Use when creating, designing, or building Claude Code skills. Auto-activates for "create a skill", "build a skill", "skill for X", or "make this into a skill". Provides complete 6-step creation protocol, embedded Anthropic documentation, working examples, and templates.
---
```

**Output:** YAML written and validated

### Pass 3: Core Instructions (SKILL.md body)

**Structure:**
1. **Title + Quote**: Capture essence
2. **Core Principle**: What makes this skill valuable
3. **When to Use**: Explicit trigger scenarios
4. **Main Protocol**: Step-by-step methodology
5. **Critical Patterns**: Do's and don'ts
6. **Success Metrics**: How to verify it worked
7. **Progressive Disclosure Pointers**: Link to references/

**Writing style:**
- Protocol thinking (behavioral strata, not instructions)
- Separation of concerns (each section one job)
- Sid's voice throughout
- Clear examples over abstract theory
- Scannable structure (headers, bullets, code blocks)

**Output:** SKILL.md core written

### Pass 4: Supporting References

**Create references/ files:**
- Embed external documentation (Anthropic, cookbooks)
- Detailed guides for complex topics
- Advanced patterns and edge cases
- Keep focused (one concern per file)

**Progressive disclosure:**
- SKILL.md = quick start + overview
- references/ = deep dive when needed
- User loads what they need, when they need it

**Output:** Reference files created

### Pass 5: Examples & Templates

**Working examples:**
- Show real usage from existing skills
- Demonstrate different patterns
- Explain why they work

**Templates:**
- Provide starting points for common patterns
- Include placeholder text
- Show structure clearly

**Output:** Examples and templates ready

### Pass 6: Testing & Verification

**Test auto-activation:**
1. Use trigger phrases in conversation
2. Verify skill loads when expected
3. Check description clarity
4. Refine if needed

**Verify structure:**
- [ ] YAML valid (name + description)
- [ ] SKILL.md complete (all sections)
- [ ] References organized (progressive disclosure)
- [ ] Examples demonstrate patterns
- [ ] Size reasonable (target met)
- [ ] Voice consistent (Sid's style)

**Output:** Skill tested and working

### Phase N: Document & Deploy

**Final steps:**
1. Test with real usage scenario
2. Document learnings in MEMORY.md
3. Add to skills inventory
4. Update related skills if needed

**Success:** Skill auto-activates correctly, provides value immediately

## When NOT to Create a Skill

**Skip skills for:**
- One-time operations (just execute)
- Rarely-used knowledge (keep in CLAUDE.md)
- User-triggered commands (use /commands instead)
- Overly broad topics (split into focused skills)

**The test:**
> "Will this auto-activate 3+ times per week?"

If no, reconsider.

## Skill Types Explained

### Simple Skills
- Single pattern or framework
- Straightforward guidance
- Example: `business-writing` (B2B email templates)
- Template: `examples/templates/simple-skill.md`

### Protocol Skills
- Multi-pass methodology
- Separation of concerns per pass
- Example: `garden-protocol` (7-pass journal tending)
- Template: `examples/templates/protocol-skill.md`

### Code-Execution Skills
- Run tools/bash during activation
- Gather data or verify state
- Example: Skills that check calendars, read files, verify git status
- Template: `examples/templates/code-execution-skill.md`

## Progressive Disclosure Architecture

**How it works:**
```
User: "Help me create a skill"
→ skill-creator auto-activates (SKILL.md loaded)
→ Provides 6-step protocol overview
→ Points to references/ for details
→ User says "explain YAML requirements"
→ Load references/yaml-guide.md
→ Detailed YAML guidance provided
```

**Benefits:**
- Fast activation (minimal context load)
- Deep expertise available on-demand
- No context bloat
- Scales to 20+ skills

## Key Principles

1. **Auto-activation precision**: Description determines when skill loads
2. **Progressive disclosure**: Core in SKILL.md, details in references/
3. **Protocol thinking**: Behavioral strata, not instruction lists
4. **Separation of concerns**: Each file, pass, section has one job
5. **Sid's voice**: Natural, clear, example-driven
6. **Size discipline**: 20-80KB sweet spot
7. **Testing required**: Verify auto-activation works
8. **Embed knowledge**: No external doc fetching needed

## Critical Patterns

### The AI Failure Pattern (NEVER DO THIS)

❌ "I'll create YAML and instructions together efficiently"
❌ "Let me write all files at once to save time"
❌ "I'll skip Phase 0 since I know what to build"

### The Correct Pattern (DO THIS)

✅ Phase 0: Detect skill type first
✅ One pass at a time (Pass 1 → 2 → 3 → 4 → 5 → 6)
✅ Test activation before declaring done
✅ Verify structure matches protocol

## Success Metrics

- [ ] Auto-activates on trigger phrases
- [ ] Provides immediate value (not just theory)
- [ ] Size within target (20-80KB)
- [ ] Progressive disclosure works
- [ ] Examples demonstrate patterns
- [ ] Voice consistent throughout
- [ ] Testing confirmed working
- [ ] No external docs needed

## Deep Dive Resources

**For complete details, see:**
- `references/anthropic-docs.md` - Official Skills documentation (embedded)
- `references/creation-protocol.md` - Detailed 6-step methodology
- `references/yaml-guide.md` - YAML requirements and patterns
- `references/description-writing.md` - Auto-activation trigger design
- `examples/cookbooks-financial.md` - Anthropic cookbook example
- `examples/cookbooks-brand.md` - Another cookbook pattern
- `examples/garden-protocol-case.md` - Working protocol skill analysis
- `examples/templates/` - Starting points for common patterns

## Quick Reference

**Creating simple skill:**
1. Phase 0: Identify as simple skill
2. Pass 1: Plan structure (SKILL.md + maybe 1-2 references)
3. Pass 2: Write YAML with clear description
4. Pass 3: Core instructions in SKILL.md
5. Pass 4: Add reference if needed
6. Pass 5: Include 1-2 examples
7. Pass 6: Test auto-activation

**Creating protocol skill:**
1. Phase 0: Identify multi-pass pattern
2. Pass 1: Plan passes (Phase 0 → Pass 1-N → Phase N)
3. Pass 2: Write YAML (mention "protocol" in description)
4. Pass 3: Document each pass with one job
5. Pass 4: Detailed methodology in references/
6. Pass 5: Show working example (like garden-protocol)
7. Pass 6: Test full protocol execution

**Execution time:**
- Simple skill: 15-20 minutes
- Protocol skill: 30-45 minutes
- Complex skill: 60+ minutes

**The meta-pattern:**
> Using skills to create skills = Meta-bootstrapping

---

*"Never fetch Anthropic docs again - all knowledge embedded"*
*"Skills = AI cognitive architecture that evolves itself"*
