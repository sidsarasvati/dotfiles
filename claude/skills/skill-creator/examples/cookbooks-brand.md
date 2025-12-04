# Cookbooks Example: Brand Guidelines Skill

> **Source:** anthropics/claude-cookbooks custom_skills
> **Pattern:** Voice/style packaging for consistent communication
> **Lessons:** How to encode brand identity for auto-activation

## Overview

This skill from Anthropic's cookbooks shows how to package brand voice, tone, and style guidelines into an auto-activating skill. Demonstrates encoding company identity so Claude maintains consistency across all communications.

## Structure

```
custom_skills/brand_guidelines/
├── SKILL.md (core guidelines)
└── examples/
    └── communication-examples.md
```

**Two-file pattern** - Core guidelines + examples

## YAML Frontmatter

```yaml
---
name: brand-guidelines
description: Use when creating any customer-facing content, marketing materials, or communications. Auto-activates for writing tasks to ensure brand voice consistency. Provides tone, style, and messaging guidelines.
---
```

**Analysis:**
- **Trigger:** "creating customer-facing content, marketing materials, or communications"
- **Purpose:** "ensure brand voice consistency"
- **Value:** "tone, style, and messaging guidelines"
- **Scope:** All writing tasks (broad but bounded to company)

## Content Structure

The skill contains:

1. **Brand Voice Definition**
   - Core personality traits
   - Tone guidelines (formal/casual spectrum)
   - Vocabulary preferences (words to use/avoid)

2. **Writing Style Rules**
   - Sentence structure preferences
   - Formatting conventions
   - Grammar and punctuation standards

3. **Messaging Frameworks**
   - Key value propositions
   - Positioning statements
   - Common objection responses

4. **Do's and Don'ts**
   - ✅ Approved patterns
   - ❌ Anti-patterns to avoid
   - Examples of each

5. **Examples File**
   - Real communications showing guidelines
   - Before/after comparisons
   - Different content types (email, social, blog)

## Key Decisions

### Decision 1: Separate Examples File

**Why:** Core guidelines in SKILL.md, detailed examples in examples/

**Benefits:**
- SKILL.md stays scannable (quick reference)
- examples/ loaded when user needs inspiration
- Progressive disclosure (overview first, details on demand)

### Decision 2: Do's and Don'ts Format

**What:** Explicit approved vs anti-patterns

**Why:**
- Clear boundaries (what sounds like us vs doesn't)
- Easy to check (is this on-brand?)
- Reduces ambiguity

**Example structure:**
```markdown
✅ "We're excited to partner with you"
❌ "We would be honored if you'd consider"

✅ Direct, energetic, partnership-focused
❌ Formal, deferential, transactional
```

### Decision 3: Voice as Personality Traits

**What:** Brand voice defined as human traits

**Example:**
- Professional but approachable
- Confident but not arrogant
- Helpful but not pushy

**Why:** Easier to embody traits than follow rules

## What Works Well

✅ **Personality-based** - Voice as traits (not just rules)
✅ **Do's and Don'ts** - Explicit patterns to follow/avoid
✅ **Real examples** - Shows guidelines in action
✅ **Progressive disclosure** - Core in SKILL.md, examples separate
✅ **Broad activation** - All customer-facing writing triggers it

## What Could Be Enhanced

**Potential improvements:**
- Voice variations by channel (email vs social vs blog)
- Audience-specific guidelines (prospects vs customers vs press)
- Industry/competitor positioning
- A/B test winners (proven effective messaging)

**Trade-offs:**
- Comprehensive vs simple
- Prescriptive vs flexible
- One voice vs many variations

## Lessons for Skill Creation

### Lesson 1: Voice is Behavioral

**Pattern:**
```
Voice = Personality Traits + Examples + Do's/Don'ts
```

**Not:**
```
Voice = Grammar Rules + Style Guide + Dictionary
```

**Application:**
- Define voice as human characteristics
- Show examples of voice in action
- Provide clear boundaries (what's on/off brand)

### Lesson 2: Progressive Disclosure for Examples

**Pattern:**
```
SKILL.md = Core guidelines (always loaded)
examples/ = Detailed examples (loaded when needed)
```

**Benefits:**
- Fast activation (guidelines only)
- Deep inspiration available (examples on demand)
- Scales well (can add many examples without bloat)

### Lesson 3: Broad Auto-Activation

**Pattern:**
```
description: Use when creating any [category] to ensure [quality]. Auto-activates for [broad task type].
```

**Application:**
- Skill loads for all brand communications
- Not triggered per message (too narrow)
- Ensures consistency automatically

## Applying to Your Skills

**Use this pattern when:**
- Encoding voice/style (brand, personal, domain-specific)
- Consistency across communications needed
- Examples help clarify guidelines
- Broad auto-activation desired (all writing, not specific tasks)

**Structure to adopt:**
```
your-voice-skill/
├── SKILL.md
│   ├── Voice definition (personality)
│   ├── Style rules (how to write)
│   ├── Do's and Don'ts (boundaries)
│   └── Quick examples (2-3 inline)
└── examples/
    └── detailed-examples.md (many examples, loaded on-demand)
```

**Size target:**
- SKILL.md: 15-25KB
- examples/: 10-20KB
- Total: 25-45KB

## Comparison to Our Skills

**Similar to:**
- `business-writing` - Sid's voice for B2B communication
- Could create: `sid-voice` for all Sid communications

**Pattern application:**

**business-writing (current):**
```yaml
description: Use when writing B2B emails, proposals, or business communications in Sid's voice.
```

**Potential sid-voice (broader):**
```yaml
description: Use when writing any communication as Sid. Auto-activates for emails, messages, posts, or any content in Sid's voice. Ensures authentic personality across all channels.
```

**Trade-off:**
- Narrow (business-writing): Precise trigger, B2B only
- Broad (sid-voice): Always-on, all writing
- Both valid: Depends on use case

## Real-World Application

**This pattern used in:**
- `business-writing` skill (Sid's B2B voice)
- Could extend to:
  - `coaching-voice` (WIN CLUB communications)
  - `technical-writing` (documentation style)
  - `social-media-voice` (X/LinkedIn tone)

**Key insight:**
> Voice skills benefit from broad auto-activation
> User shouldn't manually invoke "use my voice" - should just happen

---

*"Voice = personality traits + examples + boundaries"*
*"Progressive disclosure = guidelines always, examples on-demand"*
