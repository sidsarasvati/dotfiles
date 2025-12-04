# YAML Frontmatter Guide for Skills

> **"Description determines when skill activates"**
> **"Required fields: name + description"**

## Overview

Every SKILL.md must start with YAML frontmatter enclosed in triple dashes. This metadata tells Claude when and how to load the skill.

## Required Format

```yaml
---
name: skill-name
description: When this skill activates and what it provides
---
```

**Critical:**
- Three dashes `---` before and after
- `name:` field (lowercase-with-hyphens)
- `description:` field (triggers auto-activation)
- No tabs (use spaces for indentation)
- Colon followed by space

## Required Fields

### name

**Purpose:** Identifies the skill uniquely

**Requirements:**
- Lowercase letters only
- Hyphens for word separation (not underscores)
- Matches folder name exactly
- Concise and descriptive
- No spaces, special characters, or numbers (unless needed)

**Good names:**
```yaml
name: garden-protocol
name: business-writing
name: skill-creator
name: claude-code-platform
```

**Bad names:**
```yaml
name: Garden Protocol (spaces not allowed)
name: garden_protocol (underscores discouraged)
name: gp (too cryptic)
name: the-protocol-for-tending-journals (too verbose)
```

### description

**Purpose:** Triggers auto-activation when context matches

**Requirements:**
- 1-3 sentences ideal
- Clear about when to activate
- Includes trigger terms users would say
- Explains value provided
- Not too broad (loads unnecessarily) or narrow (misses cases)

**Structure options:**

**Option 1: Use when...**
```yaml
description: Use when [scenario] to [purpose]. Auto-activates for [triggers]. Provides [value].
```

**Option 2: Provides...**
```yaml
description: Provides [value] for [use cases]. Auto-activates when [conditions].
```

**Examples:**

```yaml
description: Use when closing or reviewing daily journal entries in Logseq to apply the 7-pass garden tending protocol. Ensures journals are zen gardens (curiosity-driven, scannable) not databases (verbose, complete). Auto-activates for journal closing, pruning, or quality checks.
```

```yaml
description: Provides comprehensive knowledge about Claude Code platform features, tools, MCP integration, and advanced capabilities. Auto-activates when discussing or using Claude Code functionality.
```

```yaml
description: Use when creating, designing, or building Claude Code skills. Auto-activates for "create a skill", "build a skill", "skill for X", or "make this into a skill". Provides complete 6-step creation protocol, embedded Anthropic documentation, working examples, and templates.
```

## Optional Fields

Currently, only `name` and `description` are supported. Future versions may add:
- `version:` (skill version tracking)
- `dependencies:` (other skills required)
- `tags:` (categorization)
- `priority:` (loading order)

For now, stick to required fields only.

## Character Limits

**name:**
- Recommended: 15-30 characters
- Maximum: No hard limit, but keep concise
- Ideal: 2-3 words joined by hyphens

**description:**
- Recommended: 100-300 characters
- Maximum: No hard limit, but keep focused
- Ideal: 1-3 clear sentences

**Why limits matter:**
- Shorter = faster to process
- Focused = better auto-activation
- Concise = easier to maintain

## Validation

### YAML Syntax Check

**Valid:**
```yaml
---
name: my-skill
description: Clear description here.
---
```

**Invalid (missing closing dashes):**
```yaml
---
name: my-skill
description: Clear description here.

# Skill Content
```

**Invalid (tabs instead of spaces):**
```yaml
---
name:	my-skill
description:	Clear description here.
---
```

**Invalid (no space after colon):**
```yaml
---
name:my-skill
description:Clear description here.
---
```

### Name Validation

**Check:**
- [ ] All lowercase
- [ ] Hyphens for spaces (not underscores)
- [ ] Matches folder name exactly
- [ ] Descriptive but concise
- [ ] No special characters

### Description Validation

**Check:**
- [ ] Clear about when to activate
- [ ] Includes realistic trigger terms
- [ ] Explains value provided
- [ ] 1-3 sentences
- [ ] Not too broad or narrow

## Common Mistakes

### Mistake 1: Wrong Casing

❌ `name: Garden-Protocol` (capital letters)
❌ `name: Garden Protocol` (spaces)

✅ `name: garden-protocol` (lowercase with hyphens)

### Mistake 2: Vague Description

❌ `description: For journal work` (too vague)
❌ `description: Use this when you need help with journals` (no triggers)

✅ `description: Use when closing or reviewing daily journal entries in Logseq to apply the 7-pass garden tending protocol.`

### Mistake 3: Missing Dashes

❌ 
```yaml
name: my-skill
description: My description
```

✅ 
```yaml
---
name: my-skill
description: My description
---
```

### Mistake 4: Name/Folder Mismatch

❌ Folder: `garden-protocol/`, YAML: `name: journal-tending`

✅ Folder: `garden-protocol/`, YAML: `name: garden-protocol`

### Mistake 5: Multi-line Description (Improper Format)

❌ 
```yaml
description: 
  This is a long description
  that spans multiple lines
  without proper formatting
```

✅ 
```yaml
description: This is a longer description that flows naturally as one paragraph, even if it visually wraps in the file. Auto-activates for specific triggers. Provides clear value.
```

**Note:** If multi-line needed, use proper YAML syntax:
```yaml
description: >
  This is a longer description
  that properly spans multiple lines
  using YAML block scalar notation.
```

## Trigger Term Strategy

### Good Trigger Terms

**Action verbs:**
- "create", "build", "design", "write"
- "close", "review", "analyze", "refactor"
- "plan", "organize", "structure"

**Domain keywords:**
- "Logseq", "journal", "skill", "protocol"
- "Claude Code", "MCP", "tool", "bash"
- "B2B", "email", "coaching", "framework"

**Task indicators:**
- "closing daily journal"
- "creating a skill"
- "writing to prospect"
- "reviewing patterns"

### Bad Trigger Terms

**Too generic:**
- "work", "help", "task", "thing"
- "use", "do", "make", "get"

**Too narrow:**
- "exactly close journal on Tuesday"
- "only when user says 'skill creator please help'"

**Internal jargon:**
- "invoke multiplex protocol"
- "run pass 7 verification"

Use terms your users would naturally say.

## Testing Your YAML

### Manual Testing Steps

1. **Save file** with YAML frontmatter
2. **Open new conversation** (fresh context)
3. **Use trigger phrase** from description
4. **Verify skill loads** (check context usage)
5. **Refine description** if needed

### Test Scenarios

**Should activate:**
- Direct request with trigger terms
- Implicit task matching scenario
- Related questions about domain

**Should NOT activate:**
- Unrelated work
- Different domain
- Generic conversation

### Iteration Process

1. **Write initial description** (best guess)
2. **Test with 3-5 phrases** (realistic usage)
3. **Note if it activates correctly** (yes/no for each)
4. **Refine description** (add/remove trigger terms)
5. **Test again** (verify improvements)
6. **Repeat until working** (usually 1-2 iterations)

## Examples from Working Skills

### garden-protocol

```yaml
---
name: garden-protocol
description: Use when closing or reviewing daily journal entries in Logseq to apply the 7-pass garden tending protocol. Ensures journals are zen gardens (curiosity-driven, scannable) not databases (verbose, complete). Auto-activates for journal closing, pruning, or quality checks.
---
```

**Why it works:**
- Specific scenario ("closing or reviewing daily journal entries")
- Clear tool context ("in Logseq")
- Explicit triggers ("journal closing, pruning, quality checks")
- Value statement ("zen gardens not databases")

### business-writing

```yaml
---
name: business-writing
description: Use when writing B2B emails, proposals, or business communications in Sid's voice. Auto-activates for "write email", "draft message", or business correspondence tasks. Provides templates and voice guidelines.
---
```

**Why it works:**
- Clear use case ("B2B emails, proposals, business communications")
- Specific voice ("in Sid's voice")
- Realistic triggers ("write email", "draft message")
- Value ("templates and voice guidelines")

### claude-code-platform

```yaml
---
name: claude-code-platform
description: Provides comprehensive knowledge about Claude Code platform features, tools, MCP integration, and advanced capabilities. Auto-activates when discussing or using Claude Code functionality.
---
```

**Why it works:**
- Broad but bounded ("Claude Code platform")
- Lists key topics ("tools, MCP integration, advanced capabilities")
- Natural activation ("discussing or using Claude Code")
- Platform-specific (not too generic)

### skill-creator

```yaml
---
name: skill-creator
description: Use when creating, designing, or building Claude Code skills. Auto-activates for "create a skill", "build a skill", "skill for X", or "make this into a skill". Provides complete 6-step creation protocol, embedded Anthropic documentation, working examples, and templates.
---
```

**Why it works:**
- Specific task ("creating, designing, or building... skills")
- Explicit trigger phrases (quoted examples)
- Clear value ("6-step creation protocol, embedded docs, examples, templates")
- Comprehensive but focused

## Best Practices

1. **Be specific about scenarios** - Not "for Logseq" but "when closing daily journal entries in Logseq"
2. **Include realistic triggers** - Phrases users actually say
3. **Explain value concisely** - What does this skill provide?
4. **Test with real phrases** - 3-5 trigger phrase tests minimum
5. **Iterate based on testing** - Refine description until activation correct
6. **Keep focused** - Better narrow and accurate than broad and noisy
7. **Match folder name** - name field must equal folder name exactly
8. **Use proper YAML** - Three dashes, spaces not tabs, colon-space format

## Troubleshooting

### Skill doesn't activate

**Check:**
- Description includes trigger terms user said
- Scenario matches actual task context
- Not too narrow (only one exact phrase)

**Fix:**
- Add more trigger terms
- Broaden scenario slightly
- Include synonyms of key terms

### Wrong skill activates

**Check:**
- Description not too generic
- Trigger terms not overlapping with other skills
- Scenario specific enough

**Fix:**
- Narrow description scope
- Remove generic trigger terms
- Add distinguishing context

### Skill activates too often

**Check:**
- Trigger terms too generic
- Scenario too broad
- Missing context qualifiers

**Fix:**
- Be more specific about when to activate
- Add context ("in Logseq", "for Claude Code")
- Remove overly generic terms

## Quick Reference

**Minimal valid YAML:**
```yaml
---
name: my-skill
description: Use when [scenario]. Provides [value].
---
```

**Full example:**
```yaml
---
name: protocol-name
description: Use when [specific scenario] to [purpose]. Auto-activates for [trigger 1], [trigger 2], or [trigger 3]. Provides [specific value including key features].
---
```

**Validation checklist:**
- [ ] Three dashes before and after
- [ ] name: lowercase-with-hyphens
- [ ] name matches folder exactly
- [ ] description: 1-3 clear sentences
- [ ] Includes specific trigger terms
- [ ] Explains value provided
- [ ] Tested with 3+ trigger phrases
- [ ] Activates correctly on tests

---

*"Description determines activation - make it specific and testable"*
*"name matches folder, triggers match usage, value matches need"*
