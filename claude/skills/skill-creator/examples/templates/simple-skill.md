# Template: Simple Skill

> **Use when:** Single pattern, straightforward guidance, domain expertise
> **Size target:** 20-40KB
> **Structure:** Single SKILL.md file (all content in one place)

## When to Use This Template

**Choose simple skill template for:**
- Domain expertise to package (financial analysis, writing style, frameworks)
- Templates and guidelines (B2B emails, coaching patterns)
- Voice/style encoding (brand voice, personal communication style)
- Reference knowledge (metrics, formulas, methodologies)

**Don't use for:**
- Multi-pass systematic processes (use protocol-skill template)
- Skills requiring tool execution (use code-execution-skill template)
- Very large knowledge bases (consider progressive disclosure)

## Directory Structure

```
skill-name/
└── SKILL.md
```

## Template: SKILL.md

```markdown
---
name: skill-name
description: Use when [SPECIFIC SCENARIO] to [PURPOSE]. Auto-activates for [TRIGGER 1], [TRIGGER 2], or [TRIGGER 3]. Provides [SPECIFIC VALUE].
---

# Skill Name: One-Line Purpose

> **"Quote capturing essence of skill"**
> **"Second quote if helpful"**

## Core Principle

[2-3 sentences explaining what makes this skill valuable and when to use it]

## When to Use This Skill

- Specific scenario 1 (be precise)
- Specific scenario 2
- Specific scenario 3
- **When NOT to use:** [Anti-pattern or out-of-scope scenario]

## [Main Content Section]

[Your core expertise, frameworks, templates, or guidelines]

### Subsection 1

[Content organized clearly]

### Subsection 2

[Continue pattern]

## Critical Patterns

### The AI Failure Pattern (NEVER DO THIS)

❌ Common mistake 1 (what NOT to do)
❌ Common mistake 2
❌ Common mistake 3

### The Correct Pattern (DO THIS)

✅ Right approach 1 (what TO do)
✅ Right approach 2
✅ Right approach 3

## Examples

### Example 1: [Scenario]

[Show the skill in action - real example]

**Why this works:**
- Reason 1
- Reason 2

### Example 2: [Scenario]

[Another example showing different aspect]

**Key insight:**
[What makes this effective]

## Quick Reference

[Optional: Condensed version for fast lookup]

**Common pattern:**
```
[Template or formula users can copy]
```

**Key points:**
- Bullet 1
- Bullet 2
- Bullet 3

## Success Metrics

- [ ] Checklist item 1 (how to verify success)
- [ ] Checklist item 2
- [ ] Checklist item 3

**The test:** [One clear question to verify quality]

---

*Closing quote summarizing skill essence*
*Second closing quote if helpful*
```

## Customization Guide

### Step 1: Replace Placeholders

**YAML frontmatter:**
- `skill-name` → Your actual skill name (lowercase-with-hyphens)
- `[SPECIFIC SCENARIO]` → When this skill should activate
- `[PURPOSE]` → What user is trying to achieve
- `[TRIGGER 1/2/3]` → Actual phrases users would say
- `[SPECIFIC VALUE]` → Concrete deliverables (templates, frameworks, etc.)

**Content sections:**
- Replace `[Main Content Section]` with your domain expertise
- Customize subsections based on your structure
- Keep examples relevant to your domain

### Step 2: Structure Your Expertise

**Common patterns:**

**For templates/guidelines:**
```markdown
## Templates

### Template 1: [Use Case]
[Template content]

### Template 2: [Use Case]
[Template content]

## Usage Guidelines
[When/how to use templates]
```

**For domain knowledge:**
```markdown
## Core Concepts

### Concept 1
[Explanation + examples]

### Concept 2
[Explanation + examples]

## Methodology
[How to apply concepts]
```

**For voice/style:**
```markdown
## Voice Definition
[Personality traits, tone]

## Style Rules
[Writing patterns]

## Do's and Don'ts
✅ / ❌ [Examples]
```

### Step 3: Add Real Examples

**Good examples:**
- From actual usage (not theoretical)
- Show before/after if applicable
- Explain why it works
- Cover common scenarios

**Example structure:**
```markdown
### Example: [Scenario Name]

**Context:** [When this applies]

**Approach:**
[What to do]

**Result:**
[Expected outcome]

**Why this works:**
- Reason 1
- Reason 2
```

### Step 4: Define Success Metrics

**Make success measurable:**
- Not: "It's good"
- Yes: "Email gets response within 24 hours"
- Yes: "Reader understands main point in 10 seconds"
- Yes: "Follows brand voice guidelines"

**Checklist format:**
```markdown
## Success Metrics

- [ ] Specific measurable outcome 1
- [ ] Specific measurable outcome 2
- [ ] Verification method

**The test:** [One clear question to ask]
```

## Size Management

**Target: 20-40KB**

**If too small (<15KB):**
- Add more examples
- Expand methodology section
- Include troubleshooting
- Add quick reference

**If too large (>50KB):**
- Consider splitting into skill + references/
- Move detailed examples to examples/ folder
- Keep core in SKILL.md, details in supporting files
- Use progressive disclosure pattern

## Testing Your Simple Skill

### Test Auto-Activation

1. **Write 5 realistic trigger phrases**
   - Phrase 1: [Direct request]
   - Phrase 2: [Implicit task]
   - Phrase 3: [Related question]
   - Phrase 4: [Variation]
   - Phrase 5: [Edge case]

2. **Test each phrase**
   - Does skill activate?
   - Is activation appropriate?
   - Any false positives?

3. **Refine description** based on results

### Test Usability

- [ ] Can user execute guidance immediately?
- [ ] Examples clear and relevant?
- [ ] Success metrics measurable?
- [ ] No confusing jargon or abstractions?
- [ ] Voice consistent throughout?

## Examples of This Pattern

**From cookbooks:**
- `financial-analysis` - Domain expertise (financial metrics + analysis)
- `brand-guidelines` - Voice/style (brand voice + examples)

**From Sid's skills:**
- `business-writing` - Templates (B2B emails in Sid's voice)
- `coaching-frameworks` - Pattern extraction (WIN CLUB transcript analysis)

## Common Variations

### Variation 1: Template-Heavy

**Structure:**
```markdown
## Templates
[Multiple templates with examples]

## When to Use Which Template
[Decision guide]

## Customization Guidelines
[How to adapt templates]
```

**Example:** business-writing skill

### Variation 2: Knowledge-Heavy

**Structure:**
```markdown
## Core Concepts
[Domain knowledge]

## Methodology
[How to apply]

## Reference
[Formulas, metrics, frameworks]
```

**Example:** financial-analysis skill

### Variation 3: Voice-Heavy

**Structure:**
```markdown
## Voice Definition
[Personality traits]

## Style Rules
[Writing guidelines]

## Do's and Don'ts
[Examples]

## Communication Examples
[Real usage]
```

**Example:** brand-guidelines skill

## Next Steps After Creating

1. **Test thoroughly** (5+ trigger phrases)
2. **Use in production** (real task, not theoretical)
3. **Document learnings** (what works, what doesn't)
4. **Iterate based on usage** (refine description, add examples)
5. **Update MEMORY.md** (capture insights from creation)

---

*"Simple skill = domain expertise + clear triggers + real examples"*
*"Single file = fast activation, no progressive disclosure complexity"*
*"20-40KB = sweet spot for focused domain knowledge"*
