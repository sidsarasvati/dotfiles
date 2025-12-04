# Anthropic Cookbooks: Custom Skills Reference

> **Source:** https://github.com/anthropics/claude-cookbooks/tree/main/custom_skills
> **Purpose:** Overview of Anthropic's official skill examples
> **Value:** Proven patterns from Anthropic's own implementation

## Overview

Anthropic's claude-cookbooks repository contains custom skill examples showing official patterns and best practices. These represent Anthropic's recommended approaches to skill creation.

## Repository Structure

```
claude-cookbooks/
└── custom_skills/
    ├── financial_analysis/
    │   └── SKILL.md
    ├── brand_guidelines/
    │   ├── SKILL.md
    │   └── examples/
    │       └── communication-examples.md
    └── financial_models/
        └── SKILL.md
```

## Skills Included

### 1. Financial Analysis

**Purpose:** Package financial analysis expertise

**Pattern:** Single-file domain expertise skill

**Key features:**
- Financial statement analysis frameworks
- Ratio calculations (liquidity, profitability, leverage)
- Analysis methodology (horizontal, vertical, comparative)
- Reporting templates

**Size:** ~30KB

**What we learned:**
- Domain expertise can be comprehensive yet concise
- Formulas/frameworks embedded (no external deps)
- Template-driven output (consistent structure)
- Clear activation triggers (analyzing, calculating, insights)

**See:** `examples/cookbooks-financial.md` for detailed analysis

### 2. Brand Guidelines

**Purpose:** Encode brand voice and style

**Pattern:** Core guidelines + examples (progressive disclosure)

**Key features:**
- Voice definition (personality traits)
- Style rules (writing patterns)
- Do's and Don'ts (explicit boundaries)
- Communication examples (real usage)

**Size:** ~35KB (SKILL.md ~20KB + examples ~15KB)

**What we learned:**
- Voice as behavioral traits (not just rules)
- Progressive disclosure works (guidelines vs examples)
- Broad auto-activation (all customer-facing writing)
- Examples separate from core (loaded on-demand)

**See:** `examples/cookbooks-brand.md` for detailed analysis

### 3. Financial Models

**Purpose:** Financial modeling and forecasting

**Pattern:** Similar to financial analysis (domain expertise)

**Key features:**
- Model structures (DCF, LBO, M&A)
- Forecasting methodologies
- Valuation frameworks
- Sensitivity analysis

**Size:** ~35KB

**What we learned:**
- Complex domains can still fit in single skill
- Methodology documentation (how to build models)
- Framework catalog (different model types)
- Domain-specific but broadly applicable

## Common Patterns Across Cookbooks

### Pattern 1: Domain Expertise Packaging

**All three skills:**
- Package complete domain knowledge
- Self-contained (no external references)
- Embed formulas/frameworks
- Provide templates/examples

**Structure:**
```
Domain Knowledge = Concepts + Methodology + Templates + Examples
```

**Lesson:** Skills can be comprehensive without bloat

### Pattern 2: Clear Auto-Activation

**Description pattern:**
```yaml
description: Use when [task type] to [purpose]. Auto-activates for [domain keywords] and [specific activities].
```

**Examples:**
- Financial analysis: "analyzing financial statements, calculating metrics"
- Brand guidelines: "creating customer-facing content, marketing materials"
- Financial models: "building financial models, forecasting, valuation"

**Lesson:** Specific tasks + domain keywords = precise activation

### Pattern 3: Progressive Disclosure (Where Needed)

**Financial analysis:** Single file (cohesive domain)
**Brand guidelines:** Core + examples (guidelines always, inspiration on-demand)
**Financial models:** Single file (methodology-focused)

**Decision criteria:**
- Cohesive domain → Single file
- Core + optional extras → Progressive disclosure
- Reference material → Separate files

**Lesson:** Use progressive disclosure strategically, not universally

### Pattern 4: Template-Driven Output

**All skills provide:**
- Expected output format
- Structure for deliverables
- Examples of final product

**Why:**
- Consistency in results
- User knows what to expect
- Claude knows what to generate

**Lesson:** Show output format, not just input guidance

## Differences from Sid's Skills

### Cookbooks vs Sid's Approaches

**Cookbooks focus:**
- Domain expertise (financial, brand, modeling)
- Professional/corporate use cases
- Template-driven outputs
- Comprehensive single domains

**Sid's skills focus:**
- Personal workflows (journal tending, B2B emails)
- Protocol thinking (multi-pass methodology)
- Voice preservation (Sid's authentic style)
- Behavioral strata (right behavior emerges)

**Both valid:** Different contexts, different needs

### Protocol Pattern Not in Cookbooks

**Sid's innovation:**
- garden-protocol (7-pass systematic process)
- Phase 0 → Pass 1-N → Phase N structure
- Separation of concerns per pass
- Explicit prohibitions (DO NOT combine)

**Why different:**
- Cookbooks show domain knowledge
- Sid shows systematic processes
- Both patterns valuable

**Lesson:** Multiple valid skill patterns exist

### Voice as Core Feature

**Sid's skills:**
- business-writing (Sid's voice for B2B)
- coaching-frameworks (WIN CLUB patterns)
- Personal voice maintained throughout

**Cookbooks:**
- Brand guidelines (company voice)
- Professional/corporate tone
- Generic expertise (not personal)

**Difference:**
- Sid's skills deeply personal
- Cookbooks more universal
- Both approaches work

**Lesson:** Skills can encode personal or universal expertise

## Applying Cookbook Patterns

### When to Use Cookbook Patterns

**Financial analysis pattern (single-file expertise):**
- Clear domain to package
- Related concepts
- Formulas/frameworks to embed
- Professional deliverables

**Brand guidelines pattern (core + examples):**
- Voice/style definition
- Do's and Don'ts needed
- Examples help clarify
- Broad auto-activation

**Financial models pattern (methodology-heavy):**
- Process documentation
- Multiple frameworks
- How-to guidance
- Complex but cohesive domain

### When to Use Sid's Patterns

**Protocol pattern (garden-protocol):**
- Multi-step systematic process
- Separation of concerns critical
- Repeatable methodology
- Order-dependent operations

**Voice pattern (business-writing):**
- Personal communication style
- Authentic voice preservation
- Specific use cases (B2B, coaching)
- Natural language, not templates

**Platform pattern (claude-code-platform):**
- Comprehensive knowledge base
- Multiple related topics
- Progressive disclosure needed
- Reference material

## Key Takeaways

### From Cookbooks

1. **Domain expertise packages well** - 30-35KB for complete domains
2. **Progressive disclosure optional** - Use when needed, not always
3. **Templates drive consistency** - Show expected output format
4. **Auto-activation can be broad** - "All customer-facing content" works
5. **Single file sufficient** - For cohesive domains

### From Sid's Skills

1. **Protocol thinking powerful** - Behavioral strata vs instructions
2. **Multi-pass separation works** - Prevents AI failure modes
3. **Personal voice valuable** - Authentic style preservation
4. **Phase 0 and N critical** - Bookend verification prevents errors
5. **Production-proven patterns** - Daily usage validates design

### Combined Insights

**Best of both:**
- Cookbooks: Domain expertise packaging, template-driven
- Sid: Protocol thinking, voice preservation, separation of concerns
- Together: Complete toolkit for skill creation

**Choose pattern based on:**
- Skill purpose (domain knowledge vs systematic process)
- Complexity (single concept vs multi-pass)
- Usage (broad activation vs specific scenarios)
- Style (universal vs personal)

## Resources

**Anthropic Cookbooks:**
- Repository: https://github.com/anthropics/claude-cookbooks
- Custom skills: https://github.com/anthropics/claude-cookbooks/tree/main/custom_skills

**Our examples:**
- `examples/cookbooks-financial.md` - Financial analysis detailed breakdown
- `examples/cookbooks-brand.md` - Brand guidelines detailed breakdown
- `examples/garden-protocol-case.md` - Sid's protocol pattern analysis

**Templates:**
- `examples/templates/simple-skill.md` - For domain expertise (cookbook pattern)
- `examples/templates/protocol-skill.md` - For multi-pass processes (Sid pattern)
- `examples/templates/code-execution-skill.md` - For verification skills

---

*"Anthropic cookbooks + Sid's patterns = comprehensive skill creation toolkit"*
*"Domain expertise + protocol thinking = powerful skills emerge"*
