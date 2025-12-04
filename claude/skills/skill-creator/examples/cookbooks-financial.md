# Cookbooks Example: Financial Analysis Skill

> **Source:** anthropics/claude-cookbooks custom_skills
> **Pattern:** Domain expertise packaging with auto-activation
> **Lessons:** How to structure knowledge for Claude's financial analysis capabilities

## Overview

This skill from Anthropic's cookbooks demonstrates packaging financial analysis expertise into an auto-activating skill. Shows how to embed domain knowledge (financial metrics, analysis frameworks) for instant access.

## Structure

```
custom_skills/financial_analysis/
└── SKILL.md
```

**Single-file pattern** - All content in SKILL.md (simple skill)

## YAML Frontmatter

```yaml
---
name: financial-analysis
description: Use when analyzing financial statements, calculating financial metrics, or providing investment insights. Auto-activates for tasks involving balance sheets, income statements, cash flow analysis, and financial ratio calculations.
---
```

**Analysis:**
- **Trigger terms:** analyzing financial statements, calculating financial metrics, providing investment insights
- **Domain keywords:** balance sheets, income statements, cash flow analysis, financial ratio calculations
- **Scope:** Financial analysis (bounded domain)
- **Auto-activation:** Multiple trigger patterns

## Content Structure

The skill contains:

1. **Financial Statement Analysis Framework**
   - Balance sheet analysis
   - Income statement analysis
   - Cash flow statement analysis
   - Ratio analysis

2. **Key Financial Metrics**
   - Liquidity ratios
   - Profitability ratios
   - Leverage ratios
   - Efficiency ratios

3. **Analysis Methodology**
   - Horizontal analysis (trends over time)
   - Vertical analysis (component percentages)
   - Comparative analysis (vs peers/industry)

4. **Reporting Templates**
   - Executive summary format
   - Detailed analysis structure
   - Recommendations framework

## Key Decisions

### Decision 1: Single File

**Why:** Financial analysis is coherent domain with related concepts

**Alternative:** Could split into:
- `SKILL.md` (overview)
- `references/ratios.md` (detailed ratio formulas)
- `references/frameworks.md` (analysis methodologies)

**Trade-off:** Simplicity vs progressive disclosure

### Decision 2: Embedded Formulas

**What:** All ratio formulas embedded directly in skill

**Why:** Financial analysts need formulas immediately

**Result:** No external lookup needed, instant access

### Decision 3: Template-Driven

**What:** Provides output templates for reports

**Why:** Consistency in financial reporting

**Pattern:** Input (data) → Analysis (methodology) → Output (template)

## What Works Well

✅ **Clear domain boundaries** - Financial analysis, not all business tasks
✅ **Multiple triggers** - Different ways to invoke (analyze, calculate, provide insights)
✅ **Embedded knowledge** - Formulas and frameworks included, no external deps
✅ **Template-driven** - Shows output format expected
✅ **Progressive structure** - Intro → Metrics → Methodology → Templates

## What Could Be Enhanced

**Potential improvements:**
- Progressive disclosure for advanced topics
- Industry-specific analysis patterns (tech vs retail)
- Examples of real financial analysis
- Edge cases and special situations

**Trade-offs:**
- More comprehensive vs simpler/faster loading
- General purpose vs industry-specific
- Single file vs multiple references

## Lessons for Skill Creation

### Lesson 1: Domain Expertise Packaging

**Pattern:**
```
Domain Knowledge = Frameworks + Metrics + Methodology + Templates
```

**Application:**
- Identify core concepts (financial ratios)
- Document methodology (how to analyze)
- Provide templates (expected output format)
- Embed everything (no external deps)

### Lesson 2: Clear Activation Triggers

**Pattern:**
```
description: Use when [task 1], [task 2], or [task 3]. Auto-activates for [domain keywords].
```

**Application:**
- List specific tasks (analyzing statements, calculating metrics)
- Include domain keywords (balance sheet, income statement)
- Multiple entry points (analyzing, calculating, providing)

### Lesson 3: Self-Contained

**Pattern:**
- All formulas embedded
- All frameworks documented
- All templates included
- No external references needed

**Application:**
- User never needs to look elsewhere
- Claude has all context immediately
- No API calls or doc fetching

## Applying to Your Skills

**Use this pattern when:**
- Clear domain expertise to package (coaching, writing, analysis)
- Related concepts that work together (all financial concepts)
- Formulas/frameworks to embed (methodologies, templates)
- Auto-activation desired (not user-invoked command)

**Structure to adopt:**
```
your-skill/
├── SKILL.md
│   ├── Domain overview
│   ├── Key concepts/formulas
│   ├── Methodology
│   └── Templates/examples
```

**Size target:** 20-40KB for single-file domain expertise skill

## Comparison to Our Skills

**Similar to:**
- `business-writing` - Domain expertise (B2B communication)
- `coaching-frameworks` - Methodology packaging

**Different from:**
- `garden-protocol` - Multi-pass process, not domain knowledge
- `claude-code-platform` - Comprehensive with progressive disclosure

**Pattern fit:**
- Single domain: financial-analysis pattern ✅
- Multiple passes: garden-protocol pattern ✅
- Platform knowledge: claude-code-platform pattern ✅

---

*"Domain expertise + clear triggers + embedded knowledge = effective simple skill"*
