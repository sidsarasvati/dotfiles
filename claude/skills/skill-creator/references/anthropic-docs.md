# Anthropic Skills Documentation (Embedded)

> **Source:** https://docs.claude.com/en/docs/agents-and-tools/agent-skills/overview
> **Embedded:** Mon Oct 27, 2025
> **Purpose:** Never fetch external docs - all knowledge captured here

## Overview

Agent Skills allow you to add specialized instructions to your Claude Code projects. Instead of writing those instructions directly in your CLAUDE.md file, you can organize them in a separate skills/ folder. This helps you:

- Keep CLAUDE.md focused on high-level context
- Reuse expertise across projects
- Enable auto-activation based on task context
- Scale to many skills without context bloat

## How Skills Work

### Basic Structure

Skills live in `.claude/skills/` directory (global) or `[project]/.claude/skills/` (project-specific).

Each skill is a folder containing:
- `SKILL.md` (required) - Main skill file with YAML frontmatter
- Additional markdown files (optional) - Supporting documentation

### YAML Frontmatter

Every SKILL.md must start with YAML frontmatter:

```yaml
---
name: skill-name
description: When this skill should activate and what it provides
---
```

**Required fields:**
- `name:` - Lowercase with hyphens, matches folder name
- `description:` - Triggers auto-activation (critical for model-invoked skills)

### File Organization

**Simple skill:**
```
my-skill/
└── SKILL.md
```

**Complex skill with references:**
```
my-skill/
├── SKILL.md (entry point)
├── detailed-guide.md
├── advanced-patterns.md
└── examples/
    └── working-example.md
```

## Auto-Activation (Model-Invoked Skills)

Skills can auto-activate based on task context. The `description` field determines when Claude loads the skill.

### Writing Effective Descriptions

**Good descriptions:**
- Start with "Use when..." or "Provides..."
- Include specific trigger scenarios
- Mention key terms that signal relevance
- Keep it 1-3 sentences

**Examples:**

```yaml
description: Use when closing or reviewing daily journal entries in Logseq to apply the 7-pass garden tending protocol.
```

```yaml
description: Use when creating, designing, or building Claude Code skills. Auto-activates for "create a skill", "build a skill", or "make this into a skill".
```

```yaml
description: Provides comprehensive knowledge about Claude Code platform features, tools, MCP integration, and advanced capabilities.
```

### What Triggers Auto-Activation

Claude analyzes:
- User's message content
- Current task context
- Tools being used
- Project state

If description matches context, skill loads automatically.

### Model-Invoked vs User-Invoked

**Model-invoked (auto):**
- Description determines activation
- Loads when contextually relevant
- User doesn't need to know it exists

**User-invoked (manual):**
- Commands in `.claude/commands/` directory
- User explicitly calls with `/command-name`
- Use for user-controlled operations

## Progressive Disclosure

Skills support progressive disclosure - loading detailed docs only when needed.

### How It Works

**SKILL.md** (always loaded when skill activates):
- Overview and core instructions
- Quick reference
- Pointers to detailed docs

**Supporting files** (loaded on-demand):
- Detailed methodologies
- Advanced patterns
- Examples and templates
- Reference documentation

### Benefits

- Fast activation (minimal initial context)
- Deep expertise available when needed
- Scales to 20+ skills without bloat
- User loads what they need

### Example Structure

```
garden-protocol/
├── SKILL.md (7-pass overview, always loaded)
├── pass-details.md (deep dive on each pass, loaded if asked)
├── examples.md (gold standard examples, loaded if needed)
└── troubleshooting.md (edge cases, loaded if problems)
```

## Best Practices

### Skill Design

1. **Single responsibility**: Each skill focused on one domain
2. **Clear activation**: Description precisely targets use cases
3. **Progressive disclosure**: Core in SKILL.md, details in supporting files
4. **Self-contained**: Embed necessary knowledge, minimize external deps
5. **Testable**: Verify auto-activation works as expected

### Description Writing

1. **Be specific**: "Use when closing daily journal entries" > "For journal work"
2. **Include triggers**: Mention key phrases that signal relevance
3. **Explain value**: What does this skill provide?
4. **Stay concise**: 1-3 sentences ideal
5. **Test activation**: Verify it loads when expected

### File Organization

1. **SKILL.md first**: Entry point with complete overview
2. **Logical grouping**: Related content in subdirectories
3. **Descriptive names**: `yaml-guide.md` > `guide.md`
4. **Size discipline**: Keep files focused (5-20KB each)
5. **Clear structure**: Use headers, bullets, code blocks

### Content Quality

1. **Protocol thinking**: Behavioral strata, not instruction lists
2. **Examples over theory**: Show working patterns
3. **Separation of concerns**: Each section has one job
4. **Scannable structure**: Easy to find what you need
5. **Voice consistency**: Maintain project's communication style

## Debugging Skills

### Skill Not Loading

**Check:**
- [ ] Folder name matches YAML `name:` field
- [ ] SKILL.md exists in folder
- [ ] YAML frontmatter valid (three dashes, required fields)
- [ ] Description includes relevant trigger terms
- [ ] Skill located in `.claude/skills/` or project `.claude/skills/`

### Wrong Skill Loading

**Fix:**
- Make description more specific
- Remove overly broad trigger terms
- Test with different phrasings

### Skill Loading Too Often

**Fix:**
- Narrow description scope
- Remove generic trigger terms
- Make activation criteria more specific

## Size Guidelines

**Per skill:**
- Simple: 10-30KB
- Medium: 30-60KB
- Complex: 60-100KB
- Very complex: 100-150KB (rare)

**Total skills:**
- Aim for 10-20 skills max per project
- Use progressive disclosure for larger skill sets
- Global skills (`.claude/skills/`) available everywhere
- Project skills override global if same name

## Migration Strategies

### From CLAUDE.md to Skills

1. Identify self-contained sections in CLAUDE.md
2. Determine if used frequently enough (3+ times/week)
3. Extract to skill with clear auto-activation
4. Test that skill loads correctly
5. Remove from CLAUDE.md once validated
6. Keep CLAUDE.md for identity/high-level context

### From Commands to Skills

**When to use commands:**
- User-controlled operations
- Explicit invocation needed
- Multiple steps requiring approval
- State changes (wake, shutdown)

**When to use skills:**
- Auto-activate based on context
- Provide expertise/guidance
- Domain-specific knowledge
- Reusable patterns

**Hybrid approach:**
- Command can invoke skill internally
- `/context-logseq` command loads logseq-operations skill
- User controls invocation, skill provides expertise

## Advanced Patterns

### Meta-Skills

Skills that help create other skills:
- skill-creator (this skill)
- protocol-design (designing protocols)
- documentation-writer (generating docs)

### Skill Composition

Skills can reference other skills:
- garden-protocol uses protocol thinking principles
- business-writing uses coaching frameworks
- Skills build on shared foundations

### Testing Skills

**Manual testing:**
1. Use trigger phrases in conversation
2. Verify skill loads (check context usage)
3. Confirm expertise provided correctly
4. Refine description if needed

**Automated testing:**
- Not yet available in Claude Code
- Manual verification required
- Test with real usage scenarios

## Common Pitfalls

### Pitfall 1: Description Too Broad

❌ `description: Use for Logseq work`
✅ `description: Use when closing or reviewing daily journal entries in Logseq`

**Problem:** Too broad loads skill unnecessarily
**Fix:** Be specific about when to activate

### Pitfall 2: All Content in SKILL.md

❌ 200KB SKILL.md with everything
✅ 20KB SKILL.md + 50KB supporting files

**Problem:** Slow activation, context bloat
**Fix:** Use progressive disclosure

### Pitfall 3: Vague Instructions

❌ "Follow best practices for journals"
✅ "Pass 1: Read entire journal. Pass 2: Absorb field reports."

**Problem:** AI doesn't know what to do
**Fix:** Use protocol thinking (behavioral strata)

### Pitfall 4: No Examples

❌ Abstract theory only
✅ Working examples showing patterns

**Problem:** Hard to apply knowledge
**Fix:** Include real examples

### Pitfall 5: External Dependencies

❌ "See Anthropic docs at URL"
✅ Embed complete docs in references/

**Problem:** Requires fetching external content
**Fix:** Embed everything needed

## Example Skills

### Simple Skill (business-writing)

**Purpose:** B2B email templates in Sid's voice

**Structure:**
```
business-writing/
├── SKILL.md (templates + voice guide)
└── examples/
    └── email-examples.md
```

**Activation:** "Write email to..." "Draft message for..."

**Size:** ~25KB

### Protocol Skill (garden-protocol)

**Purpose:** 7-pass journal tending methodology

**Structure:**
```
garden-protocol/
├── SKILL.md (7-pass protocol overview)
├── examples/
│   └── gold-standards.md
└── troubleshooting.md
```

**Activation:** "Close journal" "Review daily entry"

**Size:** ~35KB

### Platform Skill (claude-code-platform)

**Purpose:** Complete Claude Code knowledge

**Structure:**
```
claude-code-platform/
├── SKILL.md (overview + quick ref)
├── tools/
│   ├── bash.md
│   ├── read.md
│   └── write.md
└── mcp/
    └── integration-guide.md
```

**Activation:** "How does MCP work?" "Use Bash tool"

**Size:** ~96KB

## Resources

**Official Docs:**
- https://docs.claude.com/en/docs/agents-and-tools/agent-skills/overview

**Anthropic Cookbooks:**
- https://github.com/anthropics/claude-cookbooks/tree/main/custom_skills

**Example Skills:**
- See `.claude/skills/` directory in your projects
- garden-protocol (protocol pattern)
- business-writing (simple pattern)
- claude-code-platform (comprehensive pattern)

---

*"Skills = Crystallized expertise that auto-activates when needed"*
*"Progressive disclosure = 20+ skills without context bloat"*
