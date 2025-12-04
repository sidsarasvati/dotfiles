# Advanced Claude Code Features

Deep dive into Skills, Checkpoints, Subagents, Hooks, and Haiku 4.5.

## Table of Contents
- [Skills System](#skills-system)
- [Checkpoints](#checkpoints)
- [Subagents](#subagents)
- [Hooks](#hooks)
- [Haiku 4.5](#haiku-45)
- [Permission Modes](#permission-modes)

---

## Skills System

### What are Skills?

**Skills** are organized packages of instructions, code, and resources that give Claude specialized capabilities. Think of them as "expertise packages" that Claude discovers and loads dynamically.

**This very skill is an example of the Skills system in action.**

### Skill Architecture

**Progressive Disclosure** - Skills load in stages to minimize token usage:

**Level 1** (Startup - Metadata only):
- Skill name and description
- Loaded into system prompt
- Minimal tokens (~20 per skill)

**Level 2** (When Relevant - Full SKILL.md):
- Complete skill instructions
- Usage guidance
- Reference to deeper docs
- Loads when Claude determines relevance

**Level 3+** (On Demand - Reference Files):
- Additional markdown files
- Bundled resources
- Code scripts
- Loads only when needed

### Skill Structure

```
~/.claude/skills/my-skill/
├── SKILL.md           # Required: Core instructions
├── reference.md       # Optional: Deep reference
├── scripts/           # Optional: Executable code
│   └── processor.py
└── resources/         # Optional: Templates, data
    └── template.xlsx
```

### SKILL.md Format

**Required YAML frontmatter**:
```yaml
---
name: Skill Name
description: Brief description of what this skill does
---
```

Followed by markdown content with instructions, examples, and guidance.

### Creating Custom Skills

**Location Options**:
- Personal skills: `~/.claude/skills/skill-name/` (available everywhere)
- Project skills: `.claude/skills/skill-name/` (version controlled, team shared)

**Creation Methods**:
1. Manual creation (create directory + SKILL.md)
2. Using skill-creator skill (interactive guidance)
3. Through conversation with Claude

**Skill Capabilities**:
- Provide domain expertise
- Include executable code
- Bundle resources (templates, data)
- Compose with other skills

### How to Create a Skill (Step-by-Step)

#### Step 1: Create Directory Structure

```bash
# For personal skill (available everywhere)
mkdir -p ~/.claude/skills/my-skill-name

# For project skill (version controlled)
mkdir -p .claude/skills/my-skill-name
```

#### Step 2: Create SKILL.md with Required YAML

**CRITICAL**: YAML frontmatter is REQUIRED with exact format:

```markdown
---
name: My Skill Name
description: Brief description of what this skill does and when it triggers
---

# Skill content starts here
```

**YAML Requirements**:
- `name`: Maximum 64 characters
- `description`: Maximum 1024 characters
- Description should include TRIGGER KEYWORDS for skill discovery
- Three dashes before and after (---)

**Good description example**:
```yaml
description: Expert knowledge of Protocol Thinking framework - Phase 0 → Pass 1-N → Phase N pattern, behavioral strata design, separation of concerns, and protocol evolution
```

**Bad description example**:
```yaml
description: Helps with protocols
```

#### Step 3: Write Core Knowledge in SKILL.md

**Pattern**: Put essential knowledge directly in SKILL.md, reference deeper files

```markdown
## Core Knowledge

### Quick Reference
- Tool 1: Brief description and key parameters
- Tool 2: Brief description and key parameters

### Common Patterns
Pattern 1: Step-by-step
Pattern 2: Step-by-step

## When You Need More Details

### Deep Dive on Topic A
For comprehensive information about X, read:
```bash
cat ~/.claude/skills/my-skill-name/topic-a-reference.md
```

### Complete Guide to Topic B
For detailed Y information, read:
```bash
cat ~/.claude/skills/my-skill-name/topic-b-guide.md
```
```

**Why this works**: Claude sees bash command, executes it to load file on-demand.

#### Step 4: Create Reference Files (Optional)

Create additional markdown files for deep topics:

```bash
# Example reference files
~/.claude/skills/my-skill-name/
├── SKILL.md                    # Core knowledge (always loaded)
├── detailed-reference.md       # Deep dive (load on demand)
├── examples.md                 # Usage examples (load on demand)
└── troubleshooting.md          # Common issues (load on demand)
```

#### Step 5: Test the Skill

**Testing checklist**:

1. **Restart Claude Code** - Skills load at startup
   ```bash
   # Exit Claude Code, then restart
   ```

2. **Test Metadata Loading** - Ask question in skill's domain
   ```
   Ask: "Question related to skill topic"
   Expected: Claude recognizes skill is relevant
   ```

3. **Test Core Knowledge** - Ask basic question
   ```
   Expected: Answer from SKILL.md without loading files
   ```

4. **Test Progressive Loading** - Ask deep question
   ```
   Expected: See bash command loading reference file
   Example: cat ~/.claude/skills/my-skill-name/reference.md
   ```

5. **Verify Token Efficiency**
   - Metadata: ~20 tokens at startup
   - Full SKILL.md: Only when triggered
   - Reference files: Only when needed

#### Step 6: Iterate Based on Usage

**Watch for**:
- Skill not triggering? → Improve description with better keywords
- Loading wrong files? → Adjust bash commands in SKILL.md
- Too verbose? → Move details to reference files
- Missing info? → Add to appropriate file

### Skill Creation Best Practices

#### Progressive Disclosure Pattern

**Level 1** (SKILL.md - Core):
- Essential concepts and patterns
- Quick reference
- When/how to load deeper files

**Level 2** (Reference files - On Demand):
- Comprehensive documentation
- Detailed examples
- Troubleshooting guides

**Level 3** (Sections - Specific):
- Specific portions of reference files

#### File Reference Pattern

**Always use explicit bash commands**:
```markdown
✓ For details, read:
  ```bash
  cat ~/.claude/skills/skill-name/reference.md
  ```

✗ For details, see reference.md
✗ For details, load the reference file
```

**Why**: Claude needs explicit instruction to execute bash command.

#### Description Writing Tips

**Include**:
- What the skill knows
- When it should trigger
- Key concepts/tools/patterns
- Specific terminology users might use

**Example**:
```yaml
description: Git workflow expertise including commit protocols (Phase 0 → 1 → 2 → N), PR creation with gh CLI, branch management, commit message formatting, and pre-commit hook handling
```

**Keywords**: commit, PR, git, branch, merge, hook

#### Scope Guidelines

**Good skill scope**:
- `claude-code-platform` - All Claude Code tools and features
- `protocol-thinking` - Protocol design framework
- `git-workflows` - Git operations and best practices

**Too broad**:
- `programming` - Way too general
- `everything` - Not focused

**Too narrow**:
- `read-tool-offset-parameter` - Too specific
- `one-command` - Not enough value

### Testing Skills in Practice

**Create test questions**:

```markdown
Basic: "What is [core concept]?"
→ Should answer from SKILL.md

Medium: "How do I [common task]?"
→ Should answer from SKILL.md

Deep: "What's the complete protocol for [complex task]?"
→ Should load reference file via bash

Edge: "What if [unusual situation]?"
→ Should load troubleshooting file
```

**Verify progressive loading**:
- Check conversation for bash commands
- Confirm only needed files loaded
- Monitor token usage

### Common Skill Creation Mistakes

**Missing YAML frontmatter**:
```markdown
✗ # My Skill (no YAML)

✓ ---
  name: My Skill
  description: ...
  ---
```

**Vague descriptions**:
```yaml
✗ description: Helps with stuff
✓ description: Git commit protocol expertise - Phase 0 information gathering, commit message formatting, hook handling, and PR creation
```

**Not using bash commands for file loading**:
```markdown
✗ See reference.md for details
✓ Read details:
  ```bash
  cat ~/.claude/skills/skill-name/reference.md
  ```
```

**Everything in SKILL.md**:
```markdown
✗ 3000 lines of documentation in SKILL.md
✓ Core knowledge in SKILL.md, details in reference files
```

### Example: Creating a Protocol Thinking Skill

**Step 1**: Create directory
```bash
mkdir -p ~/.claude/skills/protocol-thinking
```

**Step 2**: Create SKILL.md
```yaml
---
name: Protocol Thinking
description: Protocol design framework - Phase 0 → Pass 1-N → Phase N pattern, behavioral strata not instructions, separation of concerns, forced sequencing, built-in verification, and protocol evolution through execution
---

# Protocol Thinking Framework

Core philosophy: "Protocols create conditions where RIGHT behavior is EASY behavior"

## The Universal Pattern

Every protocol follows:
- Phase 0: DETECTION → Know where you are
- Pass 1-N: EXECUTION → Do one thing well (separated concerns)
- Phase N: VERIFICATION → Confirm it worked

## Key Principles

**Behavioral Strata**: Not instructions ("do this") but conditions that make right behavior inevitable.

**Separation of Concerns**: One phase, one job. Prevents cognitive overload.

**Forced Sequencing**: Can't skip steps. Prevents mistakes.

**Built-in Verification**: Catches failures immediately.

## When to Use Protocols

Use when:
- Multiple steps that interfere with each other
- Failure patterns repeating
- Quality depends on NOT skipping steps

Don't use when:
- Single atomic operation
- One-time exploration
- Learning/discovery mode

## Examples

For detailed protocol examples and design patterns, read:
```bash
cat ~/.claude/skills/protocol-thinking/examples.md
```

For protocol evolution and improvement strategies, read:
```bash
cat ~/.claude/skills/protocol-thinking/evolution.md
```
```

**Step 3**: Test
- Restart Claude Code
- Ask: "What is protocol thinking?"
- Verify answer from SKILL.md
- Ask: "Show me detailed protocol examples"
- Verify bash loads examples.md

### Built-in Skills

Claude comes with pre-built skills:
- **Excel (xlsx)** - Create/manipulate Excel workbooks
- **PowerPoint (pptx)** - Generate presentations
- **PDF (pdf)** - Create formatted PDFs
- **Word (docx)** - Generate Word documents

### Skill Marketplace

Browse and install skills:
```bash
# View available skills
claude skills list

# Install from marketplace
claude skills install skill-name

# Create new skill
claude skills create my-skill
```

(Note: Exact commands may vary - check current CLI reference)

### Skills Best Practices

**Scope**:
- Focus on specific domain
- Don't try to be all-encompassing
- Think "Excel skill" not "Office skill"

**Documentation**:
- Clear SKILL.md overview
- Progressive disclosure for details
- Examples and usage patterns

**Composition**:
- Skills can reference other skills
- Build on existing expertise
- Combine multiple skills for complex workflows

### Skills vs Other Abstractions

**Skills** = Portable expertise (works everywhere Claude does)
**Subagents** = Specialized execution contexts (Task tool)
**Hooks** = Execution interceptors (Pre/Post tool use)
**Slash Commands** = User-defined workflows (project-specific)

---

## Checkpoints

### What are Checkpoints?

**Checkpoints** save conversation state at any point, allowing you to:
- Roll back to previous conversation state
- Branch from earlier points
- Recover from mistakes
- Experiment safely

**Status**: Experimental feature (as of Oct 2025)

### Using Checkpoints

**Creating checkpoints**:
```bash
# Create checkpoint (automatic ID)
/checkpoint

# Create named checkpoint
/checkpoint save "before major refactor"
```

**Listing checkpoints**:
```bash
/checkpoints list
```

**Restoring checkpoints**:
```bash
# Restore by ID
/checkpoint restore abc123...

# Restore by name
/checkpoint restore "before major refactor"
```

**Deleting checkpoints**:
```bash
/checkpoint delete abc123...
```

### Checkpoint Use Cases

**Experimental Work**:
```
1. Checkpoint before experiment
2. Try risky changes
3. If fails: restore checkpoint
4. If succeeds: continue from new state
```

**Protocol Evolution**:
```
1. Checkpoint before protocol changes
2. Test new protocol structure
3. Roll back if doesn't work
4. Iterate safely
```

**Bug Investigation**:
```
1. Checkpoint before debugging
2. Try multiple approaches
3. Restore to try different path
4. Keep successful approach
```

### Checkpoint Limitations

- Experimental feature (behavior may change)
- May not persist across Claude Code restarts
- Storage limits may apply
- Network operations not rolled back

### Checkpoint Best Practices

- Create checkpoints before major changes
- Use descriptive names
- Clean up old checkpoints
- Don't rely on checkpoints for production workflows
- Combine with version control (git)

---

## Subagents

### What are Subagents?

**Subagents** are specialized AI agents launched via the Task tool for complex, multi-step tasks. They provide focused expertise and autonomous execution.

### Built-in Subagents

**General Purpose**:
- `general-purpose` - Research, code search, multi-step tasks
- Tools: All tools available

**Setup Specialists**:
- `statusline-setup` - Configure Claude Code status line
- `output-style-setup` - Create output styles
- Tools: Read, Edit

**Codebase Exploration**:
- `Explore` - Fast codebase navigation and understanding
- Thoroughness levels: quick, medium, very thorough
- Tools: Glob, Grep, Read, Bash

**Engineering**:
- `pragmatic-lead` - Pragmatic programming (debug, architect, ship)
- Tools: ALL

**Testing**:
- `qa` - Browser testing with Playwright MCP
- Tools: Read, Write, Edit, Bash, mcp__playwright__*

### Creating Custom Subagents

**Location**: `.claude/agents/agent-name.md`

**Format**:
```markdown
# Agent Name

## Identity
Who this agent is and what it does

## Domain Expertise
Specific knowledge and capabilities

## Tools
- Read
- Write
- Bash
- mcp__service__*

## Protocols
How this agent approaches tasks

## Success Metrics
How to measure success
```

**Via CLI**:
```bash
# Define agent via JSON
claude --agents '{
  "reviewer": {
    "description": "Code review specialist",
    "prompt": "You are a senior code reviewer...",
    "tools": ["Read", "Grep", "Bash"],
    "model": "sonnet"
  }
}' "Review the latest changes"
```

### Using Subagents

**Via Task Tool**:
```
Tool: Task
Parameters:
  subagent_type: "Explore"
  prompt: "Find all API endpoint definitions..."
  description: "Explore API endpoints"
```

**Key Considerations**:
- Each invocation is stateless
- Agent can't communicate mid-task
- Provide detailed autonomous instructions
- Specify exact information to return

### Subagent Best Practices

**When to Use**:
- Multi-step autonomous tasks
- Specialized expertise needed
- Deep codebase exploration
- Complex workflows benefit from focus

**When NOT to Use**:
- Single direct operations
- Simple file reads
- Tasks where you're faster
- Overhead exceeds value

**Optimization**:
- Launch multiple agents in parallel
- Provide comprehensive context
- Be specific about deliverables
- Trust agent's output

### Subagent Patterns

**Code Review**:
```
Agent: code-reviewer
Context: Recent commits
Task: Review changes, identify issues
Output: Findings and suggestions
```

**Codebase Understanding**:
```
Agent: Explore (thoroughness: medium)
Context: Project structure
Task: Map architecture and patterns
Output: System diagram and key files
```

**Bug Investigation**:
```
Agent: pragmatic-lead
Context: Bug report and logs
Task: Debug, identify root cause
Output: Fix and prevention strategy
```

---

## Hooks

### What are Hooks?

**Hooks** are execution gates that run before (PreToolUse) or after (PostToolUse) tool invocations. They enable:
- Real-time guidance
- Execution gates
- Validation logic
- Enhanced results

### Hook Types

**PreToolUse Hooks**:
- Run BEFORE tool execution
- Can provide guidance
- Can block execution
- Set conditions for tool use

**PostToolUse Hooks**:
- Run AFTER tool execution
- Process results
- Add context
- Enhance output

### Hook Location

**Global hooks**: `~/.claude/hooks/`
**Project hooks**: `.claude/hooks/`

### Hook File Structure

**PreToolUse Example** (`pre-tool-use.py`):
```python
def pre_tool_use(tool_name, tool_params):
    """
    Called before tool execution
    Returns: guidance message or None
    """
    if tool_name == "Bash" and "rm -rf" in tool_params["command"]:
        return "WARNING: Destructive command detected"
    return None
```

**PostToolUse Example** (`post-tool-use.py`):
```python
def post_tool_use(tool_name, tool_result):
    """
    Called after tool execution
    Returns: enhanced result or None
    """
    if tool_name == "Read" and len(tool_result) > 1000:
        return f"Note: Large file read ({len(tool_result)} chars)"
    return None
```

### Hook Use Cases

**Execution Gates**:
```
Block analysis when DUE TODAY items exist
Force action on critical work first
Prevent premature optimization
```

**Data Verification**:
```
Verify timestamps before writes
Check git status before commits
Validate data integrity
```

**Context Enhancement**:
```
Add time awareness (DAEMON whisper)
Inject project-specific reminders
Provide real-time guidance
```

### Hook Best Practices

**Keep Hooks Fast**:
- Hooks run on every tool use
- Slow hooks = slow execution
- Cache results when possible

**Be Specific**:
- Target specific tools
- Check specific conditions
- Avoid broad interventions

**Don't Override Core Behavior**:
- Hooks guide, don't control
- Provide information, not commands
- Respect user autonomy

### Hook Examples from MULTIPLEX

**DAEMON Time Whisper** (PostToolUse):
```
After user submits prompt:
"🔧 DAEMON whispers: It's Thu Oct 16 15:12:56 EDT 2025"
```

**Execution Gate** (PreToolUse):
```
Before analysis tools:
"Stop - DUE TODAY items exist. Do critical work first."
```

---

## Haiku 4.5

### What is Haiku 4.5?

**Haiku 4.5** is Claude's fast model optimized for:
- Speed: 2x faster than Sonnet 4.5
- Cost: 1/3 the price of Sonnet 4.5
- Performance: 73.3% on SWE-bench Verified (near Sonnet 4 level)

### When to Use Haiku

**Good for Haiku**:
- Routine file operations
- Simple queries
- Repetitive tasks
- Rapid prototyping
- Testing iterations
- Journal writing
- Status updates

**Keep Sonnet for**:
- Complex reasoning
- Strategic decisions
- Protocol design
- Architecture planning
- Novel problem-solving
- Critical bug fixes

### Using Haiku

**Via CLI**:
```bash
# Use Haiku for this session
claude --model haiku

# Use Haiku with specific task
claude --model haiku "List all TODO comments"
```

**Via API**:
```python
client.messages.create(
    model="claude-haiku-4-5-20250929",
    # ... rest of params
)
```

### Haiku Best Practices

**Test First**:
- Try Haiku on your tasks
- Measure quality vs speed
- Determine task classification

**Hybrid Approach**:
- Haiku for structure
- Sonnet for strategy
- Route based on task type

**Cost Optimization**:
- Use Haiku for high-volume operations
- Switch to Sonnet when complexity requires
- Monitor quality/cost tradeoff

---

## Permission Modes

### What are Permission Modes?

Permission modes control how Claude Code requests approval for tool usage.

### Available Modes

**default**:
- Ask for permission based on settings
- Respect allowedTools/disallowedTools
- Standard user interaction

**acceptEdits**:
- Auto-approve all Edit tool calls
- Still ask for other tools
- Useful for heavy refactoring

**bypassPermissions**:
- Skip ALL permission checks
- Use with caution
- Good for sandboxed environments only

**plan**:
- Plan mode - no execution
- Research and present plans
- User approves before execution

### Using Permission Modes

**Via CLI**:
```bash
# Start in plan mode
claude --permission-mode plan

# Bypass permissions (dangerous!)
claude --dangerously-skip-permissions
```

**During Session**:
```
User can switch modes mid-session
Commands available in interactive mode
```

### Permission Best Practices

**default mode**:
- Best for normal use
- Configure allowedTools for common operations
- Reduces interruptions while maintaining safety

**plan mode**:
- Use for complex multi-step operations
- Get user approval before execution
- Good for learning Claude Code

**bypassPermissions**:
- ONLY in sandboxed environments
- NEVER with internet access
- NEVER on production systems

---

## Feature Comparison

| Feature | Purpose | When to Use |
|---------|---------|-------------|
| **Skills** | Portable expertise | Domain knowledge needed everywhere |
| **Checkpoints** | Save/restore state | Experimental work, safe iteration |
| **Subagents** | Specialized execution | Complex autonomous tasks |
| **Hooks** | Execution gates | Real-time guidance, validation |
| **Haiku 4.5** | Fast/cheap model | Routine tasks, high volume |

---

## Advanced Feature Combinations

**Skills + Subagents**:
```
Subagent with skill expertise
Tools include skill-specific knowledge
Portable specialized agents
```

**Hooks + MCP**:
```
PreToolUse: Validate MCP calls
PostToolUse: Process MCP results
Enhanced MCP integration
```

**Checkpoints + Experimentation**:
```
Checkpoint → Try approach A → Restore → Try approach B
Safe exploration of alternatives
```

**Haiku + Sonnet Routing**:
```
Haiku for file operations
Sonnet for strategic decisions
Optimize cost/performance
```

---

*Advanced features guide for Claude Code*
*Version 1.0 - Oct 16, 2025*
