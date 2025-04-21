# Claude Code Configuration

This directory contains configuration for Claude Code CLI.

## Organization and Memory Management

Claude uses special `CLAUDE.md` files for contextual memory:

- **Global Memory** (`~/.claude/CLAUDE.md`): Applied to all Claude sessions
- **Project Memory** (`/path/to/project/CLAUDE.md`): Applied to specific projects
- **Local Memory** (`CLAUDE.local.md`): Personal settings (should be .gitignored)

## Best Practices

### Global vs Project Memory

**Global Memory (~/.claude/CLAUDE.md) should contain:**
- Personal engineering philosophy
- Frequently used commands across all projects
- Development environment settings
- Cross-project package management preferences
- Git workflow preferences that apply to all repos
- Common troubleshooting steps

**Project Memory (CLAUDE.md) should contain:**
- Project-specific commands
- Codebase architecture information
- Style guides specific to the project
- Test instructions
- Common patterns and idioms
- Important files and directories
- Unique project workflows
- Repository etiquette (branch naming, etc.)

### Content Optimization

- Keep files concise and human-readable
- Refine CLAUDE.md files like any prompt
- Experiment to find what produces best instruction following
- Use emphasis ("IMPORTANT", "YOU MUST") for critical instructions
- Document as you go by using the `#` key to incorporate instructions
- Include code examples for common patterns

## Example Structure

```markdown
# Bash commands
- npm run build: Build the project
- npm run typecheck: Run the typechecker

# Code style
- Use ES modules (import/export) syntax, not CommonJS (require)
- Destructure imports when possible (eg. import { foo } from 'bar')

# Workflow
- Be sure to typecheck when you're done making a series of code changes
- Prefer running single tests, and not the whole test suite, for performance
```

## Installation

The claude configuration is managed with manual symlinks (not stow):

```bash
# Create directories if needed
mkdir -p ~/.claude

# Create symlink for Claude config file
ln -sf ~/Code/sid/dotfiles/claude/.claude/CLAUDE.md ~/.claude/CLAUDE.md
```

The install.sh script in the dotfiles repository handles this automatically.