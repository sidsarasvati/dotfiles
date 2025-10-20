# dotfiles - Intelligence Archive

**Memory is not storage - it's reference patterns that emerge from usage.**

---

## Installation System Rebuild (Session: Apr 2025)

### What Happened
Completely rebuilt the installation system to eliminate external dependencies and add intelligent detection.

### Key Changes
- **Removed GNU stow dependency** - Now uses direct symlinks with absolute paths
- **Interactive installation** - Full prompts with "No" as safe default
- **Smart detection** - Checks for already-installed configurations before acting
- **Diff and backup** - Shows differences and backs up existing configs
- **Doom Emacs detection** - Special handling with guidance
- **XDG configuration detection** - Warns about ~/.config conflicts

### Patterns Discovered
1. **Safe defaults pattern**: "No" as default prevents accidental overwrites
2. **Show before modify**: Display diffs before making changes
3. **Backup before replace**: Always create backups of existing configs
4. **Special case detection**: Platform-specific and tool-specific handling

### Files Modified
- `install.sh` - Complete rewrite of installation logic
- Added `local.zsh.example` - Template for private tokens
- Improved `.zshenv` integration

### Technical Decisions
- **Direct symlinks** > GNU stow (simpler, more reliable, no external deps)
- **Interactive** > Automatic (safer, more transparent)
- **Absolute paths** > Relative paths (works from any directory)

---

## ZSH Configuration Architecture

### Structure
- Custom zsh config with literate programming approach
- Multiple prompt styles with git integration
- Emacs-style keybindings
- No external dependencies (no oh-my-zsh required)

### Key Files
- `zsh/CLAUDE.md` - ZSH-specific context and structure
- `zsh/local.zsh.example` - Template for private environment variables
- `zsh/.zshenv` - Environment setup

### Pattern: Private Config Template
Instead of gitignoring entire files, provide `.example` templates:
- Check in: `local.zsh.example` (with placeholder values)
- Gitignore: `local.zsh` (actual secrets)
- User copies and fills in real values

---

## Repository Organization Principles

### Multi-Year Evolution (12+ years)
- Started with general Unix configs
- Evolved to macOS-focused (last 10 years)
- Accumulated tools and patterns over time

### Current Focus
- macOS primary target
- Limited Linux support (legacy, not actively maintained)
- No Windows support (historical only)

### Tool Categories
- All configurations are primary tools (no "misc" category)
- Organized by tool/category (git, emacs, zsh, claude)
- Educational documentation in org-mode under `docs/emacs/`

---

## Build & Dependency Management

### Homebrew Integration
- `Brewfile` - Declarative package management
- `brew bundle` - Install all packages
- `brew bundle check` - Verify installations
- `brew bundle cleanup` - Remove unlisted packages

### Package Organization
Brewfile organized by category:
- Development tools
- CLI utilities
- GUI applications
- Fonts

---

## Architecture Detection & Platform Support

### Cross-Platform Compatibility
- Detect OS type (darwin/linux) for platform-specific configs
- Architecture detection (Apple Silicon vs Intel)
- PATH configuration adjusted for Homebrew on different architectures

### Apple Silicon Specifics
- Updated PATH for Homebrew on Apple Silicon
- Handle differences in Homebrew install locations
- `/opt/homebrew` vs `/usr/local`

---

## Development Workflow Established

### Branch Strategy
- Feature branches: `feature/name-of-enhancement`
- Focused changes per branch
- Descriptive commit messages

### Documentation
- `docs/workflow.md` - Detailed process documentation
- GitHub issues for tracking work items
- CHANGELOG.md for significant changes

---

## Emacs Configuration Evolution

### Literate Programming Approach
- Converted to org-mode configuration
- `config.org` - Main configuration file
- Improved theme loading
- Fixed path resolution issues

### File Management
- Centralized backup/auto-save files
- Disabled lock files (reduce clutter)

### Launch Commands
- `em` - GUI Emacs (detached from terminal)
- `emacs` - Terminal Emacs (with -nw option)

---

## Custom Utilities (Aug 2025)

### killport Utility
Created `bin/killport` command-line tool:
- Lists all listening ports when run without arguments
- Kills process on specified port with confirmation
- Shows process details (PID, name, user) before killing
- Defaults to Yes for quick workflow
- Aliased as `kp` for convenience

### Pattern: Interactive CLI Tools
- Show information first (list mode)
- Take action with arguments (command mode)
- Confirm before destructive operations
- Smart defaults (Yes for common case)

---

## Memory Protocol Migration (Session: Oct 19-20, 2025)

### What We Discovered
Successfully migrated dotfiles repository to Memory Protocol - demonstrating the pattern in action.

### Migration Pattern Applied
1. **Commit first** - Clean separation: existing work committed before migration
2. **Extract intelligence** - Historical learnings from NEXT_SESSION.md → MEMORY.md
3. **Create lean state** - Current ephemeral state → NOW.md
4. **Delete legacy** - Remove NEXT_SESSION.md after extraction
5. **Single migration commit** - All changes together (create MEMORY.md, create NOW.md, delete NEXT_SESSION.md)

### Key Commits
- `9204067` - Add Memory Protocol and protocol evolution to Claude configuration
- `1c2c4a9` - Migrate to Memory Protocol (MEMORY.md + NOW.md)

### Pattern: Dogfooding Protocols
The dotfiles repository now:
- Documents Memory Protocol in `claude/CLAUDE.md`
- Uses Memory Protocol itself (MEMORY.md + NOW.md)
- Demonstrates the pattern for other projects

### Technical Decisions
- **MEMORY.md structure**: Organized by session/topic (organic growth)
- **NOW.md format**: Lean state handoff (overwritten each close)
- **Migration timing**: After committing other work (clean git history)

### Lessons Learned
- Migration is straightforward: extract → create → delete
- Organic structure > rigid templates (let sections emerge)
- Repository using its own protocols = validation
- Clean commits tell the migration story

---

## Intelligence Compounds Here

**What gets referenced = what matters**
**Unused content fades naturally**
**Proven patterns rise to CLAUDE.md**

Future sessions: Read relevant sections, skip irrelevant history.
