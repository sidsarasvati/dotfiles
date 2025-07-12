# Dotfiles Enhancement Backlog

This document tracks potential enhancements and improvements for the dotfiles repository. Each item includes context, implementation notes, and priority to help AI assistants understand and implement features effectively.

## Quick Fixes

### 1. Git Status Typo ✅
- **Status**: Completed
- **Description**: Fixed typo in git alias: `--procelain` → `--porcelain`
- **Location**: `zsh/aliases.zsh` line 69

## Shell Enhancements

### 2. Smart Git Aliases and Functions
- **Priority**: High
- **Description**: Add sophisticated git operations to improve workflow
- **Implementation**:
  - Create `zsh/functions/git.zsh` with smart functions
  - `git-undo`: Safely undo last commit keeping changes
  - `git-cleanup`: Remove merged local branches
  - `git-recent`: Show branches by last commit time
  - `git-switch`: Interactive branch switcher (requires fzf)
- **Aliases**: `gu` (undo), `gclean` (cleanup), `grec` (recent)
- **Notes**: Add safety prompts for destructive operations

### 3. Developer Productivity Functions
- **Priority**: High
- **Description**: Common tasks that developers do repeatedly
- **Implementation**:
  - `backup()`: Create timestamped backup of files
  - `ports()`: Show what's running on which ports
  - `json()`: Pretty print and validate JSON
  - `cleanup()`: Remove .DS_Store, node_modules, etc.
  - `extract()`: Universal archive extractor
- **Location**: Create `zsh/functions/productivity.zsh`

### 4. Enhanced Prompt Features
- **Priority**: Medium
- **Description**: Make prompt more informative for different contexts
- **Implementation**:
  - Python virtualenv indicator when active
  - Node.js version in Node projects
  - SSH connection indicator
  - AWS profile if set
  - Kubernetes context if configured
- **Location**: Update prompt section in `zsh/.zshrc`

### 5. FZF Integration (Related to Issue #16)
- **Priority**: High
- **Description**: Fuzzy finding for various operations
- **Implementation**:
  - File finder with preview: `ff` command
  - Directory jumper: `fd` command
  - Git branch fuzzy switch: `gb` command
  - History search: Ctrl+R replacement
  - Process killer: `fkill` command
- **Dependencies**: Requires fzf installation

### 6. AI Assistant Integration (Related to Issue #15)
- **Priority**: Medium
- **Description**: Shell functions for AI-powered assistance
- **Implementation**:
  - `ai-commit`: Generate commit messages from staged changes
  - `ai-explain`: Explain command or error message
  - `ai-suggest`: Get command suggestions for task
  - `ai-review`: Quick code review of changes
- **Notes**: Integrate with Claude API

### 7. Smart Directory Navigation
- **Priority**: Medium
- **Description**: Faster directory navigation
- **Implementation**:
  - Frecency-based jumping (like `z` or `autojump`)
  - Project shortcuts: `proj` command
  - Bookmark system: `mark` and `jump` commands
  - Recent directories: `dirs` command with selection
- **Location**: `zsh/functions/navigation.zsh`

## Package Management

### 8. Brewfile Updates
- **Priority**: High
- **Description**: Add essential developer tools
- **Tools to add**:
  - `ripgrep`: Fast search (already referenced in code)
  - `fzf`: Fuzzy finder (for issue #16)
  - `glow`: Markdown viewer (already used in aliases)
  - `jq`: JSON processor
  - `gh`: GitHub CLI (already used)
  - `bat`: Better cat with syntax highlighting
  - `eza`: Modern ls replacement
  - `zoxide`: Smarter cd command
- **Location**: `Brewfile`

## Performance & Architecture

### 9. Shell Performance Optimization
- **Priority**: Low
- **Description**: Optimize zsh startup time
- **Implementation**:
  - Profile startup with `zprof`
  - Lazy load heavy functions
  - Minimal mode for quick shells
  - Benchmark before/after changes
- **Notes**: Document performance tips

### 10. Documentation Improvements
- **Priority**: Low
- **Description**: Better onboarding and reference
- **Implementation**:
  - Interactive `help` command showing custom commands
  - `cheatsheet` command for quick reference
  - Man page style docs for custom functions
  - Tips of the day system
- **Location**: `zsh/functions/help.zsh`

## Emacs Enhancements

### 11. Additional Emacs Features
- **Priority**: Medium
- **Description**: Leverage existing issues for Emacs improvements
- **Related Issues**:
  - #9: GitHub Copilot integration
  - #12: project.el configuration
  - #13: move-file utility
  - #14: Swiper and ripgrep integration
  - #17: Educational documentation

## Infrastructure

### 12. Missing Configuration Files
- **Priority**: Low
- **Description**: Create proper root-level configs
- **Files**:
  - `.zshrc` that sources from `zsh/.zshrc`
  - `.zprofile` for login shell configuration
  - Consider `.zlogin` and `.zlogout` if needed

## Implementation Guidelines

When implementing items from this backlog:

1. **Test First**: Ensure changes work in fresh shell
2. **Document**: Add comments explaining non-obvious code
3. **Backwards Compatible**: Don't break existing functionality
4. **Cross-Platform**: Test on macOS primarily, Linux when possible
5. **Modular**: Keep functions in appropriate files
6. **Commit Message**: Reference this backlog item number

## Priority Levels

- **High**: Significantly improves daily workflow
- **Medium**: Nice to have, enhances experience
- **Low**: Cosmetic or minor improvements

## Notes for AI Assistants

- This repository uses direct symlinks managed by `install.sh`
- Primary platform is macOS (Darwin)
- No external dependencies for core shell config
- Emacs config uses literate programming (org-mode)
- Follow style guide in `CLAUDE.md`
- Create feature branches for each enhancement