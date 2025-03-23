# Dotfiles Enhancement Workflow

## Overview
This document outlines the workflow for making structured, incremental improvements to the dotfiles repository. Each enhancement should be made on a separate branch to maintain a clean commit history and allow for proper testing before integration.

## Workflow Steps

1. **Create a branch for each feature or enhancement**
   ```bash
   git checkout -b feature/name-of-enhancement
   ```

2. **Make focused changes related to a single enhancement**
   - Keep changes small and focused
   - Test changes thoroughly before committing
   - Follow established code style from CLAUDE.md

3. **Commit changes with descriptive messages**
   ```bash
   git commit -m "Add/Update/Fix: Concise description of change"
   ```

4. **Merge back to master when ready**
   ```bash
   git checkout master
   git merge feature/name-of-enhancement
   ```

## Planned Enhancements

### Emacs Configuration
- [x] Convert monolithic .emacs to literate programming style (complete)
- [x] Fix PATH configuration in env.zsh (complete)
- [x] Setup Emacs terminal/GUI launch commands (complete)
- [x] Enable show-paren-mode (complete)
- [ ] Set default frame size with 9:16 aspect ratio (branch: `feature/frame-size`)
- [ ] Configure Magit for Git integration (branch: `feature/magit-integration`)
- [ ] Move all README files to org-mode format (branch: `feature/org-readme`)
- [ ] Implement move-file utility (branch: `feature/move-file-util`)

### Shell Configuration
- [ ] Organize aliases by category (branch: `feature/organize-aliases`)
- [ ] Add more utility functions (branch: `feature/shell-utils`)
- [ ] Improve PATH management (branch: `feature/path-management`)

### Git Configuration
- [ ] Add useful Git aliases (branch: `feature/git-aliases`)
- [ ] Configure Git templates (branch: `feature/git-templates`)

### Documentation
- [ ] Improve main README with setup instructions (branch: `feature/improve-readme`)
- [ ] Add troubleshooting guide (branch: `feature/troubleshooting-guide`)
- [ ] Document custom functions and aliases (branch: `feature/document-functions`)

## Branch Naming Conventions
- `feature/` - New features or enhancements
- `fix/` - Bug fixes
- `docs/` - Documentation updates
- `refactor/` - Code refactoring without functionality changes

## Testing Changes
Before merging any branch, test changes thoroughly:
1. For Emacs changes: Test in both terminal and GUI modes
2. For shell changes: Test in new shell sessions
3. For git changes: Test with actual repositories

## Resources for Inspiration
- Literate programming: https://github.com/hrs/dotfiles
- Org-mode documentation: https://orgmode.org/
- Magit documentation: https://magit.vc/
- Swift mode documentation: https://github.com/swift-emacs/swift-mode