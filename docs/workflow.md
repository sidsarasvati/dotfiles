# Dotfiles Development Workflow

## Overview
This document outlines the workflow for making structured, incremental improvements to the dotfiles repository using GitHub issues and a lean development process.

## Issue-Based Workflow

### Creating Issues

To create GitHub issues from the command line:

```bash
cat > /tmp/issue_body.md << EOF
## Problem
[Description of the problem]

## Details
[Detailed explanation with examples]

## Proposed Solution
[How to fix the issue]

## Acceptance Criteria
- [ ] [Criteria 1]
- [ ] [Criteria 2]
EOF

gh issue create --title "Your issue title" --body "$(cat /tmp/issue_body.md)"
rm /tmp/issue_body.md
```

### Implementing Changes

1. **Create a branch for each issue**
   ```bash
   gh issue view ISSUE_NUMBER
   git checkout -b fix/ISSUE_NUMBER-short-description
   ```

2. **Make focused changes related to a single enhancement**
   - Keep changes small and focused
   - Test changes thoroughly before committing
   - Follow established code style from CLAUDE.md

3. **Commit changes with descriptive messages**
   ```bash
   git commit -m "Fix #ISSUE_NUMBER: Concise description of change"
   ```

4. **Update documentation**
   - Update CLAUDE.md with technical details if necessary
   - Add an entry to CHANGELOG.md for significant changes
   - Update any relevant README files

5. **Create a pull request**
   ```bash
   gh pr create --title "Fix #ISSUE_NUMBER: Short description" --body "Fixes #ISSUE_NUMBER

   ## Changes
   - [List of changes]
   
   ## Testing
   - [How the changes were tested]"
   ```

6. **Merge to master when ready**
   ```bash
   gh pr merge ISSUE_NUMBER --merge
   ```

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

## Planned Enhancements

### Emacs Configuration
- [x] Convert monolithic .emacs to literate programming style (complete)
- [x] Fix PATH configuration in env.zsh (complete)
- [x] Setup Emacs terminal/GUI launch commands (complete)
- [x] Enable show-paren-mode (complete)
- [x] Set default frame size with 9:16 aspect ratio (complete)
- [ ] Configure Magit for Git integration (issue: #TBD)
- [ ] Move all README files to org-mode format (issue: #TBD)
- [ ] Implement move-file utility (issue: #TBD)

### Shell Configuration
- [ ] Organize aliases by category (issue: #TBD)
- [ ] Add more utility functions (issue: #TBD)
- [ ] Improve PATH management (issue: #TBD)

### Git Configuration
- [ ] Add useful Git aliases (issue: #TBD)
- [ ] Configure Git templates (issue: #TBD)

### Documentation
- [ ] Improve main README with setup instructions (issue: #TBD)
- [ ] Add troubleshooting guide (issue: #TBD)
- [ ] Document custom functions and aliases (issue: #TBD)

## Resources for Inspiration
- Literate programming: https://github.com/hrs/dotfiles
- Org-mode documentation: https://orgmode.org/
- Magit documentation: https://magit.vc/
- Swift mode documentation: https://github.com/swift-emacs/swift-mode