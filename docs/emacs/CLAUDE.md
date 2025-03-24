# CLAUDE.md - Emacs Documentation Guidelines

## Organization
- All documentation is maintained in org-mode format
- Each document focuses on a specific feature or workflow
- README.org serves as an index to all documentation
- Examples should be provided as executable code blocks where possible
- Cross-references between documents use org-mode links

## Document Structure
- Each document should begin with #+TITLE, #+AUTHOR, #+DESCRIPTION headers
- Use proper heading hierarchy (*, **, ***)
- Include a brief introduction explaining the feature's purpose
- Provide command tables with keybindings
- Include practical workflow examples
- Add tips and customization options at the end

## Code Blocks
- Use `emacs-lisp` as the language for executable examples
- Include `:eval no` for examples that shouldn't be executed directly
- Wrap complex or destructive examples in `#+begin_src` blocks
- Add comments to explain code functionality

## Tables
- Use org-mode's table format (| column1 | column2 |)
- Always include a header row and separator
- For command tables, include command name, keybinding, and description

## Properties and IDs
- Add PROPERTIES drawers with CUSTOM_ID for major sections
- Use these IDs for cross-referencing between documents
- Format IDs as kebab-case (e.g., git-blame)

## Current Documentation
- version-control.org - Complete
- Additional documents planned in issue #17

## Implementation Examples
Example document template:
```
#+TITLE: Feature Name
#+AUTHOR: Sid Sarasvati
#+DESCRIPTION: Brief description
#+STARTUP: overview
#+OPTIONS: toc:2 num:nil

* Main Heading
Introduction text...

** Sub-heading
Content...

*** Command Reference
| Command | Keybinding | Description |
|---------+------------+-------------|
| cmd1    | key1       | desc1       |
```

When updating the documentation collection, remember to:
1. Update README.org with new documents
2. Add cross-links between related topics
3. Follow consistent formatting