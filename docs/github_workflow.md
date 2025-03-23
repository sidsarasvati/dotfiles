# GitHub Workflow

## Creating Issues

To create GitHub issues from the command line, follow these steps:

1. Create a temporary issue file:

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
```

2. Create the issue using the temporary file:

```bash
gh issue create --title "Your issue title" --body "$(cat /tmp/issue_body.md)"
```

3. Clean up the temporary file:

```bash
rm /tmp/issue_body.md
```

## Pull Requests

[To be added]

## Releases

[To be added]