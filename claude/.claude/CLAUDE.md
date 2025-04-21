# Claude Code Global Memory

## My Engineering Philosophy
- Keep it simple
- Think from first principles. Break down the problem. Grok the fundamentals. 
- Ask me questions for important decisions. Simple yes/no. I'll answer with guidnace.
- Always try to run the code you are writing before going too far. DO the OOB testing using commands. Make sure it works. 

## Package Management

### Corepack

Corepack is a built-in Node.js tool (since v16.9.0) that manages package manager versions.

- **Enable globally** with: `corepack enable`
- **Prepare specific version** with: `corepack prepare pnpm@10.2.1 --activate`
- **No repo changes**: This is a system-level setting that doesn't modify projects
- **Reads from package.json**: Uses `"packageManager": "pnpm@10.2.1"` field
- **Automatic switching**: Changes package manager version when you change directories
- **Comparison to NVM**: Corepack is for package managers; NVM is for Node.js versions

### PNPM

- **Installation**: `npm install -g pnpm` or via Corepack
- **Workspaces**: Uses `pnpm-workspace.yaml` for monorepo management
- **Speed**: Faster than npm due to symlink-based node_modules
- **Disk space**: Saves disk space with content-addressable storage

## Git Workflows

### Managing Upstream Repositories

- **Rename original remote**: `git remote rename origin upstream`
- **Add organization remote**: `git remote add origin git@github.com:ORG/REPO.git`
- **Push to organization**: `git push -u origin master`
- **Pull upstream changes**: `git fetch upstream && git merge upstream/master`
- **Push updates to origin**: `git push origin master`