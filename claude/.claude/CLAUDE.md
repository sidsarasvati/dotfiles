# Claude Code Global Memory

## My Engineering Philosophy
- Keep it simple
- Think from first principles. Break down the problem. Grok the fundamentals. 
- Ask me questions for important decisions. Simple yes/no. I'll answer with guidnace.
- Always try to run the code you are writing before going too far. DO the OOB testing using commands. Make sure it works.
- Fix the problem, not the blame. Find root causes, not just symptoms.

## Debugging Philosophy

### Core Principles
- **Fix the Problem, Not the Blame** - Focus on understanding the actual issue, not who/what caused it
- **Don't Panic** - Stay calm and methodical
- **Don't think "that can't happen"** - It clearly can and has. Accept reality and investigate
- **Find root causes** - Not just this particular appearance of the problem

### Debugging Process
1. **Start with accurate data** - Check warnings, verify observations, examine actual logs/responses
2. **Make bugs reproducible** - Ideally with a single command
3. **Visualize your data** - Use debugger tools, logs, pen & paper
4. **Trace execution** - Know what happens before and after
5. **Rubber duck** - Explain the bug out loud
6. **Process of elimination** - Your code first, then libraries, OS, compiler last

### Key Reminders
- **"select" Isn't Broken** - The bug is probably in your code, not the platform/library
- **Don't Assume It—Prove It** - Verify every assumption with data

### Debugging Checklist
- Is this a symptom or the root cause?
- What would I tell a coworker about this bug?
- Are the unit tests complete enough?
- Does this bug exist elsewhere in the system?
- What do the actual logs/responses show (not what I expect)?

## Pragmatic Programming Principles

### Core Philosophy
- **Care about your craft** - Take pride in code quality
- **Think about your work** - Don't code on autopilot
- **Provide options, not excuses** - When blocked, present alternatives
- **Don't live with broken windows** - Fix small issues before they compound
- **DRY - Don't Repeat Yourself** - Single source of truth for everything

### Development Approach
- **Use tracer bullets** - Get end-to-end working first, then enhance
- **Prototype to learn** - Spike solutions to reduce unknowns
- **Design with contracts** - Define clear interfaces and expectations
- **Crash early** - Fail fast with clear errors
- **Don't program by coincidence** - Understand why code works

### Code Quality
- **Decouple and orthogonalize** - Changes in one area shouldn't break others
- **Configure, don't integrate** - Use environment variables and config files
- **Refactor early, refactor often** - Clean as you go
- **Design to test** - If it's hard to test, the design is wrong
- **Don't use wizard code you don't understand** - No copy-paste without comprehension

### Problem Solving
- **Don't think outside the box - find the box** - Understand actual constraints
- **Some things are better done than described** - Bias toward action
- **Estimate to avoid surprises** - Break down and size work
- **Iterate the schedule with the code** - Plans must evolve with reality

### Team Practices
- **Use a project glossary** - Consistent terminology prevents confusion
- **Build documentation in, don't bolt it on** - Document while coding
- **Automate everything** - One-command builds, tests, deploys
- **Sign your work** - Take ownership and pride

### When Debugging
- **Fix the problem, not the blame**
- **"select" isn't broken** - It's probably your code
- **Don't assume it - prove it** - Verify with data
- **Find bugs once** - Add tests to prevent recurrence## Debugging Philosophy

### Core Principles
- **Fix the Problem, Not the Blame** - Focus on understanding the actual issue, not who/what caused it
- **Don't Panic** - Stay calm and methodical
- **Don't think "that can't happen"** - It clearly can and has. Accept reality and investigate
- **Find root causes** - Not just this particular appearance of the problem

### Debugging Process
1. **Start with accurate data** - Check warnings, verify observations, examine actual logs/responses
2. **Make bugs reproducible** - Ideally with a single command
3. **Visualize your data** - Use debugger tools, logs, pen & paper
4. **Trace execution** - Know what happens before and after
5. **Rubber duck** - Explain the bug out loud
6. **Process of elimination** - Your code first, then libraries, OS, compiler last

### Key Reminders
- **"select" Isn't Broken** - The bug is probably in your code, not the platform/library
- **Don't Assume It—Prove It** - Verify every assumption with data

### Debugging Checklist
- Is this a symptom or the root cause?
- What would I tell a coworker about this bug?
- Are the unit tests complete enough?
- Does this bug exist elsewhere in the system?
- What do the actual logs/responses show (not what I expect)?

## AI Code Collaboration

1. AI code agents will be coding
   1. Use structure that helps
   2. Context is limited. Keep only useful code and documentation.
2. Sid is the sole owner and benevolent dictator. Code is written for me (Sid) so it:
   - Makes sense in future
   - Is easy to debug
   - Helps me learn from it
3. **Server Management**: NEVER start servers directly - Sid will always run them to pipe logs and control execution
   - Don't use `npm run dev`, `npm run mission`, or similar commands
   - Ask Sid to start servers when needed
   - This allows proper log piping and debugging visibility

## Claude Code Configuration
- **Custom Commands**: Check `.claude/commands/` for slash commands
- **Hook System**: PreToolUse/PostToolUse in `.claude/hooks/`
- **Permissions**: Use `/permissions` to manage tool access
- **Settings**: Configure in `.claude/settings.json`

## Subagents
- **Location**: `.claude/agents/` directory  
- **When to use**: Complex multi-step tasks, parallel analysis, heavy research
- **Preserve context**: Delegate to subagents when main context is precious
- **Create as needed**: Not all projects have subagents - create when patterns emerge
- **Usage**: Use `Task` tool with `subagent_type` parameter

## GitHub Integration
- **`gh` CLI**: Use for PRs, issues, and GitHub operations
- **PR Creation**: `gh pr create` with proper formatting
- **Never use CANCELLED**: Mark as Done with resolution comment

## Performance Reminders for Claude
- **Long sessions**: Consider using `/clear` if context gets cluttered
- **Complex tasks**: Use subagents to preserve main context
- **Visual feedback**: Ask Sid for screenshots when iterating UI
- **Think mode**: Add "think" when you need deeper analysis
- **Test incrementally**: Run code frequently, don't wait

## Python Development
- **ALWAYS create a virtual environment** (.venv) for Python projects before installing dependencies
- Use `python -m venv .venv` to create it
- Activate with `source .venv/bin/activate` (Mac/Linux) or `.venv\Scripts\activate` (Windows)



## Personal Information
- **Name**: Sid Sarasvati
- **Location**: Boston, MA
- **LinkedIn**: https://www.linkedin.com/in/sidsarasvati/

## Web Development Stack
- **Frontend**: SPA with React, Vite, and Tailwind CSS
- **Language**: TypeScript (strict mode whenever possible)
- **Backend**: Only when necessary (prefer client-side solutions)
- **Testing**: Write debug-friendly code with automated testing capabilities
- **Principle**: Code should be testable by AI agents without human intervention

## Deployment Preferences
- **Primary**: Cloudflare Pages/Workers
- **Secondary**: DigitalOcean (when needed)
- **Avoid**: Docker (complicates AI agent coding and debugging)

## Project Documentation Workflow
- **All docs in `docs/` folder**: Planning, tech docs, and trackers organized by project scale
- **Start with PRFAQ**: Named as `prfaq001_$projectname.md`
- **Break into RFCs**: `rfc001_$feature.md`, `rfc002_$feature.md` for intelligently scoped work
- **RFC Trackers**: `rfc001_tracker.md` for actual task lists and progress tracking
- **Document learnings**: Update RFCs/trackers as plans change
- **Long-term tech docs**: Store in `docs/tech/` (API docs, tech debt, architectural decisions)
- **Workflow**: PRFAQ → RFCs → Trackers → Code → Update docs with learnings
