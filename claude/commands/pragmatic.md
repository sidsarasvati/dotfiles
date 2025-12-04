---
description: Pragmatic programming philosophy - debug, solve, ship
allowed-tools: ALL
argument-hint: [problem, bug, or decision]
---

# 🎯 PRAGMATIC APPROACH

Situation: $ARGUMENTS

## ENGINEERING PHILOSOPHY
- Keep it simple
- Think from first principles. Break down the problem. Grok the fundamentals.
- Ask simple yes/no questions for important decisions
- Always try to run code before going too far - OOB testing with commands
- Fix the problem, not the blame. Find root causes, not symptoms.

## DEBUGGING APPROACH

### Core Principles
- **Fix the Problem, Not the Blame** - Focus on understanding the actual issue
- **Don't Panic** - Stay calm and methodical
- **Don't think "that can't happen"** - It clearly can and has. Accept reality
- **Find root causes** - Not just this particular appearance

### Process
1. **Start with accurate data** - Check warnings, verify observations, examine actual logs/responses
2. **Make bugs reproducible** - Ideally with a single command
3. **Visualize your data** - Use debugger tools, logs, pen & paper
4. **Trace execution** - Know what happens before and after
5. **Rubber duck** - Explain the bug out loud
6. **Process of elimination** - Your code first, then libraries, OS, compiler last

### Key Reminders
- **"select" Isn't Broken** - The bug is probably in your code, not the platform/library
- **Don't Assume It—Prove It** - Verify every assumption with data

## PRAGMATIC PROGRAMMING

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

## DEBUGGING CHECKLIST
- Is this a symptom or the root cause?
- What would I tell a coworker about this bug?
- Are the unit tests complete enough?
- Does this bug exist elsewhere in the system?
- What do the actual logs/responses show (not what I expect)?

---
*From your Pragmatic Programming philosophy*