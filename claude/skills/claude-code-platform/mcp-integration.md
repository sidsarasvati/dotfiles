# MCP Integration Guide

Complete guide to Model Context Protocol integration in Claude Code.

## What is MCP?

**Model Context Protocol (MCP)** is Anthropic's standard for connecting Claude to external data sources and services. It enables Claude Code to:

- Access external databases and APIs
- Integrate with third-party tools (Linear, Jira, Google Calendar, etc.)
- Read from custom data sources
- Execute business logic through external services

## MCP Tool Naming Convention

All MCP tools follow this pattern:

```
mcp__[server-name]__[function-name]
```

**Examples**:
- `mcp__linear__list_issues` - List issues from Linear
- `mcp__google-calendar__create-event` - Create Google Calendar event
- `mcp__stripe__list_subscriptions` - List Stripe subscriptions

**Recognition**: Any tool starting with `mcp__` is an MCP integration.

## Common MCP Integrations

### Linear (Project Management)
Tools available: `mcp__linear__*`

**Common operations**:
- `list_teams` - Get all teams
- `list_cycles` - Get all cycles
- `list_issues` - List issues with filters
- `get_issue` - Get specific issue details
- `create_issue` - Create new issue
- `update_issue` - Update issue fields
- `list_my_issues` - Get issues assigned to authenticated user
- `list_projects` - Get all projects
- `create_comment` - Add comment to issue
- `search_documentation` - Search Linear docs

**Usage pattern**:
```
Always use mcp__linear__get_issue BEFORE updating issues
Never fabricate ticket data - fetch it first
Verify status changes with actual Linear state
```

### Google Calendar
Tools available: `mcp__google-calendar__*`

**Common operations**:
- `list-calendars` - Get all calendars
- `list-events` - List events from calendars
- `create-event` - Create new event
- `update-event` - Modify existing event
- `delete-event` - Remove event
- `search-events` - Search for events
- `get-freebusy` - Check availability
- `get-current-time` - Get time in calendar's timezone

**Usage pattern**:
```
Always check ALL calendars (primary + work + personal)
Include attendees list (especially user themselves)
Enable conferenceData for video meetings
Use proper timezone handling
```

### Stripe (Payments)
Tools available: `mcp__stripe__*`

**Common operations**:
- `list_subscriptions` - Get subscription list
- `retrieve_balance` - Get account balance
- `list_customers` - List customers
- `list_products` - Get products

### App Store Connect
Tools available: `mcp__appstore-connect__*`

**Common operations**:
- `test_connection` - Verify connection
- `list_apps` - Get all apps
- `get_revenue_metrics` - Revenue data
- `get_subscription_metrics` - Subscription analytics

### Atlassian/Confluence
Tools available: `mcp__atlassian__*`

**Common operations**:
- `getAccessibleAtlassianResources` - List resources
- `getConfluenceSpaces` - Get spaces
- `getPagesInConfluenceSpace` - List pages

### Playwright (Browser Testing)
Tools available: `mcp__playwright__*`

**Purpose**: Automated browser testing
**Used by**: QA subagent

## Configuring MCP Servers

### Configuration Location

MCP servers configured in:
- Global: `~/.claude/.mcp.json` or via settings
- Project: `.mcp.json` in project root

### Configuration Format

```json
{
  "mcpServers": {
    "server-name": {
      "command": "path/to/server",
      "args": ["arg1", "arg2"],
      "env": {
        "API_KEY": "value"
      }
    }
  }
}
```

### CLI Configuration

```bash
# List configured MCP servers
claude mcp list

# Add new MCP server
claude mcp add server-name

# Remove MCP server
claude mcp remove server-name

# Test MCP connection
claude mcp test server-name
```

## Using MCP Tools

### Discovery

MCP tools appear in system prompt automatically when configured. Look for tool names starting with `mcp__`.

### Invocation

Call MCP tools like any other tool:

```
Tool: mcp__linear__get_issue
Parameters: {
  "issue_id": "SID-123"
}
```

### Permission System

MCP tools follow same permission rules as built-in tools:
- Can be added to auto-approved list in settings
- Can be blocked if needed
- Permission patterns apply

**Example approval**:
```json
{
  "allowedTools": [
    "mcp__linear__list_issues",
    "mcp__linear__get_issue",
    "mcp__google-calendar__list-events"
  ]
}
```

## MCP vs Built-in Tools

**Use MCP when**:
- Need external data (APIs, databases)
- Integrating third-party services
- Accessing business logic
- Custom tooling specific to organization

**Use Built-in Tools when**:
- File operations (Read/Write/Edit)
- Local command execution (Bash)
- Code search (Grep/Glob)
- Web research (WebFetch/WebSearch)

**Prefer MCP fetch over WebFetch**:
MCP-provided web fetch tools often have fewer restrictions than built-in WebFetch. If available, use them.

## Best Practices

### Data Verification
```
❌ Never assume MCP data
✓ Always fetch before operating
✓ Verify state before changes
✓ Confirm operations succeeded
```

### Error Handling
```
MCP calls may fail (network, auth, rate limits)
- Handle errors gracefully
- Provide alternatives
- Ask user for help if blocked
```

### Rate Limiting
```
Some MCP services have rate limits
- Batch operations when possible
- Cache results within session
- Don't hammer APIs with repeated calls
```

### Security
```
Never log API keys or tokens
Be careful with sensitive data
Follow service-specific security practices
Respect data privacy
```

## Common MCP Patterns

### Linear Workflow
```
1. Fetch current state: mcp__linear__get_issue
2. Verify fields and status
3. Make changes: mcp__linear__update_issue
4. Confirm success
```

### Calendar Workflow
```
1. Check all calendars: mcp__google-calendar__list-calendars
2. Get events: mcp__google-calendar__list-events (multiple calendars)
3. Check conflicts: mcp__google-calendar__get-freebusy
4. Create event: mcp__google-calendar__create-event (with attendees + video)
```

### Multi-Service Integration
```
Combine MCP + built-in tools:
1. Read local file (Read tool)
2. Fetch external data (MCP tool)
3. Process and merge (local logic)
4. Update service (MCP tool)
5. Save locally (Write tool)
```

## Troubleshooting MCP

### Connection Issues
```
Problem: "MCP server not responding"
Solutions:
- Check server is configured: claude mcp list
- Test connection: claude mcp test server-name
- Verify credentials in env vars
- Restart Claude Code to reload MCP
```

### Authentication Failures
```
Problem: "Unauthorized" or "Invalid token"
Solutions:
- Check API keys in MCP config
- Verify token hasn't expired
- Confirm permissions for API user
- Regenerate credentials if needed
```

### Tool Not Found
```
Problem: "Unknown tool mcp__service__function"
Solutions:
- Verify MCP server is configured
- Check server name matches exactly
- Restart Claude Code to load new MCP
- List available tools to confirm
```

### Rate Limiting
```
Problem: "Too many requests"
Solutions:
- Batch operations
- Add delays between calls
- Cache results
- Use pagination
```

## Advanced MCP Features

### Custom MCP Servers

Build custom MCP servers for:
- Internal APIs
- Database access
- Custom business logic
- Proprietary tools

**Resources**:
- MCP specification: [Anthropic docs]
- Server templates: [GitHub examples]
- Community servers: [MCP marketplace]

### MCP in Subagents

Subagents can use MCP tools:
```json
{
  "agents": {
    "custom-agent": {
      "description": "Agent with MCP access",
      "tools": ["Read", "mcp__linear__*"],
      "prompt": "..."
    }
  }
}
```

### MCP + Hooks

Hooks can intercept MCP calls:
```
PreToolUse: Validate before MCP call
PostToolUse: Process MCP responses
```

## MCP Security Considerations

**API Keys**:
- Store in environment variables
- Never commit to version control
- Rotate regularly
- Use least-privilege access

**Data Privacy**:
- MCP servers may access sensitive data
- Only configure trusted servers
- Review permissions before enabling
- Audit MCP usage

**Network Security**:
- MCP communicates over network
- Use HTTPS when available
- Be aware of data leaving localhost
- Consider firewall rules

## Useful MCP Commands

```bash
# List all MCP servers
claude mcp list

# Add server from config
claude mcp add --config /path/to/config.json

# Test specific server
claude mcp test linear

# View server details
claude mcp info linear

# Remove server
claude mcp remove linear

# Load MCP from specific config
claude --mcp-config /path/to/mcp.json

# Use strict MCP config (ignore others)
claude --strict-mcp-config --mcp-config /path/to/mcp.json
```

## Example: Complete Linear Integration

```
1. List teams:
   mcp__linear__list_teams

2. Get current cycle:
   mcp__linear__list_cycles (filter: active)

3. List issues in cycle:
   mcp__linear__list_issues (cycle_id: X)

4. Get issue details:
   mcp__linear__get_issue (issue_id: "SID-123")

5. Update issue:
   mcp__linear__update_issue (
     issue_id: "SID-123",
     status: "In Progress",
     assignee: "user-id"
   )

6. Add comment:
   mcp__linear__create_comment (
     issue_id: "SID-123",
     body: "Update text"
   )
```

## Example: Complete Calendar Integration

```
1. List calendars:
   mcp__google-calendar__list-calendars

2. Get events from multiple calendars:
   mcp__google-calendar__list-events (
     calendarId: ["primary", "work@example.com"],
     timeMin: "2025-10-16T00:00:00",
     timeMax: "2025-10-16T23:59:59"
   )

3. Check availability:
   mcp__google-calendar__get-freebusy (
     calendars: [{id: "primary"}, {id: "work@example.com"}],
     timeMin: "2025-10-16T14:00:00",
     timeMax: "2025-10-16T15:00:00"
   )

4. Create event with video:
   mcp__google-calendar__create-event (
     calendarId: "primary",
     summary: "Team Meeting",
     start: "2025-10-16T14:00:00",
     end: "2025-10-16T15:00:00",
     attendees: [{email: "user@example.com"}],
     conferenceData: {
       createRequest: {
         requestId: "unique-id",
         conferenceSolutionKey: {type: "hangoutsMeet"}
       }
     }
   )
```

---

*MCP Integration guide for Claude Code*
*Version 1.0 - Oct 16, 2025*
