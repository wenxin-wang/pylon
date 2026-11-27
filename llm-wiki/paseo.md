---
description: Paseo behaviours that surprised agents: worktree base, file paths, fork, MCP tools, API auth
category: agents
sources:
  - https://github.com/getpaseo/paseo/releases/tag/v0.17.3
  - https://github.com/getpaseo/paseo/blob/v0.15.4/docs/development.md#L395-L408
  - https://github.com/getpaseo/paseo/pull/2848
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/screens/new-workspace-picker-item.ts#L160-L172
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/utils/worktree.ts#L1838-L1905
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/resolve-worktree-creation-intent.ts#L116-L125
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/utils/checkout-git.ts#L1601-L1630
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/file-explorer/preview-target.ts#L60-L108
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/utils/assistant-image-source.ts#L9-L35
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/providers/provider-image-output.ts#L31-L48
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/providers/codex-app-server-agent.ts#L3288-L3291
  - https://github.com/getpaseo/paseo/blob/v0.17.6/packages/app/src/hooks/use-fork-agent.ts#L93-L193
  - https://github.com/getpaseo/paseo/blob/v0.17.6/packages/server/src/server/agent/activity-curator.ts#L239-L388
  - https://github.com/getpaseo/paseo/blob/v0.17.6/packages/server/src/server/agent/prompt-attachments.ts#L7-L38
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/providers/claude/rewind.ts#L123-L140
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/providers/codex/rewind.ts#L44-L112
  - https://github.com/getpaseo/paseo/blob/v0.15.4/CHANGELOG.md#L198-L213
  - https://github.com/getpaseo/paseo/blob/v0.15.4/docs/connected-hosts.md
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/runtime/host-runtime.ts#L2452-L2500
  - https://github.com/getpaseo/paseo/blob/v0.15.4/SECURITY.md#L49-L55
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/config.ts#L476-L490
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/auth.ts#L81-L85
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/auth.ts#L136-L155
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/providers/opencode/v2/capabilities.ts
  - https://github.com/getpaseo/paseo/blob/v0.15.4/public-docs/mcp.md
  - https://github.com/getpaseo/paseo/blob/v0.15.4/public-docs/orchestration.md
  - https://github.com/getpaseo/paseo/blob/v0.15.4/docs/permissions.md#L32-L55
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/tools/paseo-tools.ts#L873-L893
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/tools/paseo-tools.ts#L1589-L1660
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/tools/paseo-tools.ts#L1945-L1975
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/tools/paseo-tools.ts#L3215-L3280
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/runtime-mcp-config.ts#L29-L58
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/bootstrap.ts#L1692-L1700
  - https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/agent-stream/view.tsx#L1699-L1731
  - https://reactnative.dev/docs/text#selectable
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/paseo/daemon-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/container-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/paseo-connected-hosts-review-20261115.md
checked: 2026-11-27
---

# Paseo

Paseo (https://github.com/getpaseo/paseo) is the app and daemon the human uses to run
Claude Code and Codex agents. Checked against source at v0.15.4 (2026-11-10, the
deployed version) and v0.17.3 (2026-11-23 01:46 UTC+8); each fact holds in both unless
noted. v0.17.4 followed on 2026-11-24 and does not touch the checked code paths beyond
small refactors. Links below point at the v0.15.4 tag, except the fork section,
rechecked at v0.17.6 (2026-11-27). What uraj does around Paseo
(daemon container, broker, mounts) is in uraj's own docs, linked where needed.

## New worktrees from the app start from origin, not local main

- The app's New workspace picker defaults the base to the **upstream of the project
  checkout's current branch** (`refs/remotes/origin/main`; the local branch if it has no
  upstream), "because branching off the local ref silently carries unpushed commits into
  the new workspace"
  ([picker code](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/screens/new-workspace-picker-item.ts#L160-L172),
  [docs](https://github.com/getpaseo/paseo/blob/v0.15.4/docs/development.md#L395-L408),
  PR 2848, in every release since v0.3.0). The picker offers a second row "main (local)"
  when local and origin differ, or when it can't tell.
- The daemon then fetches that one branch (15 s timeout, falls back to the cached ref)
  and runs `git worktree add -b <new> --no-track refs/remotes/origin/main`, so the new
  branch has no upstream and its reflog says `Created from refs/remotes/origin/main`
  (seen in a real workspace on 2026-10-26;
  [worktree.ts](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/utils/worktree.ts#L1838-L1905)).
  Commits on local `main` that were never pushed are absent; uncommitted changes are
  never copied.
- **The agent-facing path defaults the other way** (read from source, not run). MCP
  `create_workspace` and the CLI `--base` pass the base as a plain name; with none given
  the daemon uses the repo default branch name, and a plain name resolves local-first
  (`refs/heads/main`, then `refs/remotes/origin/main`)
  ([intent](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/resolve-worktree-creation-intent.ts#L116-L125),
  [default branch](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/utils/checkout-git.ts#L1601-L1630)).
  `origin/main` or a `refs/...` name is taken exactly. The fetch happens when the
  resolved ref is a remote one, so a plain name with no local branch is fetched too.
- Diagnose a missing commit: `git rev-list --left-right --count main...origin/main` in
  the main checkout and `git reflog` of the worktree branch. Fix: push `main`, or pick
  "main (local)" when creating, or `git merge main` in the worktree (linked worktrees
  share refs).

## File paths are passed through, never remapped

- An absolute path in an assistant message (link, inline-code path, markdown image) makes
  the app ask the daemon to read exactly that path (`file_rpc`; relative paths are taken
  from the workspace root); the daemon reads it in its own filesystem, and may read any
  file its process can read
  ([preview-target.ts](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/file-explorer/preview-target.ts#L60-L108),
  [assistant-image-source.ts](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/utils/assistant-image-source.ts#L9-L35),
  [SECURITY.md](https://github.com/getpaseo/paseo/blob/v0.15.4/SECURITY.md#L55)).
  v0.17.3 has no path mapping for file reads either.
- So a screenshot an agent saves in a `/tmp` that only its own container sees (private
  tmpfs) is "not found" for the daemon and the app. A shared location must have the
  **same absolute path on both sides**; the agent's cwd already works this way (Paseo
  passes host paths).
- The other direction: for Codex, Paseo writes an image attachment to
  `os.tmpdir()/paseo-attachments-*/<sha256>.<ext>` in the daemon and passes that path as
  `localImage`; Codex must be able to open the same path
  ([code](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/providers/provider-image-output.ts#L31-L48)).
  The daemon's `TMPDIR` therefore decides where those files go.
- uraj's choice of daemon `TMPDIR` and mounts:
  [daemon-guide, image attachments](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/paseo/daemon-guide.md#image-attachments)
  and [container-guide](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/container-guide.md).

## "Fork" is a text summary; Rewind forks the session

- Fork in a new tab or new workspace (the split icon under a reply) does **not** copy
  the provider session. The app asks the daemon for a `<chat-history-summary>` text
  attachment and puts it in a draft for a fresh agent with the same provider, mode,
  model, thinking option and cwd (a new workspace gets the cwd moved into it)
  ([use-fork-agent.ts](https://github.com/getpaseo/paseo/blob/v0.17.6/packages/app/src/hooks/use-fork-agent.ts#L93-L193)).
- How the summary is built (v0.17.6,
  [activity-curator.ts](https://github.com/getpaseo/paseo/blob/v0.17.6/packages/server/src/server/agent/activity-curator.ts#L239-L388)):
  - It takes the timeline up to the chosen reply, or up to now when forked from a
    running turn; a tool call that spans the cut is copied as still running.
  - Only three kinds go in, one line each: `[User] <text>`, `[Assistant] <text>` (the
    visible reply, in full), and tool calls as `[<tool name>] <summary>` (the summary,
    usually the command, at most 200 characters). Tool output, reasoning, todos,
    errors and compactions are left out.
  - An English header wraps it: `Chat history from a previous Paseo agent.`, then
    `Source agent: <title>` and `Source directory: <cwd>`.
  - The whole text is capped at 100,000 characters: the oldest entries are dropped and
    `[Earlier chat history omitted]` is put first; if even the newest entry does not
    fit, its middle is cut to `[Message truncated]`.
- When the human sends the draft, the summary and the typed text go into the **same
  first user message**, summary first
  ([prompt-attachments.ts](https://github.com/getpaseo/paseo/blob/v0.17.6/packages/server/src/server/agent/prompt-attachments.ts#L7-L38)).
  The new agent starts a new Claude Code session (new session id, SessionStart hooks
  and CLAUDE.md run again; seen in a transcript on 2026-11-27). It sees which commands
  ran but not their results, and its earlier replies only as quoted text inside a
  user message, not as its own turns. One effect: an agent forked from a Chinese
  session that slips into English has no Chinese turns of its own to pull it back,
  and may stay in English for the rest of the session (seen 2026-11-27).
- Rewind (conversation or both, at a user message) is the real fork, and it keeps the
  **same Paseo agent**, rebinding it to the fork. Claude: the Agent SDK `forkSession`
  with `upToMessageId` writes a new transcript beside the old one
  ([code](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/providers/claude/rewind.ts#L123-L140)).
  Codex: `thread/fork` (older threads fork, then roll back); the old thread stays on
  disk, and Codex rollback is chat-only, so file edits remain
  ([code](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/providers/codex/rewind.ts#L44-L112)).
  OpenCode supports only rewinding conversation and files together, through its own
  `session.revert`, which is not a fork; ACP providers support no rewind (v0.15.4 and
  v0.17.3).
- The daemon writes the Claude fork, so it needs write access to the transcript folder;
  see uraj's [daemon-guide](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/paseo/daemon-guide.md).

## Connected hosts (0.13.0 and later)

- Added in 0.13.0 (2026-10-24): `paseo host add <target>` makes one daemon (A) register
  others (B) in A's own `hosts.json`; agents on A reach them through MCP tools with
  `serverId`. The app, once connected to A, discovers B automatically and saves it as a
  routed connection in its own host list
  ([doc](https://github.com/getpaseo/paseo/blob/v0.15.4/docs/connected-hosts.md),
  [app discovery](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/runtime/host-runtime.ts#L2452-L2500),
  [changelog](https://github.com/getpaseo/paseo/blob/v0.15.4/CHANGELOG.md#L198-L213)).
- One hop only: a routed session cannot select a second hop, and saving a host grants no
  reverse access (same doc). The app skips discovery on hosts it already reaches by a
  routed connection.
- uraj's detailed review of the routing and authority model:
  [paseo-connected-hosts-review](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/projects/261115-contained-agent-v2/paseo-connected-hosts-review-20261115.md).

## The API has no auth unless a password is set

- The daemon listens on `127.0.0.1:6767` by default
  ([config.ts](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/config.ts#L476-L490)).
  With no password, "anything that can reach the daemon socket can control the daemon";
  loopback is reachable by other local users and some forwarding tools
  ([SECURITY.md](https://github.com/getpaseo/paseo/blob/v0.15.4/SECURITY.md#L49-L55)).
  The HTTP middleware lets every request through when no password is configured
  ([auth.ts](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/auth.ts#L81-L85)).
- Even with a password, a process running as the daemon's user can read
  `$PASEO_HOME/local-credential` (SECURITY.md, same lines). Permissions are daemon-wide
  today and agents use the same authority as a client
  ([permissions.md](https://github.com/getpaseo/paseo/blob/v0.15.4/docs/permissions.md#L32-L55)).
- Any agent that can run the `paseo` CLI or open a socket to the daemon therefore has the
  tool set below without MCP at all
  ([orchestration.md](https://github.com/getpaseo/paseo/blob/v0.15.4/public-docs/orchestration.md):
  the CLI route "does not require enabling tool injection").

## Paseo MCP tools reach every agent and workspace

- Tool injection is off by default (`daemon.mcp.injectIntoAgents`, Settings, Enable
  Paseo tools); when on, each agent gets an MCP URL
  `/mcp/agents?callerAgentId=<id>` with one daemon-wide capability token
  ([mcp.md](https://github.com/getpaseo/paseo/blob/v0.15.4/public-docs/mcp.md),
  [runtime-mcp-config.ts](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/runtime-mcp-config.ts#L29-L58),
  [bootstrap.ts](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/bootstrap.ts#L1692-L1700)).
  The caller id is a query parameter that the caller supplies; the per-provider tool
  policy below is looked up from it, so an agent can step around that policy by sending
  another id or none (read from code, not run).
- With no daemon password the token is not checked at all: `/mcp/agents` accepts every
  request, and the route is served whenever `daemon.mcp.enabled` is on (default `true`),
  even with injection off
  ([auth.ts](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/auth.ts#L136-L155)).
- `create_agent` takes any existing `workspaceId`, so an agent can start a child in
  another project's workspace; `create_workspace` takes a `path`, and an absolute path
  is used as given (no cwd lock outside voice sessions)
  ([resolveScopedCwd](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/tools/paseo-tools.ts#L873-L893),
  [create_agent](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/tools/paseo-tools.ts#L1589-L1660)).
- `send_agent_prompt` accepts any existing `agentId`. `list_pending_permissions` lists
  pending requests of all agents and `respond_to_permission` takes any `agentId` and
  `requestId`, so one agent can approve another's permission request
  ([send](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/tools/paseo-tools.ts#L1945-L1975),
  [permissions](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/server/src/server/agent/tools/paseo-tools.ts#L3215-L3280)).
- Per-provider `paseoTools.disabledTools` (for example `respond_to_permission`,
  `create_agent`, `send_agent_prompt`) removes tools from the catalog; upstream says
  this "is not a security boundary for an agent that can access the host through a
  shell" (mcp.md).

## Permission popups on the phone

- The permission card shows its title and description as plain text: no markdown, no
  links, and no `selectable` prop (React Native's `Text` is not selectable by default,
  [docs](https://reactnative.dev/docs/text#selectable)). The tool input below it is
  selectable but capped at 200 px height
  ([view.tsx](https://github.com/getpaseo/paseo/blob/v0.15.4/packages/app/src/agent-stream/view.tsx#L1699-L1731)).
- Assistant messages and plan cards render through `react-native-enriched-markdown`;
  whether it turns a bare URL into a link was not checked. To give the human a link or a
  path to copy, put it in a normal message before the request, not in the permission
  text.
- This is app source at v0.15.4 and v0.17.3, not tested on a device.
