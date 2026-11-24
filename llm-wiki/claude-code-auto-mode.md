---
description: Claude Code auto mode: when a chat approval clears a classifier block, and other ways to unblock
category: agents
sources:
  - https://code.claude.com/docs/en/permission-modes
  - https://code.claude.com/docs/en/auto-mode-config
  - https://github.com/anthropics/claude-code/blob/main/CHANGELOG.md
checked: 2026-11-24
---

# Claude Code auto mode: getting past a classifier block

In auto mode a second model, the classifier, reviews each action that no allow, ask, or
deny rule settles. Reads and edits inside the working directory are approved without it
(except writes to protected paths), but in sessions with server-side classifier review
(the default on a direct API connection) edits and read-only commands still wait for that
review and can be blocked. Interactive terminal and VS Code sessions start in auto mode
when no mode is configured: since v2.1.283 per the docs, while the changelog lists the
change for every plan under 2.1.284. `permissions.defaultMode` and `--permission-mode`
override it.

## What the classifier sees

In the requests Claude Code sends itself: user messages, Claude's tool calls (except
read-only lookups), and CLAUDE.md. Tool results are stripped, so hostile text in a file
or page can't manipulate it directly; the flip side is that a fact such as "this repo is
public" reaches it only if a user message states it. A block returns a reason to Claude,
a broad category such as `[Destructive or Unwanted Actions]`.

## Approvals in chat

Rules apply in this order (auto-mode-config, "Override the block and allow rules"):

1. `hard_deny`: blocks even when the user asks.
2. `soft_deny`: blocks unless an `allow` exception or explicit user intent clears it.
3. `allow`: exceptions to `soft_deny`.
4. Explicit user intent clears the remaining soft blocks only if the human's message
   "directly and specifically describes the exact action": name the action and its
   target ("force-push branch `x`", "overwrite `data/seed.json`"). Naming only the verb
   ("you can push") may leave the block in place. An approval covers that one action;
   later ones are blocked again unless the human said it is standing.

A `permissions.deny` rule or a `hard_deny` block cannot be cleared from chat at all;
leave auto mode and answer the prompt instead.

Boundaries the human states ("don't push until I review") work the other way: they
block matching actions until the human lifts them, and they can be lost when compaction
drops the message that stated them. An ask or deny rule makes a boundary durable.

Observed 10/26 (version unknown, session run through Paseo): "授权" did not clear a
block on editing a sandbox launcher, and "明确授权你用 gh pr create" did not clear a
block on `gh pr create`; switching out of auto mode did. The first is too general by the
rules above. For the second, a candidate from the docs, not checked: the default rules
block sensitive details in PR bodies of a public repo unless the user names those details
and the recipient.

## Ways to unblock

Tell the human once, concretely; don't retry the same action in a loop or try another
tool for it.

- The human names the action and its target in the next message; Claude retries.
- `/permissions` → **Recently blocked** → `r` approves one retry. Terminal only; the
  desktop app has no such tab.
- For a routine action: an `autoMode.allow` entry (prose), or a narrow
  `permissions.allow` rule such as `Bash(gh pr create:*)`. Broad rules (`Bash(*)`,
  `Bash(python*)`, `Agent`) are dropped while in auto mode. An allow rule settles the
  action before the classifier, so it no longer sees the arguments.
- For a trusted destination (registry, repo host): `autoMode.environment`.
- `autoMode` is read only from `~/.claude/settings.json`, managed settings or
  `--settings`, never from a repo's `.claude/settings*.json`. Each list you set replaces
  the built-in one unless it includes the literal `"$defaults"`.
- Leave auto mode and answer the permission prompt.

3 blocks in a row or 20 in a session pause auto mode and fall back to prompting; the
thresholds are fixed. Approving the prompted action resumes auto mode.
