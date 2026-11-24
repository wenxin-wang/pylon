---
description: Claude Code auto mode: when a chat approval clears a classifier block, and other ways to unblock
category: agents
sources:
  - https://code.claude.com/docs/en/permission-modes
  - https://code.claude.com/docs/en/auto-mode-config
checked: 2026-11-23
---

# Claude Code auto mode: getting past a classifier block

In auto mode a second model, the classifier, reviews each action that no allow, ask, or
deny rule settles; reads and edits inside the working directory skip it. Since v2.1.283
auto mode is the default starting mode in the terminal and VS Code.

## What the classifier sees

User messages, Claude's tool calls (except read-only lookups), and CLAUDE.md. Tool
results are stripped, so text in a file or page can't talk it into anything. A block
returns a reason to Claude, e.g. `[Destructive or Unwanted Actions]`.

## Approvals in chat

Rules apply in this order (auto-mode-config, "Override the block and allow rules"):

1. `hard_deny`: blocks even when the user asks.
2. `soft_deny`: blocks unless an `allow` exception or explicit user intent clears it.
3. `allow`: exceptions to `soft_deny`.
4. Explicit user intent clears the remaining soft blocks only if the human's message
   "directly and specifically describes the exact action": name the action and its
   target ("force-push branch `x`", "overwrite `data/seed.json`"). A bare "授权" or
   "you can push" does not count. An approval covers that one action, not later ones.

Boundaries the human states ("don't push until I review") work the other way: they
block matching actions until the human lifts them, and they can be lost when compaction
drops the message that stated them.

Observed 10/26 (version unknown, session run through Paseo): "授权" did not clear a
block on editing a sandbox launcher, and "明确授权你用 gh pr create" did not clear a
block on `gh pr create`; switching out of auto mode did. The first is too general by the
rules above; why the second failed was not found (an older version, or a block the
human's intent can't clear).

## Ways to unblock

Tell the human once, concretely; don't retry the same action in a loop or try another
tool for it.

- The human names the action and its target in the next message; Claude retries.
- `/permissions` → **Recently blocked** → `r` approves one retry. Terminal only; the
  desktop app has no such tab.
- For a routine action: an `autoMode.allow` entry (prose), or a narrow
  `permissions.allow` rule such as `Bash(gh pr create:*)`. Broad rules (`Bash(*)`,
  `Bash(python*)`, `Agent`) are dropped while in auto mode.
- For a trusted destination (registry, repo host): `autoMode.environment`.
- Leave auto mode and answer the permission prompt.

3 blocks in a row or 20 in a session pause auto mode and fall back to prompting; the
thresholds are fixed.
