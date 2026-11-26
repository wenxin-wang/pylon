---
description: Making Claude Code and Codex stop for the human on chosen commands, even in bypass mode
category: agents
sources:
  - https://code.claude.com/docs/en/permission-modes
  - https://code.claude.com/docs/en/permissions
  - https://code.claude.com/docs/en/hooks
  - https://code.claude.com/docs/en/auto-mode-config
  - https://learn.chatgpt.com/docs/hooks
  - https://learn.chatgpt.com/docs/agent-configuration/rules
  - https://learn.chatgpt.com/docs/agent-approvals-security
checked: 2026-11-26
---

# Stopping an agent for confirmation on chosen commands

What each tool offers for "ask the human before this kind of command" (deploys,
restarts on a server, DB writes), read from the docs on 2026-11-26; nothing here was
run. None of these is a security boundary: an agent can reach the same effect another
way (`bash -c`, a Python subprocess), so they only catch mistakes. pylon looked at this
for live deploys and decided not to add any of it (agent-policy project, 11/26).

## Claude Code

- **Ask rules** (`permissions.ask`) prompt in every mode, `bypassPermissions` included
  (permission-modes, "Actions no mode auto-approves"); in auto mode they are settled
  before the classifier. Allow rules have no effect in bypass; deny rules apply.
- **Compound commands**: deny and ask rules match when any subcommand matches,
  including subshells, `$(...)` and loop bodies, so `Bash(git clean *)` prompts for
  `cd /tmp && git clean -f` (permissions, "Compound commands"). Wrappers such as
  `timeout` and `nohup` are stripped first. Another spelling (`git -C . push`,
  `/bin/rm`, `bash -c '...'`) is not matched. For `ssh host 'systemctl restart x'`
  the docs don't say the quoted remote command is parsed, so only `Bash(ssh *)` is
  sure to match, and it catches read-only ssh too.
- **"Yes, and don't ask again"** for Bash is permanent per repository: saved to
  `.claude/settings.local.json` at the main checkout's root, used by all its worktrees
  (permissions, "Permission system"). There is no session-only choice for Bash.
- **PreToolUse command hooks** return allow, deny, ask or defer (precedence
  `deny > defer > ask > allow`). Whether a hook's `ask` prompts in bypass is not
  stated. A hook can't make an approval last for the session; only `PermissionRequest`
  hooks return `updatedPermissions`, whose `session` destination is in memory only. A
  per-session "lease" therefore needs the hook's own state, keyed by `session_id`.
- The **`if` field** on a hook handler takes one permission rule (`"Bash(ssh *)"`) and
  runs the handler only for matching subcommands, so an expensive hook doesn't run on
  every Bash call.
- **Prompt and agent hooks** send the hook input (with `transcript_path`) to a model;
  the agent type may read files, so it can read the transcript. They answer
  `{"ok": true|false, "reason"}`: on PreToolUse `false` denies, there is no ask. Agent
  hooks are marked experimental. Default timeouts 30 s (prompt) and 60 s (agent).
- **Auto mode** is the built-in model check: `autoMode.soft_deny` rules are prose that
  explicit user intent can clear. It doesn't run in bypass mode. See
  [claude-code-auto-mode](claude-code-auto-mode.md).

## Codex

- **Rules** (`prefix_rule` with `allow`, `prompt` or `forbidden`; experimental) split
  plain compound commands, but redirection, substitution or variables make the whole
  script one invocation. Whether `prompt` still fires with approvals off
  (`approval_policy = "never"`, `--yolo`) is not stated.
- **Hooks** ([agent-hooks](agent-hooks.md)): PreToolUse can deny, or allow with
  `updatedInput`; `permissionDecision: "ask"` is parsed but not supported, so the
  hook run is marked failed and the tool call goes ahead. Prompt and agent handlers
  are skipped. `PermissionRequest` runs only where Codex would prompt anyway.
- No session-scoped approval is documented. Rules files live in a `rules/` folder next
  to a config layer, e.g. `~/.codex/rules/default.rules`.
- **Auto-review** (`approvals_reviewer = "auto_review"`) is a model reviewer, and it
  judges only approval requests, so it applies only with interactive approvals
  (`on-request` or granular), not with approvals off.
