---
description: Claude Code and Codex lifecycle hooks that can feed context to the agent (SessionStart, PreCompact) and their limits
category: agents
sources:
  - https://code.claude.com/docs/en/hooks
  - https://github.com/anthropics/claude-code/issues/16538
  - https://learn.chatgpt.com/docs/config-file/config-reference
  - https://codex.danielvaughan.com/2026/04/15/codex-cli-hooks-complete-guide-events-policy-patterns/
checked: 2026-11-22
---

# Agent hooks

## Claude Code

- `SessionStart`: plain stdout (exit 0) is added to the context; the JSON form uses
  `hookSpecificOutput.additionalContext` with `hookEventName: "SessionStart"`.
  Matchers include the source (startup, resume, clear, compact), so the same hook can
  run again after compaction.
- `PreCompact`: stdout becomes instructions for the compaction summary; it does not
  add context or let the agent act first.
- A reported bug (issue 16538) dropped `additionalContext` from hooks defined in a
  plugin; the workaround was defining them in settings instead. May be fixed.

## Codex CLI

- Feature flag `features.hooks` (old name `features.codex_hooks`); hooks in
  `hooks.json` or inline `[hooks]` in `config.toml`.
- Events: PreModelRequest, PostModelRequest, PreToolUse, PermissionRequest,
  PostToolUse, PreCompact, PostCompact, SessionStart, SessionEnd, SubagentStart,
  SubagentStop, UserPromptSubmit, Stop, Interrupt.
- Only command and MCP tool handlers run; prompt and agent handlers are skipped.
- `SessionStart` can return `hookSpecificOutput.additionalContext`; guides say plain
  stdout also works. Its `source` is startup, resume, clear, compact, or fork.
- `additionalContextLimit` per handler defaults to 2500 tokens: larger context is
  saved to disk and the model sees a preview; `0` passes everything.
- Project-scoped `.codex/` config, hooks included, loads only for trusted projects.
- Not supported with cloud orchestration.
