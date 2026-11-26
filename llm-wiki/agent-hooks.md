---
description: Claude Code and Codex hooks that feed context to the agent (SessionStart, PreCompact), and limits
category: agents
sources:
  - https://code.claude.com/docs/en/hooks
  - https://github.com/anthropics/claude-code/issues/16538
  - https://learn.chatgpt.com/docs/config-file/config-reference
  - https://learn.chatgpt.com/docs/hooks
  - https://www.turboai.dev/docs/codex/hooks
  - https://codex.danielvaughan.com/2026/04/15/codex-cli-hooks-complete-guide-events-policy-patterns/
checked: 2026-11-23
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

- Hooks are on by default; `features.hooks = false` (old name
  `features.codex_hooks`) turns them off.
- Hooks live in `hooks.json` or inline `[hooks]` in `config.toml`, per layer:
  user `~/.codex/` (`$CODEX_HOME`), system `/etc/codex/`, project `.codex/`, and
  plugins. Matching hooks from all layers run. `hooks.json` has the same shape as
  Claude Code's settings: `{"hooks": {event: [{"matcher", "hooks": [handler]}]}}`.
- User and project hooks run only after the human trusts them in `/hooks`; trust is
  recorded per hook hash, so a changed hook is skipped until trusted again. System
  and managed hooks are trusted by policy.
- Events: PreModelRequest, PostModelRequest, PreToolUse, PermissionRequest,
  PostToolUse, PreCompact, PostCompact, SessionStart, SessionEnd, SubagentStart,
  SubagentStop, UserPromptSubmit, Stop, Interrupt.
- Only command and MCP tool handlers run; prompt and agent handlers are skipped.
- `SessionStart` can return `hookSpecificOutput.additionalContext`; guides say plain
  stdout also works. Its `source` is startup, resume, clear, compact, or fork.
- `additionalContextLimit` per handler defaults to 2500 tokens: larger context is
  saved to disk and the model sees a preview; `0` passes everything.
- `PreCompact` can't shape the compaction summary: plain stdout is ignored, JSON can
  only stop compaction with `continue: false`, and `hookSpecificOutput` is rejected
  for this event. Claude Code, by contrast, uses PreCompact stdout as instructions
  for the summary.
- Project-scoped `.codex/` config, hooks included, loads only for trusted projects.
  Trust is stored in the user's own config (`[projects."<path>"]` with
  `trust_level = "trusted"`), not in the repo; when the project is untrusted, the
  startup log says so and project hooks are disabled. A sandbox that throws away
  config changes on exit loses the trust too.
- Not supported with cloud orchestration.
