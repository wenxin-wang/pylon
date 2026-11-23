---
description: How Karpathy's wiki, Letta, Hermes, OpenClaw, Claude Code and Codex keep agent memory; staleness
category: agents
sources:
  - https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f
  - https://www.letta.com/blog/context-repositories/
  - https://www.letta.com/blog/our-next-phase/
  - https://github.com/nousresearch/hermes-agent
  - https://www.glukhov.org/ai-systems/hermes/hermes-agent-memory-system/
  - https://safeguard.sh/resources/blog/how-hermes-agent-learns-memory-skills-session-search-and-background-review
  - https://docs.openclaw.ai/concepts/memory-architecture
  - https://docs.openclaw.ai/concepts/memory
  - https://dev.to/naman_here/why-coding-agent-memory-files-go-stale-and-what-teams-should-do-instead-1659
  - https://blog.sigplan.org/2026/04/21/repositories-are-human-agent-knowledge-factories/
  - https://sidtechno.com/blog/ai-agent-memory-fewer-notes
  - https://code.claude.com/docs/en/memory
  - https://developers.openai.com/codex/memories
  - https://github.com/BayramAnnakov/claude-reflect
  - https://arxiv.org/html/2611.26361
checked: 2026-11-23
---

# Agent memory

No standard exists; most details below come from each project's docs plus blog
write-ups, which sometimes disagree. Check the project's own docs before relying on a
number.

## Karpathy's LLM wiki (gist, 2026-04)

- Three layers: raw sources (the LLM reads, never edits), the wiki (markdown pages the
  LLM owns), and a schema file (`CLAUDE.md`/`AGENTS.md`) with structure and workflows.
- `index.md`: one line per page with a summary, grouped by category; read first on
  every query. `log.md`: append-only record of ingests, queries, lint passes.
- Operations: ingest (one source can touch 10 to 15 pages), query (good answers are
  filed back as pages), lint (contradictions, stale claims, orphans, missing links).
- Scale: index-only navigation works to about 100 sources and a few hundred pages;
  past that, add a search tool.
- Known weak spots: the index drifts if sources are added without an ingest; many
  writers conflict because each ingest touches many files.

## Letta (MemGPT team)

- 2026: memory moved from database blocks to git-backed "context repositories"
  (MemFS); special memory tools are being replaced by plain file operations.
- Files under `system/` are always fully loaded; the rest is on demand. The whole file
  tree sits in the system prompt, so names navigate.
- Each file has frontmatter with a description, modeled on `SKILL.md`.
- Writing: a background "sleep-time" reflection reviews recent conversation and
  commits; "defrag" splits large files, merges duplicates, aims at 15 to 25 files.
- Concurrency: each subagent writes in its own git worktree, merged with plain git.

## Hermes Agent (Nous Research)

- Always-loaded memory is two capped files: `MEMORY.md` (environment, conventions,
  lessons; about 2,200 chars) and `USER.md` (preferences; about 1,375 chars). A write
  past the cap fails loudly, forcing the agent to choose.
- Loaded once at session start as a frozen snapshot; edits show up next session.
- Past sessions in SQLite with full-text search. Skills are created and patched by
  the agent after complex tasks; a background review after turns can save memory and
  skills. Writes can require human approval.

## OpenClaw

- Curated `MEMORY.md` (durable facts, standing decisions; not a log) and daily notes
  `memory/YYYY-MM-DD.md` (only today and yesterday load automatically).
- Memory flush: before compaction a silent turn tells the agent to save context into
  the daily note. A "dreaming" sweep promotes useful items into `MEMORY.md`.

## Claude Code auto memory

- On by default in local sessions. The agent itself decides what to save ("doesn't
  save something every session"): `user`, `feedback`, `project`, `reference` notes,
  one file each plus a `MEMORY.md` index, in `~/.claude/projects/<repo>/memory/`,
  shared across worktrees of a repo, machine-local.
- Loaded each session: the first 200 lines or 25KB of `MEMORY.md`. Off with
  `autoMemoryEnabled: false` or `CLAUDE_CODE_DISABLE_AUTO_MEMORY=1`.
- Community write-ups describe an "auto dream" consolidation and a `/dream` command;
  not found in the official memory page as of 2026-11-23.

## Codex memories

- Off by default (`features.memories`). No in-session notes: after a chat has been
  idle long enough, a background pipeline reviews it and writes summaries and durable
  entries under `~/.codex/memories/` (`memories_v2/` in fresh Codex homes since
  2026-10). `/memories` sets per chat whether it reads or feeds memories; secrets
  are redacted from generated fields.

## claude-reflect (third-party plugin)

- Capture by hook, not by the agent: a hook on every prompt matches corrections
  with regex ("no, use X", "actually", "remember:") and queues them; the queue is
  backed up before compaction.
- Nothing is written until the human runs `/reflect` and approves each item (with a
  semantic check and merge against existing CLAUDE.md). `--scan-history` backfills
  corrections the regex missed from old session files; Claude Code deletes sessions
  after 30 days by default (`cleanupPeriodDays`).

## When distilled lessons go wrong

- A deployment study of lessons mined from agent trajectories (arXiv 2611.26361):
  memory helped overall, but a lesson derived many times was still wrong (it assumed
  a page size of 20 where the API default was 5). Support counts how often a lesson
  was derived, not whether it is true. The authors would not run an unattended
  memory loop without a correctness or recency signal, and found keeping provenance
  (source tasks) through every stage important; it was silently dropped twice.
- Compressing to a few general rules lost most of the gain; one-off, situational
  lessons carried much of it.

## Common patterns

- Two tiers: a small, curated, always-loaded tier and a larger on-demand tier.
- Navigation by descriptions (frontmatter, file tree, or index) so the agent knows
  what exists without opening files; the always-loaded part is what makes the agent
  remember to look.
- Something must consolidate regularly (reflection, dreaming, defrag, or hard caps),
  or memory bloats and goes stale.
- Git as the store: history, rollback, parallel writers.

## Practices against staleness

- Don't store what can be read from the code or repo.
- Don't repeat an explanation in two files; link instead.
- Put a date (and optionally a review-by date) on each note.
- Prune regularly: a few distilled notes beat hundreds of fragments.
