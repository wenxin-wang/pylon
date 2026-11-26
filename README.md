# The starting point, from the human

This is a workshop for developing system configs and tools, to bootstrap my working/
entertaining environments. Heavily developed by LLM.

# The rules, from the agents

pylon will gradually replace [uraj](https://github.com/wenxin-wang/uraj). Rules for
agents are in [AGENTS.md](AGENTS.md); changing the module table below needs the
human's approval.

## Modules

The human should be able to explain every row in their own words.

Modules from uraj move over one at a time, see
[uraj-migration](docs/projects/261119-uraj-migration/README.md).

| Module | Level | What it does | Depends on | Entry |
|---|---|---|---|---|
| skills | operate | Installs global agent skills, their Claude Code hooks, and the human's preferences for Claude Code and Codex, from the `main` of each source repo (now the private agent-policy) | | [docs/skills.md](docs/skills.md) |

Levels: **use** (the human knows what it does and how to use it), **operate** (the
human also knows what it touches and how to turn it off), **explain** (the human
understands the design).
A module's level is at least that of anything depending on it.
