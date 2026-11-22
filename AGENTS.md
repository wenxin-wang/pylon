# pylon

A public repo: host configs, sandboxes for coding agents, and the tools around them.
Mostly developed by agents; a human reviews and merges on GitHub. It will gradually
replace uraj (`wenxin-wang/uraj`).

pylon is a workshop. **Before any work, read the workshop skill in full**: the
conventions shared by all workshops (modules and levels, docs, projects, llm-wiki, PRs,
voice). It is a global skill, installed as `~/.claude/skills/workshop/` or
`~/.agents/skills/workshop/`; if neither is there, read
`~/.local/share/agent-policies/agent-policy/skills/workshop/SKILL.md`, and if that is missing
too, stop and ask the human. This file
holds the rules that must always hold and what is specific to pylon; where the two
disagree, this file wins.

## Entry points

- [README](README.md): the module table. Which modules exist, their levels, why they
  exist, where to start. **Written for the human**, one page at most.
- [Glossary](docs/glossary.md): one name per concept; register a new term before
  using it.
- Projects (status in each `TODO.md`):
  - [agent-policy](docs/projects/261120-agent-policy/README.md): a private repo for
    agents' standing instructions and permissions.
  - [llm-wiki](docs/projects/261121-llm-wiki/README.md): how workshops use
    `llm-wiki/`, the knowledge agents keep for themselves.
  - [uraj-migration](docs/projects/261119-uraj-migration/README.md): moving modules
    over from uraj.

## Languages

- The main languages are C/C++ and Python. "explain" modules use only these.
- Guile only where Guix requires it (packages, services, system and Home configs).
  Put logic in Python or C where possible, and keep Guile code readable for the
  human: plain, few macros, nothing clever.
- Any other language needs approval. To try one, put it in a "use" module or under
  `exp/`.

## Layout

Follows https://trop.in/blog/a-sane-directory-structure-for-software-projects.

| Directory | Contents |
|---|---|
| `src/<lang>/pylon/...` | Code. `<lang>` is `c`, `python`, `guile`, or `guix` |
| `test/<lang>/pylon/...` | Tests, mirroring the `src` paths |
| `bin/` | Thin entry points: parse arguments, call into `src` |
| `env/` | Deployment configs |
| `exp/<name>_<yyyymmdd>/` | One-off scripts and experiments; `src` and `env` never reference them |
| `vendor/` | Third-party code and submodules, never modified |
| `docs/` | Documentation, see the workshop skill |
| `llm-wiki/` | Knowledge agents keep for themselves, see the workshop skill |
| `.claude/`, `.codex/` | Hooks for Claude Code and Codex (the llm-wiki index); Claude Code's local files ignored |
| `out/`, `tmp/` | Run outputs, not in git |

## pylon-specific

- `docs/` and the glossary follow the workshop skill. The first section of
  `README.md` ("from the human") is in the human's own voice.
- PR topics: a module name, or `docs`, `env`, `layout`, `exp`.
- Don't link to or mention private material outside the repo: this repo is public.

## Readable spaces

- garden (`~/src/garden`): the human's public notes.

## Must

- Never commit directly to `main`, never merge PRs; the human merges.
- Never commit plaintext secrets, private keys, tokens, or host-private configs.
- Never deploy to a host or restart services (`guix system reconfigure`,
  `guix deploy`, `guix home reconfigure`, `herd restart`, etc.) unless the human
  explicitly asks in this session.
- Never modify `vendor/`.
- Reread `AGENTS.md` on `origin/main` before committing or opening a PR; old sessions
  may miss new rules.
- Confirm scope before starting; stop and ask where a change would overturn something
  the human has decided.
