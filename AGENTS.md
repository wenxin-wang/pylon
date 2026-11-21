# pylon

A public repo: host configs, sandboxes for coding agents, and the tools around them.
Mostly developed by agents; a human reviews and merges on GitHub. It will gradually
replace uraj (`wenxin-wang/uraj`).

**The first goal of this repo is that the human can always understand it.** The lesson
from uraj: agents produce far faster than a human can read, until the human no longer
understands their own repo. So many rules below slow agents down and make them stop
and ask more often. That is on purpose: the human's understanding comes before agent
efficiency.

## Entry points

- [README](README.md): the module table. Which modules exist, their levels, why they
  exist, where to start. **Written for the human**, one page at most.
- [Glossary](docs/glossary.md): one name per concept; register a new term before
  using it.
- Projects (status in each `TODO.md`):
  - [workshop-design](docs/projects/261120-workshop-design/README.md): the workshop
    conventions themselves, for workshops other than pylon too.
  - [uraj-migration](docs/projects/261119-uraj-migration/README.md): moving modules
    over from uraj.

## Levels

Every module has a level, recorded in the README module table. The level says how
well the human needs to understand it:

| Level | What the human knows | How it is ensured |
|---|---|---|
| use | It exists, what it does, how to use it | Guide and tests; if broken, delete and redo it |
| operate | What it touches (permissions, secrets, network, hosts), what happens when it breaks, how to turn it off | The "Blast radius" section of `docs/<module>.md` |
| explain | The design itself and the key decisions | The human takes part in the design |

- **Levels propagate down dependencies**: a piece of code is at least at the highest
  level of the modules depending on it. Shared code is kept in one copy and raised in
  level, not duplicated.
- If a change would raise a module's level (e.g. a new dependency makes "use" code
  used by an "explain" module), stop and tell the human: "X is now depended on by Y,
  rising from use to explain." The human may accept or ask for another approach.
- If a change to an "operate" module widens its blast radius (new permission, secret,
  port, or host change), stop and ask first, and update the "Blast radius" section.
- Design decisions for "explain" modules are made by the human. The agent lists
  options, costs, and a recommendation; it does not decide.

## Changes to the module table need approval

Ask before writing code for any of these:

- Adding or removing a module, or changing its level;
- A new language, or a new external dependency (package, service, third-party repo);
- Changing the directory layout.

## Languages

- The main languages are C/C++ and Python. "explain" modules use only these.
- Guile only where Guix requires it (packages, services, system and Home configs).
  Put logic in Python or C where possible, and keep Guile code readable for the
  human: plain, few macros, nothing clever.
- Any other language counts as a module table change. To try one, put it in a "use"
  module or under `exp/`.

## Layout

Follows https://trop.in/blog/a-sane-directory-structure-for-software-projects.
Directories are created when a module moves in; no empty directories up front.

| Directory | Contents |
|---|---|
| `src/<lang>/pylon/...` | Code. `<lang>` is `c`, `python`, `guile`, or `guix` |
| `test/<lang>/pylon/...` | Tests, mirroring the `src` paths |
| `bin/` | Thin entry points: parse arguments, call into `src` |
| `env/` | Deployment configs |
| `exp/<name>_<yyyymmdd>/` | One-off scripts and experiments; `src` and `env` never reference them |
| `vendor/` | Third-party code and submodules, never modified |
| `docs/` | Documentation, rules below |
| `out/`, `tmp/` | Run outputs, not in git |

## Docs

There are two kinds of docs: **living** ones describe how things are now and change
at any time; **process** ones record something that happened, belong to a project,
and are archived when it ends. Day to day, only living docs and in-progress projects
need reading.

- Always markdown. `README.md` and `AGENTS.md` at the repo root are in English;
  everything under `docs/` is in Chinese. Code identifiers, commands, and paths stay
  as they are. No READMEs inside code directories.
- Agents don't write in the human's voice. In docs, the repo owner is "the human"
  (中文“人”) and the AI is "agent"; neither is "I"/“我”. Only the first section
  of the root `README.md` ("from the human") is in the human's own voice.
- File names are lowercase kebab-case; only `README.md`, `TODO.md`, and `AGENTS.md`
  are uppercase.
- **Living docs**, undated:
  - `docs/glossary.md`: the glossary, a hard constraint.
  - `docs/<module>.md`: the module's entry: what it does, how to use it, blast radius
    ("operate" and above), plus links to guides.
  - `docs/<module>/<topic>-guide.md`: usage guide describing what is on `main`; a PR
    that changes usage updates it too.
  - `docs/<module>/` holds living docs only.
- **Projects**: `docs/projects/<yymmdd>-<project>/`, template in
  [`docs/projects/template/`](docs/projects/template/README.md).
  - `README.md` (goal, background, open questions, decisions), `TODO.md` (the single
    source of progress), `step-<n>-handoff.md` (handoff).
  - Design docs `<topic>-design.md`: change them freely while the project runs, but
    keep the "why", don't reduce them to "what is". One per topic; edit in place,
    don't start a new one.
  - Records `<topic>-<kind>-<yyyymmdd>.md`: something that happened, never edited
    afterwards. `<kind>` is `measure`, `review`, `debug`, or `deploy` (deployments,
    migrations). Write one only when the conclusion is worth keeping, and also put
    the conclusion in `TODO.md`.
  - When a project ends: distill conclusions into guides or module entries, move the
    whole directory to `docs/archive/projects/`, and remove it from the entry points.
  - Open questions ("待定") list options, costs, and a recommendation; the human
    decides. "决定" marks what the human decided, so agents know what not to
    overturn without asking: one line each, a conclusion with its date, only those
    still in force. An overturned decision is rewritten in place with the new date;
    git keeps the history, the design doc keeps the why.
  - Tidy "决定" at the end of every step, before the PR: merge small related lines;
    once a decision is written into a design doc, living doc, or `AGENTS.md`,
    replace it with a link there. When the project ends, none is left without a home.
- **Several projects may run at once; every one shows its status.** The line under
  the `TODO.md` heading is its status: in progress, waiting on the human (for what),
  or paused (why). Every project not yet archived is listed under "Entry points".
  Ask the human before starting a new project; follow-up work on an existing project
  is a new step in it.
- Deleting code, deleting docs, and merging duplicate docs are real tasks.
- Don't link to or mention private material outside the repo: this repo is public.

## Wiki and memory

- A workshop may have a `wiki/`: knowledge and material agents care about and the
  human does not read. It is in git; pylon is public, so private material goes to a
  private workshop. Docs cite the wiki only in a references section at their end; a
  conclusion they rely on is written in the doc itself. `src` and `env` never
  reference the wiki.
- Progress lives only in the project's `TODO.md`; knowledge goes in `wiki/`. A
  tool's own memory is private to that tool, so it holds nothing another agent
  would need.

## Skills

- **Global skills** apply to every session; they live in the policy repo and the
  skills module installs them ([docs/skills.md](docs/skills.md)).
- **Project skills** apply to one repo: put them in its `.claude/skills/<name>/`, with
  `.agents/skills` a symlink to `.claude/skills`. The tools read them on their own.

## Readable spaces

A space is the human's own notebook; agents only read it, never write. This repo may
read:

- garden (`~/src/garden`): the human's public notes.

No other spaces.

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

## Submitting code

- One worktree per session, work on a branch; rebase onto the latest `main` before
  submitting, open the PR with `gh pr create`.
- PR title `<topic>: <short description>`, where topic is a module name or `docs`,
  `env`, `layout`, `exp`.
- **PR descriptions are written for the human**, not for the next agent:
  - **One line**: what this PR does. The human should be able to restate it before
    merging.
  - **What you need to know**: effect on the module table (which module; whether
    level or blast radius changed); decisions needed from the human; any deployment
    steps.
  - **How it was verified**: what was run and the result; say plainly what wasn't
    verified.
  - Details and handoff for the next agent go in `docs/projects/`; the PR only links
    there.
- PRs small enough for the human to read in one sitting. One PR, one change; split
  if needed.
- PRs stacked on unmerged branches stay draft; once the base merges, retarget the
  child PR to `main` right away.

## Running and verifying

- Keep the full output of failing tests; measure before concluding on performance.
- Long tasks must not depend on the session staying alive: write the start and status
  commands and hand them to the human to start.
