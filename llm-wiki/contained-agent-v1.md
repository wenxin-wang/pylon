---
description: What an agent in uraj's contained-agent v1 sandbox cannot do (herd, ssh, push, git config) and why
category: agents
sources:
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/overview-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/container-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/project-config-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/ssh-agent-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/agent-specs-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/guix-daemon-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/paseo/broker-guide.md
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/src/guile/uraj/bin/contained-agent.scm
  - https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/src/guix/uraj/home/llm.scm
  - https://man.openbsd.org/ssh_config
  - https://man.openbsd.org/ssh-add.1
  - https://git-scm.com/docs/git-clone
  - https://code.claude.com/docs/en/interactive-mode
checked: 2026-11-24
---

# contained-agent v1: what works inside the sandbox

An agent started by uraj's contained-agent v1 (`guix shell --container` per project,
from a terminal or through Paseo) has far less than the human's login. This page lists
what an agent will hit, so it can say so early, with the exact command for the human,
instead of failing halfway. How the sandbox is built: uraj's
[overview](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/overview-guide.md),
[container](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/container-guide.md),
[project config](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/project-config-guide.md)
and [ssh-agent](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/ssh-agent-guide.md)
guides. pylon's contained-agent v2 is a separate design; update this page when it
replaces v1.

Checked against uraj `1e434fd` and, where marked *live*, in a running v1 sandbox on
2026-11-23 (git 2.55.0, OpenSSH 10.5p1). Grants are per project and per host and come
from a central config the agent cannot see or change, so probe instead of assuming:

```sh
which ping herd sudo guix ssh        # a missing tool was not granted
ls /run/user/$(id -u)                # empty: no Shepherd socket
[ -S "$SSH_AUTH_SOCK" ] && echo ssh-agent mounted
```

## At a glance

| Want | Inside v1 | Say to the human |
|---|---|---|
| Restart a host service (`herd`, `sudo`) | Impossible (below). | The exact command, to run in a host terminal. |
| `ping`, `iperf3`, other tools | Not in the container's package list (`llm.scm:154-162`); the network is the host's, no egress filter, so `curl`/`wget`/TCP tools work. `guix shell` exists only where the project has `(guix-daemon)` ([guide](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/guix-daemon-guide.md); absent in the sandbox checked). | Ask for the tool, or put it in the project's `.guix/manifest.scm`: it counts after the human runs `contained-agent allow` and starts a new session. |
| `ssh` | Only with a central `(ssh-agent ...)` clause; `known_hosts` and `config` read-only (below). | A new host, or a new sshd port, needs the human. |
| `git push`, `gh` | Only with a central credential clause (below). | The `git push` / `gh pr create` commands to run. |
| Write `.git/config` | Fails with "Device or resource busy" (below). | Nothing; use the workaround. |
| `git worktree add` | Fails (below). | Nothing; use a clone under `/tmp`. |

## Host services

- There is no `herd`, no `sudo`, and `/run/user/UID` is an empty directory (*live*). The
  container has its own PID namespace, so host processes cannot be inspected or
  attached to. `/run/user`, `~/.ssh`, `~/.paseo` and a few more can never be mounted by
  a project's request ([denied paths](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/contained-agent/project-config-guide.md)).
- Claude Code's `!` shell mode runs the command from the Claude Code process
  ([docs](https://code.claude.com/docs/en/interactive-mode)), and the code starts that
  process inside the container (`contained-agent.scm:1352`), so `! herd restart x`
  fails the same way. Inferred; not tried.
- Observed 2026-11-04 (no `herd`, no Shepherd socket) and 2026-11-07 (no `sudo`): the
  agent diagnosed a hung Paseo daemon and a wrong service image from logs, then handed
  the human `herd restart ...` / `sudo herd restart ...`.

## ssh

- No central `(ssh-agent ...)` clause for the project: the container has no `openssh`
  and `SSH_AUTH_SOCK` is unset (`contained-agent.scm:1275`, `:1283`). A Paseo worktree
  without its own central entry inherits the entries of the checkout it was made from,
  else the main worktree's; an explicit entry, even an empty one, stops the inheritance
  (project-config-guide), so check which one applies before promising anything.
- Only the central `projects.scm` or host-private `projects.local.scm` may declare the
  clause; the project's own `.guix/contained-agent.scm` cannot, even approved
  (ssh-agent-guide, first paragraph). By the code, such a clause makes the launch abort
  with `bad clause` (`contained-agent.scm:314`, `:1196-1200`); not tried.
- With the clause, the keys stay on the host. The container gets a socket and a
  read-only `~/.ssh/known_hosts` snapshot and `~/.ssh/config` (the clause's
  `client-config`). Writing either fails with "Read-only file system"; the rest of
  `~/.ssh` is writable, in the session's tmpfs home (*live*).
- Keys are loaded with `ssh-add -h` destination constraints: usable only for the listed
  destinations, whose host keys `ssh-add` looks up in the host's known_hosts file when
  it loads them ([ssh-add(1)](https://man.openbsd.org/ssh-add.1)). A host missing there
  fails loading, and the wrapper never fetches or trusts host keys on its own
  (ssh-agent-guide, "Destination patterns").
- uraj's example `client-config` sets `StrictHostKeyChecking yes`, and `ssh -G` in a
  live sandbox reported strict checking. An unknown host then fails with `No ED25519
  host key is known for HOST and you have requested strict checking. Host key
  verification failed.` ([ssh_config(5)](https://man.openbsd.org/ssh_config): `yes`
  "forces the user to manually add all new hosts"). `ssh-keyscan HOST >>
  ~/.ssh/known_hosts` fails too (*live*).
- So a plan that needs a host key the snapshot lacks needs the human: a new server, or
  moving sshd to another port, because the entry is then spelled `[host]:port` and so
  is the destination (ssh-agent-guide, "Destination patterns"). Observed 2026-10-29:
  the agent planned to move a new server's sshd to port 2222; the human said that would
  lock the agent out, its known_hosts not trusting the host, then took over that setup.
  The human adds the host key to the central known_hosts source and the destination to
  the clause, then runs `paseo-broker ssh-refresh DIR` on the host (below).
- Don't work around the snapshot on your own. `ssh -o UserKnownHostsFile=/tmp/known_hosts
  -o StrictHostKeyChecking=accept-new HOST` does connect (*live*, against a public
  server; the 2026-10-29 session kept two new servers' keys in a file under `/tmp`), but
  it is the trust on first use that uraj's design avoids on purpose ("the wrapper never
  fetches or trusts host keys automatically", ssh-agent-guide). Ask the human first. It
  also does not make the project's constrained keys usable for that host (per the `-h`
  text above; not tried).
- After the human edits the clause, known_hosts or keys: a new session picks up a
  changed policy or known_hosts file by itself; `paseo-broker ssh-refresh DIR` forces a
  reload even if both look unchanged and lets running sessions see it without a
  restart. If the refresh fails, the agent is stopped: retry the command, then reopen
  the sessions, since the new agent has a new socket ([broker-guide](https://github.com/wenxin-wang/uraj/blob/1e434fd433fb872225f2d20cca9df174ff278d77/docs/paseo/broker-guide.md),
  "Shared project SSH agents"). Terminal sessions use `contained-agent ssh-start`
  instead, which does not reach the broker's agents. A changed set of mounts never
  reaches a running session (project-config-guide: "Restart sessions to apply the new
  policy").

## git push and gh

- Without a central `(github TIER)` or `(git-credential URL FILE)` clause there is no
  HTTPS credential: the host's `gh` login and credential helpers are never used, and a
  credential clause in the project's own file is rejected (overview-guide, "What agents
  cannot do"; project-config-guide, "Credentials"). With no ssh-agent clause there is no
  SSH key either. `git commit` works; `git push` and `gh pr create` fail.
- The github clause applies to HTTPS remotes only; SSH remotes use the ssh-agent clause.
  A key constrained to `git@github.com` does not limit repositories (ssh-agent-guide);
  a repository-scoped deploy key is rejected by GitHub on any other repository.
- Observed 2026-11-09 (HTTPS remote, no credential; the SSH key present had no access to
  that repository) and 2026-11-14 (no `gh` login, no key): the agent committed, stopped,
  and gave the human the `git push` / `gh pr create` commands. Say this before the
  work, not after.

## git inside a Paseo worktree

Mounts, as listed in `/proc/self/mountinfo` (*live*; design in container-guide, "Git"):
the main worktree `~/src/REPO` is read-only; its `.git` is a read-write mount, with
`config`, `hooks/` and `worktrees/` read-only mounts on top; the session's own
`worktrees/NAME` is read-write, with `commondir` and `gitdir` read-only; the worktree's
`.git` file is read-only. So commits, branches, refs and fetches work, and these fail:

- Writing `.git/config`. Git writes `config.lock` next to `config` (writable) and
  renames it over `config`; `config` is a bind-mounted file, so `rename(2)` returns
  `EBUSY`, "Device or resource busy". An in-place write returns `EROFS` (*live*).
  Affected: `git config --local` (exit 4), `git remote add` (fatal, 128), `git branch -u`
  (prints the error twice, then still says "set up to track", exits 0, writes nothing),
  and the upstream step of `git push -u`: the push and the PR succeed, only a warning
  shows (uraj docs; observed 2026-11-04 to 11-15 in five sessions; not re-run live).
- Workaround, no explicit remote branch name needed: the launcher gives the session its
  own `~/.gitconfig` (tmpfs, dropped on exit) with `push.default=current`
  (`contained-agent.scm:1292`), so a bare `git push` pushes the current branch to its
  namesake on `origin`. For anything else use `git config --global KEY VALUE` (*live*),
  e.g. `branch.B.remote origin` and `branch.B.merge refs/heads/B`, or
  `remote.R.url URL` (container-guide's table).
- `git worktree add` fails: `fatal: could not create directory of
  '<main>/.git/worktrees/NAME': Read-only file system` (*live*). The Agent tool's
  worktree isolation fails with it (observed 2026-11-14). Use a clone that shares
  objects: `git clone --shared <main repo> /tmp/x` works (*live*; objects come from the
  main `.git/objects` through `alternates`), then `git -C /tmp/x remote set-url origin
  <real URL>` and push from there (push not re-run). Observed 2026-11-14/15: four
  sub-agents each worked in such a clone. Caveats: `/tmp` is a private tmpfs, so the
  human cannot see it and it is gone when the session restarts (observed); and a
  `--shared` clone breaks if the source repo loses the objects it borrows
  ([git-clone](https://git-scm.com/docs/git-clone), `--shared`), so push soon.

## What the sandbox mounts for agents' own files

In code (`llm.scm:148-150`) every agent gets these read-only when they exist; a missing
path is skipped without a warning: `~/.agents`, `~/.local/share/agent-policies`,
`~/.llm-wiki`, `~/src`, `~/.gitconfig.local`, `~/.cache/paseo-tmp`, `~/.paseo/uploads`.

- `~/.llm-memory` is mounted with `share` (`llm.scm:138`), which uraj's
  agent-specs-guide defines as read-write, although the comment above it and
  container-guide call it read-only. If it exists, writes there reach every later session
  of every project.

- Global skills and preferences are symlinks into `~/.local/share/agent-policies`, for
  example `~/.claude/CLAUDE.md` and `~/.claude/skills/*` (*live*), so they resolve only
  because that mount exists.
- `~/.codex` is the read-only host directory under a session-only writable overlay,
  dropped on exit, with a few paths persisted to the host (`sessions/`, SQLite state,
  `history.jsonl`, `auth.json`: `contained-agent.scm:1019-1025`). Codex and Pi overlay
  `~/.agents/skills` the same way; Claude Code overlays `~/.claude` (agent-specs-guide).
  So edits to settings, plugins, `/config` and `claude mcp add` last for one session.
- Neither the policy checkout nor `~/.agents` can be edited from inside. Observed
  2026-11-23: the agent cloned the policy repo to `/tmp` and opened its PR from there.
- Mounts are fixed at launch: a session started before a mount was added never sees it
  (container-guide: changes take effect in a new session; observed 2026-11-21).

## Other limits, covered in uraj's docs

No GPG; root-only host logs (`/var/log/debug`, `secure`) stay unreadable even when the
project exposes `/var/log`; all of `~/src` is readable, but by default only the current
worktree is writable, unless the central entry shares more (overview-guide, "What agents
cannot do"; container-guide).
