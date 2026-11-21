"""Install global agent skills and the human's preferences for every tool.

Global skills come from one or more source repos. Each source has a checkout
that only tracks main, outside the working repos, so switching branches in a
working copy never changes what agents read. Each tool's skill directory gets
one symlink per skill, pointing into a checkout's skills/<name>.

The preferences come from the policy repo; each tool's global instruction
file is a symlink to its preferences.md.

Skills used by one workshop only live in that repo (.claude/skills/,
.agents/skills/), which the tools read on their own; they are not installed.
"""

import os
import subprocess
from pathlib import Path

CHECKOUTS = Path.home() / ".local/share/pylon"
# Source name -> remote. The checkout is CHECKOUTS / name.
SOURCES = {"agent-policy": "git@github.com:wenxin-wang/agent-policy.git"}
POLICY = "agent-policy"
# Claude Code and Codex read personal skills from these directories.
TOOL_DIRS = [Path.home() / ".claude/skills", Path.home() / ".agents/skills"]
# Claude Code and Codex load these files in every session.
GLOBAL_FILES = [Path.home() / ".claude/CLAUDE.md", Path.home() / ".codex/AGENTS.md"]


class Error(Exception):
    pass


def git(checkout, *args):
    result = subprocess.run(["git", "-C", str(checkout), *args],
                            capture_output=True, text=True)
    if result.returncode != 0:
        raise Error(f"git {' '.join(args)}: {result.stderr.strip()}")
    return result.stdout.strip()


def update_checkout(checkout, remote):
    """Clone the checkout, or fast-forward it to the remote main."""
    if not checkout.exists():
        checkout.parent.mkdir(parents=True, exist_ok=True)
        result = subprocess.run(
            ["git", "clone", "--branch", "main", remote, str(checkout)],
            capture_output=True, text=True)
        if result.returncode != 0:
            raise Error(f"git clone: {result.stderr.strip()}")
        return
    if git(checkout, "branch", "--show-current") != "main":
        raise Error(f"{checkout} is not on main; fix it by hand")
    if git(checkout, "status", "--porcelain"):
        raise Error(f"{checkout} has local changes; fix it by hand")
    git(checkout, "pull", "--ff-only", "origin", "main")


def skills_in(checkout):
    """Names of the skills on the checkout: skills/<name>/SKILL.md."""
    root = checkout / "skills"
    if not root.is_dir():
        return []
    return sorted(d.name for d in root.iterdir() if (d / "SKILL.md").is_file())


def ours(link, checkouts):
    """Whether link is a symlink into one of the checkouts' skills/."""
    if not link.is_symlink():
        return False
    target = Path(os.readlink(link))
    return any(target.parent == c / "skills" for c in checkouts)


def link_skills(checkouts, tool_dirs):
    """Link every skill into every tool dir; drop our links to removed skills.

    Returns the problems found. A name offered by two sources is linked from
    neither; a name already taken by something that is not ours is left alone.
    """
    problems = []
    sources = {}
    for checkout in checkouts:
        for name in skills_in(checkout):
            sources.setdefault(name, []).append(checkout)
    wanted = {}
    for name, found in sorted(sources.items()):
        if len(found) > 1:
            places = ", ".join(str(c) for c in found)
            problems.append(f"skill {name} is in several sources ({places}); skipped")
        else:
            wanted[name] = found[0] / "skills" / name
    for tool_dir in tool_dirs:
        tool_dir.mkdir(parents=True, exist_ok=True)
        for link in tool_dir.iterdir():
            if ours(link, checkouts) and link.name not in wanted:
                link.unlink()
        for name, target in wanted.items():
            link = tool_dir / name
            if ours(link, checkouts):
                link.unlink()
            elif link.exists() or link.is_symlink():
                problems.append(f"{link} exists and is not ours; skipped")
                continue
            link.symlink_to(target)
    return problems


def link_preferences(checkout, files):
    """Link every global instruction file to the checkout's preferences.md.

    Without preferences.md, our links are removed. Returns the problems found;
    a file that is not our link is left alone and reported.
    """
    problems = []
    target = checkout / "preferences.md"
    for link in files:
        if link.is_symlink() and Path(os.readlink(link)) == target:
            if not target.is_file():
                link.unlink()
            continue
        if not target.is_file():
            continue
        if link.exists() or link.is_symlink():
            problems.append(f"{link} exists and is not ours; skipped")
            continue
        link.parent.mkdir(parents=True, exist_ok=True)
        link.symlink_to(target)
    return problems


def remove_preferences(checkout, files):
    target = checkout / "preferences.md"
    for link in files:
        if link.is_symlink() and Path(os.readlink(link)) == target:
            link.unlink()


def remove_links(checkouts, tool_dirs):
    """Remove every skill link of ours; the checkouts are kept."""
    for tool_dir in tool_dirs:
        if not tool_dir.is_dir():
            continue
        for link in tool_dir.iterdir():
            if ours(link, checkouts):
                link.unlink()


def checkout_status(checkout):
    if checkout.exists():
        return f"{checkout}: {git(checkout, 'log', '-1', '--oneline')}"
    return f"{checkout}: missing"


def status(checkouts, tool_dirs):
    lines = [checkout_status(c) for c in checkouts]
    for tool_dir in tool_dirs:
        links = [p.name for p in sorted(tool_dir.iterdir())
                 if ours(p, checkouts)] if tool_dir.is_dir() else []
        lines.append(f"{tool_dir}: {' '.join(links) or '(none)'}")
    return lines


def preferences_status(checkout, files):
    lines = []
    target = checkout / "preferences.md"
    for link in files:
        if link.is_symlink() and Path(os.readlink(link)) == target:
            state = "linked"
        elif link.exists() or link.is_symlink():
            state = "not ours"
        else:
            state = "(none)"
        lines.append(f"{link}: {state}")
    return lines
