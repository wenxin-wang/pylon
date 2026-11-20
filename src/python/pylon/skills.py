"""Install agent skills and the human's preferences for every agent tool.

Skills come from a checkout of pylon that only tracks main, outside the
working repos, so switching branches in ~/src/pylon never changes what agents
read. Each tool's skill directory gets one symlink per skill.

The preferences come the same way from the private agent-policy repo; each
tool's global instruction file is a symlink to its preferences.md.
"""

import os
import subprocess
from pathlib import Path

REMOTE = "https://github.com/wenxin-wang/pylon.git"
CHECKOUT = Path.home() / ".local/share/pylon/main"
# Claude Code and Codex read personal skills from these directories.
TOOL_DIRS = [Path.home() / ".claude/skills", Path.home() / ".agents/skills"]

POLICY_REMOTE = "git@github.com:wenxin-wang/agent-policy.git"
POLICY_CHECKOUT = Path.home() / ".local/share/pylon/agent-policy"
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


def ours(link, checkout):
    """Whether link is a symlink into the checkout's skills/."""
    if not link.is_symlink():
        return False
    target = Path(os.readlink(link))
    return target.parent == checkout / "skills"


def link_skills(checkout, tool_dirs):
    """Link every skill into every tool dir; drop our links to removed skills.

    Returns the problems found; a name already taken by something that is not
    ours is left alone and reported.
    """
    problems = []
    names = skills_in(checkout)
    for tool_dir in tool_dirs:
        tool_dir.mkdir(parents=True, exist_ok=True)
        for name in names:
            link = tool_dir / name
            target = checkout / "skills" / name
            if ours(link, checkout):
                link.unlink()
            elif link.exists() or link.is_symlink():
                problems.append(f"{link} exists and is not ours; skipped")
                continue
            link.symlink_to(target)
        for link in tool_dir.iterdir():
            if ours(link, checkout) and link.name not in names:
                link.unlink()
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


def remove_links(checkout, tool_dirs):
    """Remove every link of ours; the checkout itself is kept."""
    for tool_dir in tool_dirs:
        if not tool_dir.is_dir():
            continue
        for link in tool_dir.iterdir():
            if ours(link, checkout):
                link.unlink()


def checkout_status(checkout):
    if checkout.exists():
        return f"{checkout}: {git(checkout, 'log', '-1', '--oneline')}"
    return f"{checkout}: missing"


def status(checkout, tool_dirs):
    lines = [checkout_status(checkout)]
    for tool_dir in tool_dirs:
        links = [p.name for p in sorted(tool_dir.iterdir())
                 if ours(p, checkout)] if tool_dir.is_dir() else []
        lines.append(f"{tool_dir}: {' '.join(links) or '(none)'}")
    return lines


def preferences_status(checkout, files):
    lines = [checkout_status(checkout)]
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
