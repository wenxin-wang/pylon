"""Install agent skills from a checkout of pylon that only tracks main.

The checkout lives outside the working repos, so switching branches in
~/src/pylon never changes what agents read. Each tool's skill directory gets
one symlink per skill, pointing into the checkout.
"""

import os
import subprocess
from pathlib import Path

REMOTE = "https://github.com/wenxin-wang/pylon.git"
CHECKOUT = Path.home() / ".local/share/pylon/main"
# Claude Code and Codex read personal skills from these directories.
TOOL_DIRS = [Path.home() / ".claude/skills", Path.home() / ".agents/skills"]


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


def remove_links(checkout, tool_dirs):
    """Remove every link of ours; the checkout itself is kept."""
    for tool_dir in tool_dirs:
        if not tool_dir.is_dir():
            continue
        for link in tool_dir.iterdir():
            if ours(link, checkout):
                link.unlink()


def status(checkout, tool_dirs):
    lines = []
    if checkout.exists():
        lines.append(f"{checkout}: {git(checkout, 'log', '-1', '--oneline')}")
    else:
        lines.append(f"{checkout}: missing")
    for tool_dir in tool_dirs:
        links = [p.name for p in sorted(tool_dir.iterdir())
                 if ours(p, checkout)] if tool_dir.is_dir() else []
        lines.append(f"{tool_dir}: {' '.join(links) or '(none)'}")
    return lines
