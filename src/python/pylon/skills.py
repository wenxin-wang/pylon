"""Install global agent skills and the human's preferences for every tool.

Global skills come from one or more source repos. Each source has a checkout
that only tracks main, outside the working repos, so switching branches in a
working copy never changes what agents read. Each tool's skill directory gets
one symlink per skill, pointing into a checkout's skills/<name>.

The preferences come from the policy repo; each tool's global instruction
file is a symlink to its preferences.md.

A skill that needs Claude Code hooks declares them in its claude-hooks.json;
they are merged into Claude Code's user settings, every variant of them.

Skills used by one workshop only live in that repo (.claude/skills/,
.agents/skills/), which the tools read on their own; they are not installed.
"""

import json
import os
import re
import shutil
import subprocess
import time
from pathlib import Path

CHECKOUTS = Path.home() / ".local/share/agent-policies"
# Source name -> remote. The checkout is CHECKOUTS / name.
SOURCES = {"agent-policy": "https://github.com/wenxin-wang/agent-policy.git"}
POLICY = "agent-policy"
# Claude Code and Codex read personal skills from these directories.
TOOL_DIRS = [Path.home() / ".claude/skills", Path.home() / ".agents/skills"]
# Claude Code and Codex load these files in every session.
GLOBAL_FILES = [Path.home() / ".claude/CLAUDE.md", Path.home() / ".codex/AGENTS.md"]
# Claude Code reads user hooks from this file only. The human keeps variants
# of it as settings.json.* and links settings.json to one of them.
CLAUDE_SETTINGS = Path.home() / ".claude/settings.json"
# A settings file is copied here before its hooks change.
BACKUPS = Path.home() / ".local/state/pylon/settings-backup"
# A skill's hooks, in the shape of the settings' "hooks": event -> groups.
HOOKS_FILE = "claude-hooks.json"
# A hook command that runs code from ~/.claude/skills/<name>/ belongs to that skill.
HOOK_OWNER = re.compile(r"~/\.claude/skills/([^/\s]+)/")


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


def hook_owner(handler):
    """The skill whose code a hook handler runs, or None."""
    match = HOOK_OWNER.search(str(handler.get("command", "")))
    return match.group(1) if match else None


def skill_hooks(checkouts, skill_dir):
    """The hooks declared by each skill linked into skill_dir, and problems.

    A skill may only declare hooks that run its own code.
    """
    wanted, problems = {}, []
    if not skill_dir.is_dir():
        return wanted, problems
    for link in sorted(skill_dir.iterdir()):
        path = link / HOOKS_FILE
        if not ours(link, checkouts) or not path.is_file():
            continue
        try:
            hooks = json.loads(path.read_text())
            handlers = [h for groups in hooks.values() for g in groups
                        for h in g["hooks"]]
            valid = all(hook_owner(h) == link.name for h in handlers)
        except (ValueError, AttributeError, KeyError, TypeError):
            valid = False
        if valid:
            wanted[link.name] = hooks
        else:
            problems.append(f"{path} is not valid or runs code outside "
                            f"~/.claude/skills/{link.name}/; skipped")
    return wanted, problems


def merge_hooks(settings, wanted, mine):
    """settings with the hooks mine() claims dropped and wanted appended."""
    hooks = {}
    for event, groups in settings.get("hooks", {}).items():
        kept = []
        for group in groups:
            handlers = [h for h in group.get("hooks", []) if not mine(h)]
            if handlers or not group.get("hooks"):
                kept.append({**group, "hooks": handlers})
        if kept or not groups:
            hooks[event] = kept
    for declared in wanted.values():
        for event, groups in declared.items():
            hooks.setdefault(event, []).extend(groups)
    result = dict(settings)
    if hooks:
        result["hooks"] = hooks
    else:
        result.pop("hooks", None)
    return result


def settings_files(settings):
    """The real files behind settings and its variants, and problems.

    A file reached through a dangling link is reported, not created.
    """
    files, problems = [], []
    for path in [settings] + sorted(settings.parent.glob(settings.name + ".*")):
        if path.is_symlink() and not path.exists():
            problems.append(f"{path} is a dangling link; skipped")
            continue
        real = path.resolve()
        if real not in files:
            files.append(real)
    return files, problems


def write_settings(path, data, backup_dir):
    """Copy path into backup_dir, then replace it in one step."""
    if path.exists():
        backup = backup_dir / path.relative_to("/")
        if not backup.exists():
            backup.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, backup)
    tmp = path.with_name(f".{path.name}.pylon-tmp")
    try:
        tmp.write_text(json.dumps(data, indent=2, ensure_ascii=False) + "\n")
        if path.exists():
            shutil.copymode(path, tmp)
        os.replace(tmp, path)
    finally:
        if tmp.exists():
            tmp.unlink()


def sync_hooks(wanted, checkouts, skill_dir, settings, backups):
    """Make every settings file hold exactly the wanted skill hooks.

    A hook is ours when it runs code under ~/.claude/skills/<name>/ and <name>
    is our link or gone; other hooks and settings are left alone. Returns the
    problems found; a file that can't be read or written is skipped.
    """
    def mine(handler):
        name = hook_owner(handler)
        if name is None:
            return False
        entry = skill_dir / name
        return ours(entry, checkouts) or not (entry.exists() or entry.is_symlink())

    files, problems = settings_files(settings)
    backup_dir = backups / time.strftime("%Y%m%d-%H%M%S")
    for path in files:
        try:
            old = json.loads(path.read_text()) if path.exists() else {}
            new = merge_hooks(old, wanted, mine)
        except (ValueError, AttributeError, TypeError) as e:
            problems.append(f"{path}: cannot read its hooks ({e}); skipped")
            continue
        if new == old:
            continue
        try:
            write_settings(path, new, backup_dir)
        except OSError as e:
            snippet = json.dumps({"hooks": new.get("hooks", {})}, indent=2)
            problems.append(f"{path}: cannot write ({e}); its hooks should be:\n"
                            f"{snippet}")
    return problems


def hooks_status(checkouts, skill_dir, settings):
    lines = []
    files, _ = settings_files(settings)
    for path in files:
        try:
            data = json.loads(path.read_text()) if path.exists() else {}
            names = sorted({hook_owner(h) for groups in data.get("hooks", {}).values()
                            for g in groups for h in g.get("hooks", [])} - {None})
            state = " ".join(names) or "(none)"
        except (ValueError, AttributeError, TypeError, OSError):
            state = "cannot read"
        lines.append(f"{path}: hooks {state}")
    return lines
