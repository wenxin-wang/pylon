#!/usr/bin/env python3
"""Filter Claude Code transcripts down to what the human and the agent said.

For the first reflection run (docs/projects/261122-reflection/). Reads the
top-level transcripts (<session-id>.jsonl, not subagents) in the given
directories under ~/.claude/projects/, drops tool calls, tool output, thinking
and injected text, and writes one markdown file per session:

    [人 3 11-23 10:52] the human's message, numbered within the session
    [agent] the agent's reply text

Human messages that match CORRECTION are listed in flagged.tsv, not marked in
the session files, so a full read can be checked against the regex without
seeing it. A forked session
repeats earlier messages under new ids; those are left out, with a pointer to
where they were first written. Also writes sessions.tsv: one line per session
with its size before and after.
"""

import argparse
import json
import re
import sys
from datetime import datetime
from pathlib import Path

# Frozen before reading the transcripts, so the comparison is fair.
CORRECTION = re.compile(
    r"不对|不是这样|不是这个|错了|搞错|弄错|别再|不要再|又(忘|错|漏|没)|说过|我不是说|"
    r"为什么(又|还|没|不)|不通顺|看不懂|读不懂|没看懂|太长|啰嗦|没必要|不应该|"
    r"而不是|改回|撤回|回退|先别|不行|有问题|不太对|不太好|不准确|误解|理解错|跑偏|"
    r"\b(no|nope|wrong|don't|do not|stop|instead|not what|i said|again|why did you|"
    r"shouldn't|revert|undo)\b",
    re.IGNORECASE)

# Blocks the tools inject into a user message; what is left is the human's.
# They nest (a Paseo summary quotes earlier summaries), so the innermost block
# is removed first, until none is left.
INJECTED = re.compile(
    r"<(system-reminder|chat-history-summary|task-notification|command-name|"
    r"command-message|command-args|local-command-stdout|local-command-stderr|"
    r"local-command-caveat|user-prompt-submit-hook)\b[^>]*>(?:(?!<\1\b).)*?</\1>",
    re.DOTALL)
# Pasted text is the human's input but rarely their words; keep a stub.
PASTED = re.compile(r"<pasted_content[^>]*>(.*?)</pasted_content[^>]*>", re.DOTALL)
MAX_HUMAN = 3000


def text_of(content):
    if isinstance(content, str):
        return content
    return "\n".join(b.get("text", "") for b in content
                     if isinstance(b, dict) and b.get("type") == "text")


def human_text(entry):
    """The human's own words in a transcript entry, or None."""
    if entry.get("type") == "attachment":
        att = entry.get("attachment", {})
        if att.get("type") != "queued_command":
            return None
        text = text_of(att.get("prompt", ""))
    elif entry.get("type") == "user" and not (
            entry.get("isMeta") or entry.get("isCompactSummary")
            or entry.get("isVisibleInTranscriptOnly")):
        content = entry.get("message", {}).get("content", "")
        if isinstance(content, list) and any(
                isinstance(b, dict) and b.get("type") == "tool_result"
                for b in content):
            return None
        text = text_of(content)
    else:
        return None
    while True:
        stripped = INJECTED.sub("", text)
        if stripped == text:
            break
        text = stripped
    text = PASTED.sub(lambda m: f"[pasted {len(m.group(1))} chars]", text).strip()
    if not text or text.startswith("<"):
        return None
    if len(text) > MAX_HUMAN:
        text = text[:MAX_HUMAN] + f" [... {len(text) - MAX_HUMAN} more chars]"
    return text


def agent_text(entry):
    if entry.get("type") != "assistant":
        return None
    content = entry.get("message", {}).get("content", [])
    return text_of(content).strip() or None


def local_time(stamp):
    try:
        t = datetime.fromisoformat(stamp.replace("Z", "+00:00"))
    except (AttributeError, ValueError):
        return "?"
    return t.astimezone().strftime("%m-%d %H:%M")


def entries(path):
    with open(path, errors="replace") as f:
        for raw in f:
            try:
                yield json.loads(raw)
            except json.JSONDecodeError:
                continue


def first_stamp(path):
    return min((e["timestamp"] for e in entries(path) if e.get("timestamp")),
               default="")


def filter_session(path, seen):
    """Returns (lines, number of human messages, numbers of those flagged).

    seen maps each human message written so far, (timestamp, text), to where
    it was written; repeats and the replies to them are skipped.
    """
    lines, n, flagged, stamps = [], 0, [], []
    cwd = branch = None
    repeating = False
    for entry in entries(path):
        cwd = cwd or entry.get("cwd")
        branch = branch or entry.get("gitBranch")
        if entry.get("timestamp"):
            stamps.append(entry["timestamp"])
        text = human_text(entry)
        if text is not None:
            key = (entry.get("timestamp"), text)
            if key in seen:
                if not repeating:
                    lines.append(f"[repeats {seen[key]} onwards]\n")
                repeating = True
                continue
            repeating = False
            n += 1
            seen[key] = f"{path.stem} 人 {n}"
            if CORRECTION.search(text):
                flagged.append(n)
            when = local_time(entry.get("timestamp"))
            lines.append(f"[人 {n} {when}] {text}\n")
            continue
        text = agent_text(entry)
        if text is not None and not repeating:
            lines.append(f"[agent] {text}\n")
    first = local_time(min(stamps)) if stamps else "?"
    last = local_time(max(stamps)) if stamps else "?"
    header = f"# {path.stem}\n\n{cwd} ({branch}), {first} - {last}\n\n"
    return [header] + lines, n, flagged


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("out", type=Path)
    parser.add_argument("dirs", nargs="+", type=Path)
    args = parser.parse_args()
    args.out.mkdir(parents=True, exist_ok=True)
    # Oldest first, so a fork points back to the session it came from.
    paths = sorted((p for d in args.dirs for p in d.glob("*.jsonl")),
                   key=first_stamp)
    rows, seen, flags = [], {}, []
    for path in paths:
        lines, n, flagged = filter_session(path, seen)
        if n == 0:
            continue
        body = "".join(lines)
        (args.out / f"{path.stem}.md").write_text(body)
        rows.append((path.stem, path.parent.name, local_time(first_stamp(path)),
                     n, len(flagged), path.stat().st_size, len(body.encode())))
        flags += [(path.stem, k) for k in flagged]
    with open(args.out / "sessions.tsv", "w") as f:
        f.write("session\tdir\tfirst\thuman\tflagged\tbytes\tfiltered\n")
        for row in rows:
            f.write("\t".join(map(str, row)) + "\n")
    with open(args.out / "flagged.tsv", "w") as f:
        f.write("session\thuman\n")
        for session, k in flags:
            f.write(f"{session}\t{k}\n")
    print(f"{len(rows)} sessions, {sum(r[3] for r in rows)} human messages, "
          f"{sum(r[4] for r in rows)} flagged, "
          f"{sum(r[5] for r in rows) / 1e6:.1f}MB -> "
          f"{sum(r[6] for r in rows) / 1e6:.2f}MB", file=sys.stderr)


if __name__ == "__main__":
    main()
