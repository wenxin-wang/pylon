#!/usr/bin/env python3
"""Check the correction regex against what a full read found.

Takes the directory filter_transcripts.py wrote (its flagged.tsv) and the
readers' JSON files ({"corrections": [{session, n}], "lessons": [{evidence:
[{session, n}]}]}). Prints how many corrections the regex caught, how many of
its hits were corrections, and how many lessons it would have pointed to.
"""

import argparse
import csv
import json
from pathlib import Path


def pairs(items):
    out = set()
    for item in items:
        try:
            out.add((str(item["session"]).strip(), int(item["n"])))
        except (KeyError, TypeError, ValueError):
            pass
    return out


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("filtered", type=Path)
    parser.add_argument("reads", nargs="+", type=Path)
    args = parser.parse_args()
    with open(args.filtered / "flagged.tsv") as f:
        flagged = {(r["session"], int(r["human"]))
                   for r in csv.DictReader(f, delimiter="\t")}
    with open(args.filtered / "sessions.tsv") as f:
        total = sum(int(r["human"]) for r in csv.DictReader(f, delimiter="\t"))
    corrections, lessons = set(), []
    for path in args.reads:
        data = json.loads(path.read_text())
        corrections |= pairs(data.get("corrections", []))
        lessons += data.get("lessons", [])
    caught = corrections & flagged
    print(f"human messages: {total}; regex flagged {len(flagged)}; "
          f"readers marked {len(corrections)} as corrections")
    print(f"recall: regex caught {len(caught)} of {len(corrections)} corrections")
    print(f"precision: {len(caught)} of {len(flagged)} regex hits are corrections")
    hit = [l for l in lessons if pairs(l.get("evidence", [])) & flagged]
    print(f"lessons: {len(hit)} of {len(lessons)} have evidence the regex flagged")


if __name__ == "__main__":
    main()
