#!/usr/bin/env python3
"""Read GRAPHIX_PROFILE logs; report roots and their exclusive phase costs."""

import argparse
import json
import re
from pathlib import Path


def read_profile(path):
    roots, intervals, active = [], [], {}
    for line in Path(path).read_text().splitlines():
        if not line.startswith("PROFILE "):
            continue
        fields = dict(re.findall(r"(\w+)=(\S+)", line))
        thread = fields.pop("thread")
        for key, value in fields.items():
            if value.isdigit():
                fields[key] = int(value)
        if "root" in fields:
            fields.update(thread=thread, phases={})
            roots.append(fields)
            active[thread] = fields
        elif "phase" in fields:
            active[thread]["phases"][fields.pop("phase")] = fields
        elif "interval" in fields:
            intervals.append(dict(thread=thread, **fields))
    for root in roots:
        measured = sum(p["self_ns"] for p in root["phases"].values())
        if measured != root["duration_ns"]:
            raise ValueError(f"phase accounting mismatch in {path}: {root}")
    return dict(roots=sorted(roots, key=lambda r: r["start_ns"]), intervals=intervals)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("logs", nargs="+", type=Path)
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--min-ms", type=float, default=1)
    args = parser.parse_args()
    profiles = {str(path): read_profile(path) for path in args.logs}
    if args.json:
        print(json.dumps(profiles, indent=2))
        return
    for path, profile in profiles.items():
        print(path)
        for i, root in enumerate(profile["roots"]):
            if root["duration_ns"] < args.min_ms * 1e6:
                continue
            print(f"  {i}: {root['root']} {root['duration_ns'] / 1e6:.3f} ms")
            print("    phase                calls    self ms   total ms  failures  failed ms")
            for name, p in sorted(
                root["phases"].items(), key=lambda x: -x[1]["self_ns"]
            ):
                print(
                    f"    {name:20} {p['calls']:6} {p['self_ns'] / 1e6:10.3f}"
                    f" {p['total_ns'] / 1e6:10.3f} {p['failed_calls']:9}"
                    f" {p['failed_ns'] / 1e6:10.3f}"
                )


if __name__ == "__main__":
    main()
