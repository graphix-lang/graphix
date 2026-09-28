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
        elif "module" in fields:
            modules = active[thread].setdefault("modules", {})
            modules.setdefault(fields["module"], {})[fields["mphase"]] = fields["self_ns"]
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
    parser.add_argument(
        "--modules", action="store_true", help="per-module self time and the parallel bound"
    )
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
            if args.modules and root.get("modules"):
                print_modules(root)


FUSION = {
    "Fusion", "ReturnType", "Inputs", "Builtins", "Callees", "Emit", "JitInit",
    "JitBuild", "Clif", "Link", "Finalize", "Freeze", "Normalize", "ExpandRefs",
}
LINK = {"StaticBind", "InstanceGraph", "InstanceCheck", "InstanceCensus"}


def bucket(phase):
    return "fuse" if phase in FUSION else "link" if phase in LINK else "body"


def print_modules(root):
    rows = []
    for name, phases in root["modules"].items():
        b = {"body": 0, "link": 0, "fuse": 0}
        for phase, ns in phases.items():
            b[bucket(phase)] += ns
        rows.append((name, b))
    rows.sort(key=lambda r: -sum(r[1].values()))
    total = root["duration_ns"]
    in_modules = sum(sum(b.values()) for _, b in rows)
    residual = total - in_modules
    print("    module                                    body ms   link ms   fuse ms")
    for name, b in rows:
        if sum(b.values()) < 0.5e6:
            continue
        print(
            f"    {name[:40]:40} {b['body'] / 1e6:9.2f} {b['link'] / 1e6:9.2f}"
            f" {b['fuse'] / 1e6:9.2f}"
        )
    largest = max(sum(b.values()) for _, b in rows)
    print(
        f"    {len(rows)} modules: {in_modules / 1e6:.1f} ms in modules, "
        f"{residual / 1e6:.1f} ms outside any module, largest {largest / 1e6:.1f} ms; "
        f"one module per core bounds the root at {(residual + largest) / 1e6:.1f} ms "
        f"({total / (residual + largest):.1f}x)"
    )


if __name__ == "__main__":
    main()
