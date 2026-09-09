#!/usr/bin/env python3
"""Summarize GRAPHIX_PROFILE=1 GRAPHIX_PROFILE_INSTANCES=1 logs.

Groups are measurements of repeated work, not valid body-cache keys.
The first completed construction in each group is retained. Missing
signatures/callback vectors are never merged in the refined groups.
Callback vectors can themselves contain unknown entries.
"""

import argparse
import json
import re
from collections import defaultdict
from pathlib import Path

from profile import read_profile


def read_instances(path):
    profile = read_profile(path)
    roots = {(r["thread"], r["start_ns"]): r for r in profile["roots"]}
    for root in roots.values():
        root["instances"] = []
    for line in Path(path).read_text().splitlines():
        if not line.startswith("INSTANCE "):
            continue
        fields, label = line.split(" label=", 1)
        row = dict(re.findall(r"(\w+)=(\S+)", fields))
        thread = row.pop("thread")
        row = {k: int(v) for k, v in row.items()}
        root = roots[thread, row.pop("root_ns")]
        row["label"] = json.loads(label)
        root["instances"].append(row)
    for root in roots.values():
        rows = root["instances"]
        for field, phase in [("graph", "InstanceGraph"), ("check", "InstanceCheck")]:
            metric = root["phases"].get(phase, {})
            for suffix, metric_key in [("ns", "self_ns"), ("calls", "calls")]:
                measured = sum(r[f"{field}_{suffix}"] for r in rows)
                expected = metric.get(metric_key, 0)
                if measured != expected:
                    raise ValueError(
                        f"instance accounting mismatch: {path}, root {root['start_ns']}, "
                        f"{phase} {suffix}: {measured} != {expected}"
                    )
    return profile


def summarize(rows, level):
    groups = defaultdict(list)
    for row in rows:
        key = (row["definition"],)
        if not row["label"]:
            key += ("unknown-definition", row["id"])
        if level >= 1:
            key += (row["signature"],)
            if not row["signature"]:
                key += ("unknown-signature", row["id"])
        if level >= 2:
            key += (row["callbacks"],)
            if not row["callbacks"]:
                key += ("unknown-callbacks", row["id"])
        groups[key].append(row)
    repeated = []
    for group in groups.values():
        repeated.extend(sorted(group, key=lambda r: r["id"])[1:])
    top = defaultdict(lambda: dict(instances=0, graph_ns=0, check_ns=0))
    for row in repeated:
        t = top[row["label"]]
        t["instances"] += 1
        t["graph_ns"] += row["graph_ns"]
        t["check_ns"] += row["check_ns"]
    return dict(
        instances=len(rows),
        groups=len(groups),
        repeated=len(repeated),
        graph_ns=sum(r["graph_ns"] for r in repeated),
        check_ns=sum(r["check_ns"] for r in repeated),
        top=sorted(
            [dict(label=k, **v) for k, v in top.items()],
            key=lambda r: -(r["graph_ns"] + r["check_ns"]),
        ),
    )


def report(roots):
    levels = ("definition", "closed_signature", "callback_sources")
    result = {}
    for level, name in enumerate(levels):
        summaries = [summarize(r["instances"], level) for r in roots]
        result[name] = {
            k: sum(s[k] for s in summaries)
            for k in ("instances", "groups", "repeated", "graph_ns", "check_ns")
        }
        result[name]["top"] = sorted(
            [t for s in summaries for t in s["top"]],
            key=lambda r: -(r["graph_ns"] + r["check_ns"]),
        )
    result["total_graph_ns"] = sum(
        i["graph_ns"] for r in roots for i in r["instances"]
    )
    result["total_check_ns"] = sum(
        i["check_ns"] for r in roots for i in r["instances"]
    )
    result["census_ns"] = sum(
        r["phases"].get("InstanceCensus", {}).get("self_ns", 0) for r in roots
    )
    result["closed"] = sum(
        bool(i["signature"]) for r in roots for i in r["instances"]
    )
    result["with_callbacks"] = sum(
        bool(i["callbacks"]) for r in roots for i in r["instances"]
    )
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("logs", nargs="+", type=Path)
    parser.add_argument("--admin", action="store_true", help="split milestone_timing roots")
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()
    results = {}
    for path in args.logs:
        roots = [r for r in read_instances(path)["roots"] if r["root"] == "Compile"]
        if args.admin:
            app = sorted(
                sorted(roots, key=lambda r: r["duration_ns"])[-2:],
                key=lambda r: r["start_ns"],
            )
            segments = dict(
                registration=[r for r in roots if r["start_ns"] < app[0]["start_ns"]],
                app_on=app[:1], app_off=app[1:],
            )
        else:
            segments = {"compile": roots}
        results[str(path)] = {name: report(rs) for name, rs in segments.items()}
    if args.json:
        print(json.dumps(results, indent=2))
        return
    for path, segments in results.items():
        print(path)
        for name, summary in segments.items():
            print(f"  {name}: graph {summary['total_graph_ns'] / 1e6:.3f} ms, "
                  f"check {summary['total_check_ns'] / 1e6:.3f} ms, "
                  f"census {summary['census_ns'] / 1e6:.3f} ms")
            for level in ("definition", "closed_signature", "callback_sources"):
                s = summary[level]
                print(f"    {level}: {s['groups']}/{s['instances']} groups, "
                      f"{s['repeated']} repeated; "
                      f"graph {s['graph_ns'] / 1e6:.3f} ms, "
                      f"check {s['check_ns'] / 1e6:.3f} ms")
            for row in summary["callback_sources"]["top"][:10]:
                cost = (row["graph_ns"] + row["check_ns"]) / 1e6
                print(f"      {cost:8.3f} ms {row['instances']:5} {row['label']}")


if __name__ == "__main__":
    main()
