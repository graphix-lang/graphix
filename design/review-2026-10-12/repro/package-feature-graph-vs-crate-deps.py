#!/usr/bin/env python3
# The package manager's removal cascade (graphix-package/src/lib.rs
# feature_edges / installed_dependents) reads dependencies from
# graphix-shell's [features]; what a build registers follows each stdlib
# crate's own [dependencies] (defpackage!'s register registers every
# graphix-package-* dependency). Prints every disagreement.
# usage: python3 -I package-feature-graph-vs-crate-deps.py /path/to/graphix
import sys, tomllib, pathlib
ws = pathlib.Path(sys.argv[1])
shell = tomllib.loads((ws / "graphix-shell/Cargo.toml").read_text())
feats = shell["features"]
bad = 0
for name in feats["all"]:
    edges = {e for e in feats[name] if ":" not in e and "/" not in e}
    crate = tomllib.loads((ws / f"stdlib/graphix-package-{name}/Cargo.toml").read_text())
    deps = {k[len("graphix-package-"):] for k in crate.get("dependencies", {})
            if k.startswith("graphix-package-")} - {"core"}
    if edges != deps:
        bad += 1
        print(f"{name}: feature edges {sorted(edges)} crate deps {sorted(deps)}")
sys.exit(1 if bad else 0)
