#!/usr/bin/env python3
# package-01: removal cascade reads the shell [features] graph, which disagrees
# with the crate deps that decide registration.
#
# Mirrors graphix-package's feature_edges/installed_dependents (the cascade
# `graphix package remove <p>` offers, over graphix-shell/Cargo.toml
# [features]) and compares it with what stays registered: defpackage!'s
# register registers every graphix-package-* [dependencies] entry of a crate,
# so a removed package whose remaining dependent is compiled in is still
# registered.
#
# command: python3 -I design/review-2026-10-12/repro/package-01.py /home/eric/proj/graphix
# expected: for each removal, cascade == crate-dep dependents, nothing still registered
# observed (HEAD 7324207f):
#   remove sys: cascade ['hbs','json','pack','toml','tui','xls'];
#     crate-dep dependents ['gui','tui']; needlessly removed ['hbs','json','pack','toml','xls'];
#     still registered by ['gui']
#   remove array: cascade ['list','tui']; crate-dep dependents ['http','list','tui'];
#     still registered by ['http']
#   remove str: cascade ['gui']; crate-dep dependents ['gui','tui']; still registered by ['tui']
import sys, tomllib, pathlib
ws = pathlib.Path(sys.argv[1])
feats = tomllib.loads((ws / "graphix-shell/Cargo.toml").read_text())["features"]
installed = list(feats["all"])
fedges = {f: {e for e in v if ":" not in e and "/" not in e}
          for f, v in feats.items() if isinstance(v, list)}
cedges = {}
for p in installed:
    c = tomllib.loads((ws / f"stdlib/graphix-package-{p}/Cargo.toml").read_text())
    cedges[p] = {k[16:] for k in c.get("dependencies", {})
                 if k.startswith("graphix-package-")} - {"core"}
def depends_on(p, t, edges):
    stack, seen = [p], set()
    while stack:
        cur = stack.pop()
        if cur in seen: continue
        seen.add(cur)
        d = edges.get(cur, set())
        if t in d: return True
        stack.extend(d)
    return False
bad = 0
for target in ["sys", "array", "str"]:
    casc = sorted(p for p in installed if p != target and depends_on(p, target, fedges))
    real = sorted(p for p in installed if p != target and depends_on(p, target, cedges))
    extra = sorted(set(casc) - set(real)); kept = sorted(set(real) - set(casc))
    bad += bool(extra or kept)
    print(f"remove {target}: cascade {casc}; crate-dep dependents {real}; "
          f"needlessly removed {extra}; still registered by {kept}")
sys.exit(1 if bad else 0)
