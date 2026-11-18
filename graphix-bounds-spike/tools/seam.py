#!/usr/bin/env python3
"""Group the build's bindings by the old relation (GRAPHIX_BOUNDS_AUDIT=trace
output of a run under the check by bounds) by the frames that made them.

usage: seam.py <trace output> [-d DEPTH]
Each binding is keyed by the first DEPTH graphix_compiler frames below the
relation (default 3); the groups print most frequent first.
"""
import collections, re, sys

text = open(sys.argv[1], errors='replace').read()
depth = int(sys.argv[sys.argv.index('-d') + 1]) if '-d' in sys.argv else 3
groups = collections.Counter()
for block in text.split('bounds audit: the old relation binds (build) ')[1:]:
    frames = re.findall(r'^\s+\d+: (\S+)', block, re.M)
    mine = []
    for f in frames:
        if 'graphix_compiler' not in f or 'typ::' in f:
            continue
        f = re.sub(r'<graphix_rt::rt::GXRt<graphix_rt::NoExt>>', '', f)
        f = re.sub(r'::\{closure#\d+\}', '', f)
        if not mine or mine[-1] != f:
            mine.append(f)
        if len(mine) == depth:
            break
    groups[' <- '.join(mine)] += 1
for k, n in groups.most_common():
    print(f'{n:6} {k[:300]}')
