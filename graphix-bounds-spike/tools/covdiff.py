#!/usr/bin/env python3
"""Which lines of the check run with the check by bounds off and not with it on.

usage: covdiff.py <off.lcov> <on.lcov> [-f FILE]   (-f lists the functions of one file)

Both files come from `llvm-cov export -format=lcov` over the same binary, one
profile from runs with GRAPHIX_BOUNDS unset and one with it set, over the same
programs. A line "only off" ran in today's check and never under the check by
bounds on this corpus: what the new check does not need, as far as these
programs show. Test modules are left out (lines from the first `mod tests`).
"""
import collections, os, re, sys

def read(path):
    hits, cur = collections.defaultdict(dict), None
    for line in open(path):
        if line.startswith('SF:'):
            cur = line[3:].strip()
        elif line.startswith('DA:'):
            n, c = line[3:].split(',')[:2]
            hits[cur][int(n)] = hits[cur].get(int(n), 0) + int(c)
    return hits

off, on = read(sys.argv[1]), read(sys.argv[2])
want = sys.argv[sys.argv.index('-f') + 1] if '-f' in sys.argv else None

def tests_from(path):
    try:
        for i, l in enumerate(open(path), 1):
            if re.match(r'(pub(\(crate\))? )?mod tests', l):
                return i
    except OSError:
        pass
    return 10 ** 9

rows = []
tot = collections.Counter()
for f in sorted(off):
    cut = tests_from(f)
    lines = [n for n in off[f] if n < cut]
    a = {n for n in lines if off[f][n] > 0}
    b = {n for n in lines if on.get(f, {}).get(n, 0) > 0}
    short = f.split('/graphix-lanes/bounds/')[-1]
    rows.append((len(a - b), len(a & b), len(b - a), len(lines), short))
    for k, v in zip(('only_off', 'both', 'only_on', 'instrumented'), rows[-1][:4]):
        tot[k] += v
    if want and short.endswith(want):
        src = open(f).read().split('\n')
        fn, by = None, collections.Counter()
        for i, l in enumerate(src, 1):
            m = re.match(r'\s*(pub(\([a-z]+\))? )?(async )?fn (\w+)', l)
            if m:
                fn = m.group(4)
            if i in (a - b):
                by[fn] += 1
        for name, n in by.most_common(40):
            print(f'{n:6} {name}')
if not want:
    print(f'{"only off":>9} {"both":>7} {"only on":>8} {"lines":>7}  file')
    for r in sorted(rows, reverse=True)[:45]:
        print(f'{r[0]:9} {r[1]:7} {r[2]:8} {r[3]:7}  {r[4]}')
    print(dict(tot))
