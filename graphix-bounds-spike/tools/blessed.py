#!/usr/bin/env python3
"""Sort what the failing fixtures gave under the check by bounds (the file
GRAPHIX_BOUNDS_BLESS collected) by what each test expects in its source.

usage: blessed.py <bless file> [-k KIND] [-m MODULE] [-g]   (-g groups the texts)
       blessed.py <bless file> -M      the manifest lines of the `message` kind:
                                       fixtures both checks refuse, in other words
kinds: message (both refuse, other words), accepted (the test expects a
refusal, bounds accepts), refused (the test expects a value, bounds refuses),
value (both run, another value)
"""
import collections, glob, os, re, sys

root = os.path.expanduser('~/proj/graphix-lanes/bounds/stdlib/graphix-tests/src')
kind_wanted = sys.argv[sys.argv.index('-k') + 1] if '-k' in sys.argv else None
mod_wanted = sys.argv[sys.argv.index('-m') + 1] if '-m' in sys.argv else None

expect = {}
for f in glob.glob(f'{root}/**/*.rs', recursive=True):
    src = open(f).read()
    mod = os.path.relpath(f, root)[:-3].replace('/', '::').replace('::mod', '')
    for m in re.finditer(r'run!\(\s*(\w+)\s*,', src):
        body = src[m.end():m.end() + 8000]
        depth, end = 1, 0
        for i, c in enumerate(body):
            depth += c == '('; depth -= c == ')'
            if depth == 0:
                end = i
                break
        body = body[:end]
        r = re.search(r'refused(?:_renumbered)?\(\s*"((?:[^"\\]|\\.)*)"', body)
        expect[f'graphix_tests::{mod}::{m.group(1)}'] = ('refuse', r.group(1)) if r else ('value', '')

rows = []
seen = set()
for line in open(sys.argv[1]):
    parts = line.rstrip('\n').split('\t', 2)
    if len(parts) != 3 or parts[0] in seen:
        continue
    seen.add(parts[0])
    name, got, text = parts
    exp = expect.get(name, ('?', ''))
    kind = {('refuse', 'refuses'): 'message', ('refuse', 'accepts'): 'accepted',
            ('value', 'refuses'): 'refused', ('value', 'accepts'): 'value'}.get((exp[0], got), 'unknown')
    rows.append((name, kind, exp[1], text))

if '-M' in sys.argv:
    for name, kind, exp, text in sorted(rows):
        if kind == 'message':
            print(f'{name}\trefuses\t{text}')
    sys.exit(0)
tally = collections.Counter(r[1] for r in rows)
print(dict(tally), 'of', len(rows))
def norm(t):
    return re.sub(r"\d+", "N", re.sub(r"'_\d+", "'v", t))[:110]
sel = [r for r in rows if (kind_wanted is None or r[1] == kind_wanted) and (mod_wanted is None or mod_wanted in r[0])]
if '-g' in sys.argv:
    for t, n in collections.Counter(norm(r[3]) for r in sel).most_common(60):
        print(f'{n:5} {t}')
elif kind_wanted:
    for name, kind, exp, text in sel:
        print(f'{name.replace("graphix_tests::", "")}\n    expects: {exp[:140]}\n    got:     {text[:220]}')
