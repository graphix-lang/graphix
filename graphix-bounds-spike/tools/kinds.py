#!/usr/bin/env python3
"""Sort the fixtures a bless file lists by what each expects and what the
check by bounds gave. A fixture expects a refusal where its run! names a
refusal predicate.

usage: kinds.py <bless file> [-k KIND] [-g] [-M]
kinds: message (both refuse, other words), accepted (a refusal expected,
bounds accepts, its value or nothing), lost (a value expected, none came), refused (a value expected, bounds refuses), value (both run,
another value), unknown (no run! found)
-g groups the texts, -M prints the manifest lines of the message kind
"""
import collections, glob, os, re, sys

root = os.path.expanduser('~/proj/graphix-lanes/bounds/stdlib/graphix-tests/src')
REFUSAL = re.compile(r'\b\w*refus\w*\s*[(,;]|holds_itself\s*\(|is_err|Err\(')
bodies = {}
for f in glob.glob(f'{root}/**/*.rs', recursive=True):
    src = open(f).read()
    mod = os.path.relpath(f, root)[:-3].replace('/', '::').replace('::mod', '')
    marks = [m for m in re.finditer(r'run!\(\s*(\w+)\s*,', src)]
    for i, m in enumerate(marks):
        end = marks[i + 1].start() if i + 1 < len(marks) else len(src)
        bodies[f'graphix_tests::{mod}::{m.group(1)}'] = src[m.end():end]

rows = []
for line in open(sys.argv[1]):
    parts = line.rstrip('\n').split('\t', 2)
    if len(parts) != 3:
        continue
    name, got, text = parts
    body = bodies.get(name)
    if body is None:
        kind = 'unknown'
    else:
        wants_refusal = bool(REFUSAL.search(body))
        kind = {(True, 'refuses'): 'message', (True, 'accepts'): 'accepted',
                (True, 'settles'): 'accepted', (False, 'refuses'): 'refused',
                (False, 'accepts'): 'value', (False, 'settles'): 'lost'}[(wants_refusal, got)]
    rows.append((name, kind, text))

if '-M' in sys.argv:
    for name, kind, text in sorted(rows):
        if kind == 'message':
            print(f'{name}\trefuses\t{text}')
    sys.exit(0)
print(dict(collections.Counter(r[1] for r in rows)), 'of', len(rows))
want = sys.argv[sys.argv.index('-k') + 1] if '-k' in sys.argv else None
norm = lambda t: re.sub(r"\d+", "N", re.sub(r"'_\d+", "'v", t))[:120]
sel = [r for r in rows if want is None or r[1] == want]
if '-g' in sys.argv:
    for t, n in collections.Counter(norm(r[2]) for r in sel).most_common(70):
        print(f'{n:5} {t}')
elif want:
    for name, kind, text in sel:
        print(f'{name.replace("graphix_tests::", "")}\t{text[:200]}')
