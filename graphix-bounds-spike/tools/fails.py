#!/usr/bin/env python3
"""Sort the failures of a run_mods.sh output directory by what went wrong,
from each failing fixture's interp run (its first mode where interp passed).

usage: fails.py <mods dir> [-k KIND] [-n]
kinds: check-build (the check accepts what the build refuses), run-bind (a
call's callee did not bind at run time), settled (no value came), verdict
(a refusal or value the fixture or manifest does not expect), fusion (a
fusion expectation), panic (anything else that panicked), other
-n prints the fixture names of the kind alone
"""
import collections, glob, os, re, sys

out = sys.argv[1]
want = sys.argv[sys.argv.index('-k') + 1] if '-k' in sys.argv else None
blocks = {}
for f in sorted(glob.glob(os.path.join(out, '*.txt'))):
    if os.path.basename(f) in ('summary.txt', 'bless.txt'):
        continue
    text = open(f, errors='replace').read()
    for m in re.finditer(r'---- (\S+)::(interp|jit_par|jit|par) stdout ----\n(.*?)(?=\n---- |\nfailures:)',
                         text, re.S):
        name, mode, body = m.groups()
        blocks.setdefault(name, {})[mode] = body


def kind(body):
    if 'the check accepts what the build refuses' in body:
        return 'check-build'
    if 'binding the callee failed' in body or 'run-time bind' in body:
        return 'run-bind'
    if 'settled without an update' in body:
        return 'settled'
    if 'fuse: ' in body and 'expected' in body:
        return 'fusion'
    if 'assertion failed: expected(' in body:
        return 'verdict'
    if 'panicked' in body:
        return 'panic'
    return 'other'


rows = []
for name, modes in sorted(blocks.items()):
    mode = next((m for m in ('interp', 'par', 'jit', 'jit_par') if m in modes), None)
    body = modes[mode]
    k = kind(body)
    line = next((l for l in body.split('\n') if l.strip()), '')
    rows.append((name, k, mode, line))

if want:
    for name, k, mode, line in rows:
        if k == want:
            print(name if '-n' in sys.argv else f'{name}\t{mode}\t{line[:200]}')
else:
    print(dict(collections.Counter(k for _, k, _, _ in rows)), 'of', len(rows))
