#!/usr/bin/env python3
"""Run the prototype over every book example; print the ones that do not check.
(netidx-admin is `mutants.py`, which gives it the annotations it needs.)

usage: examples.py [-v]
"""
import glob, os, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
root = os.path.dirname(here)
spike = os.environ.get('SPIKE', os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike'))
files = sorted(glob.glob(f'{root}/book/src/examples/**/*.gx', recursive=True))
ok = 0
for f in files:
    r = subprocess.run([spike, f], capture_output=True, text=True)
    line = r.stdout.strip() or f'crash\t{r.stderr.strip()[-200:]}'
    if line.startswith('ok'):
        ok += 1
    if not line.startswith('ok') or '-v' in sys.argv:
        print(os.path.relpath(f, root), line.replace('\t', '  '), sep='\n    ')
print(ok, 'of', len(files), 'check')
