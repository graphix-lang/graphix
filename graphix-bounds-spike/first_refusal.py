#!/usr/bin/env python3
"""Find the first top-level statement of a .gx file at which the prototype's verdict
is not `ok`: checks each prefix that ends at a `;` outside every bracket.

usage: first_refusal.py FILE.gx
"""
import os, subprocess, sys
spike = os.environ.get('SPIKE', os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike'))
src = sys.argv[1]
lines = open(src).read().split('\n')
d = os.path.dirname(os.path.abspath(src))
depth, last = 0, 0
for n, line in enumerate(lines, 1):
    code = line.split('//')[0]
    depth += sum(code.count(c) for c in '([{') - sum(code.count(c) for c in ')]}')
    if depth != 0 or not code.rstrip().endswith(';'):
        continue
    p = os.path.join(d, '_prefix.gx')
    open(p, 'w').write('\n'.join(lines[:n]) + '\n0\n')
    out = subprocess.run([spike, p], capture_output=True, text=True).stdout
    os.unlink(p)
    if not out.startswith('ok'):
        print(f'lines {last + 1}-{n}:')
        print('\n'.join(lines[last:n]))
        print(out[:700])
        break
    last = n
else:
    print('every prefix checks; the last expression is what fails')
