#!/usr/bin/env python3
"""Compare the ported solver (graphix-types/src/typ/bounds.rs) with the
prototype's, program by program: the prototype replays each of its solver
calls into the port (SPIKE_PORT), and the two verdicts must agree. A program
that uses something not replayed yet is not compared.

usage: port.py [-v]      over the skill's `graphix,cases` lines and the probes
"""
import collections, glob, os, re, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
root = os.path.dirname(here)
spike = os.environ.get('SPIKE', os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike'))
progs = []
for f in sorted(glob.glob(f'{root}/ide/skills/graphix-lang/*.md')):
    fence = False
    for n, line in enumerate(open(f), 1):
        t = line.rstrip('\n')
        if t.startswith('```'):
            fence = t.startswith('```graphix,cases')
            continue
        m = fence and (re.match(r'ok (.*)$', t) or re.match(r'no "(?:[^"\\]|\\.)*" (.*)$', t))
        if m:
            progs.append((f'{os.path.basename(f)}:{n}', m.group(1)))
for name in ('probes', 'probes2'):
    for n, p in enumerate(open(f'{here}/{name}.txt').read().split('\n')[:-1], 1):
        progs.append((f'{name}.txt:{n}', p))

out = []
env = dict(os.environ, SPIKE_PORT='1')
while len(out) < len(progs):
    r = subprocess.run([spike], input='\n'.join(p for _, p in progs[len(out):]) + '\n',
                       capture_output=True, text=True, env=env).stdout.split('\n')[:-1]
    out += r
    if len(out) < len(progs):
        out.append('crash\t\t\t\tcrash')
# where the port is meant to differ from the prototype, and why
MEANT = {
    'generics.md:269': 'an abstract type\'s parameter is one type (the tree\'s rule); the prototype let it widen',
    'generics.md:282': 'the same; today refuses this one too',
}
tally, why = collections.Counter(), collections.Counter()
for (at, prog), line in zip(progs, out):
    cols = line.split('\t')
    proto, port = cols[0], (cols[4] if len(cols) > 4 else 'skip no verdict')
    if proto in ('skip', 'crash') or port.startswith('skip') or port == 'crash':
        tally['not compared'] += 1
        why[port if proto not in ('skip', 'crash') else f'prototype: {proto}'] += 1
        continue
    same = proto == port.split(' ')[0]
    if not same and at in MEANT:
        tally['differs as meant'] += 1
        continue
    tally['same' if same else 'differs'] += 1
    if not same or '-v' in sys.argv:
        print(f'{"same" if same else "DIFFERS"} {at}: {prog}\n    prototype: {proto} {cols[1][:160]}\n    port:      {port[:200]}')
print(dict(tally), 'of', len(progs))
print('not compared for:', dict(why.most_common()))
