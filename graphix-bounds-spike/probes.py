#!/usr/bin/env python3
"""Run the prototype over probes.txt and probes2.txt and compare each verdict
(and the type or message with it) with the one recorded beside the file.

usage: probes.py [--bless]
"""
import os, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
spike = os.environ.get('SPIKE', os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike'))
bad = 0
for name in ('probes', 'probes2'):
    progs = open(f'{here}/{name}.txt').read().split('\n')[:-1]
    out = subprocess.run([spike], input='\n'.join(progs) + '\n', capture_output=True, text=True).stdout
    got = ['\t'.join(l.split('\t')[:2]) for l in out.split('\n')[:-1]]
    want_path = f'{here}/{name}.expected'
    if '--bless' in sys.argv:
        open(want_path, 'w').write('\n'.join(got) + '\n')
        continue
    want = open(want_path).read().split('\n')[:-1]
    for n, (p, g, w) in enumerate(zip(progs, got, want), 1):
        if g != w:
            bad += 1
            print(f'{name}.txt:{n}: {p}\n    was: {w}\n    now: {g}')
    if len(got) != len(want):
        bad += 1
        print(f'{name}: {len(got)} verdicts, {len(want)} recorded')
print('blessed' if '--bless' in sys.argv else f'{bad} changed')
