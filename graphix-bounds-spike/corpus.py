#!/usr/bin/env python3
"""Run the prototype over the `graphix,cases` lines of the language skill and
compare each verdict with the one the gate checks today.

usage: corpus.py [-v] [FILE.md ..]     (default: ide/skills/graphix-lang/*.md)
A cases line is `ok <program>` or `no "<text>" <program>`.
"""
import glob, os, re, subprocess, sys, collections

here = os.path.dirname(os.path.abspath(__file__))
root = os.path.dirname(here)
spike = os.environ.get('SPIKE', os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike'))
verbose = '-v' in sys.argv
files = [a for a in sys.argv[1:] if a != '-v'] or sorted(glob.glob(f'{root}/ide/skills/graphix-lang/*.md'))

cases = []
for f in files:
    fence = False
    for n, line in enumerate(open(f), 1):
        t = line.rstrip('\n')
        if t.startswith('```'):
            fence = t.startswith('```graphix,cases')
            continue
        if not fence or not t.strip():
            continue
        m = re.match(r'ok (.*)$', t)
        if m:
            cases.append((f, n, 'ok', '', m.group(1)))
            continue
        m = re.match(r'no "((?:[^"\\]|\\.)*)" (.*)$', t)
        if m:
            cases.append((f, n, 'no', m.group(1), m.group(2)))

out = []
while len(out) < len(cases):
    # a program that kills the prototype (a stack overflow) is a crash; go on after it
    r = subprocess.run([spike], input='\n'.join(c[4] for c in cases[len(out):]) + '\n',
                       capture_output=True, text=True).stdout.split('\n')[:-1]
    out += r
    if len(out) < len(cases):
        out.append('crash\t')
tally, skips = collections.Counter(), collections.Counter()
for (f, n, want, text, prog), line in zip(cases, out):
    got, msg = (line.split('\t') + [''])[:2]
    kind = (got if got in ('skip', 'crash') else 'agree' if got == want
            else 'accepts' if got == 'ok' else 'refuses')
    tally[kind] += 1
    if kind == 'skip':
        skips[msg] += 1
    if kind in ('accepts', 'refuses', 'crash') or (verbose and kind != 'skip'):
        print(f'{kind:8} {os.path.basename(f)}:{n}: {prog}\n         today: {want} {text}\n         spike: {got} {msg}')
print(dict(tally), 'of', len(cases))
print('skipped for:', dict(skips.most_common()))
