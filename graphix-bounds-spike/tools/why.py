#!/usr/bin/env python3
"""Print each failing fixture's first failing mode's output, cut short.

usage: why.py <run output file> [fixture-substring..] [-l LINES]
"""
import re, sys

text = open(sys.argv[1], errors='replace').read()
lines = int(sys.argv[sys.argv.index('-l') + 1]) if '-l' in sys.argv else 8
want = [a for a in sys.argv[2:] if not a.startswith('-') and not a.isdigit()]
seen = set()
for m in re.finditer(r'---- (\S+)::(interp|jit_par|jit|par) stdout ----\n(.*?)(?=\n---- |\nfailures:)',
                     text, re.S):
    name, mode, body = m.groups()
    if name in seen or (want and not any(w in name for w in want)):
        continue
    seen.add(name)
    src = re.findall(r'source: "((?:[^"\\]|\\.)*)"', body)
    keep = [l for l in body.split('\n') if l.strip() and 'testing.rs' not in l
            and not l.strip().startswith(('Error {', 'context:', '},', ')', 'Err(c)'))]
    print(f'== {name.split("::")[-1]} ({mode})')
    for l in (src[-1:] if src else []) + keep[:lines]:
        print('   ', l[:260])
