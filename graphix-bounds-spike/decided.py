#!/usr/bin/env python3
"""Compare the type the prototype decides for a program's last expression with
the type today's REPL prints for it (`-: T`).

usage: decided.py [-v]      over the skill's `ok` cases and the probe lists
Each program runs in a REPL of its own, memory capped, killed after 10 s. A
type that holds a function is not compared: the two printers differ there.
"""
import glob, os, re, subprocess, sys, collections

here = os.path.dirname(os.path.abspath(__file__))
root = os.path.dirname(here)
spike = os.environ.get('SPIKE', os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike'))
graphix = os.environ.get('GRAPHIX', os.path.expanduser('~/tmp/target/debug/graphix'))
verbose = '-v' in sys.argv

progs = []
for f in sorted(glob.glob(f'{root}/ide/skills/graphix-lang/*.md')):
    fence = False
    for line in open(f):
        t = line.rstrip('\n')
        if t.startswith('```'):
            fence = t.startswith('```graphix,cases')
        elif fence and t.startswith('ok '):
            progs.append(t[3:])
for f in ('probes.txt', 'probes2.txt'):
    progs += [l.rstrip('\n') for l in open(f'{here}/{f}') if l.strip()]

def split(s):
    """The top-level comma-separated parts of s."""
    parts, depth, cur = [], 0, ''
    for c in s:
        if c in '([{<':
            depth += 1
        elif c in ')]}>':
            depth -= 1
        if c == ',' and depth == 0:
            parts.append(cur.strip()); cur = ''
        else:
            cur += c
    if cur.strip():
        parts.append(cur.strip())
    return parts

def norm(s):
    s = s.strip()
    while True:  # a printed variable with what it stands for: keep what it stands for
        m = re.match(r"'[A-Za-z_0-9]+: (.*)$", s)
        if not m or m.group(1).startswith('unbound'):
            break
        s = m.group(1)
    if s.startswith('[') and s.endswith(']'):
        ms = []
        for p in split(s[1:-1]):
            p = norm(p)
            ms += split(p[1:-1]) if p.startswith('[') and p.endswith(']') else [p]
        ms = sorted(set(ms))
        return ms[0] if len(ms) == 1 else '[' + ', '.join(ms) + ']'
    for o, c in (('(', ')'), ('{', '}')):
        if s.startswith(o) and s.endswith(c):
            return o + ', '.join(norm_field(p) for p in split(s[1:-1])) + c
    m = re.match(r'([A-Za-z`_][A-Za-z0-9_:]*)([<(])(.*)[>)]$', s)
    if m:
        close = '>' if m.group(2) == '<' else ')'
        return m.group(1) + m.group(2) + ', '.join(norm(p) for p in split(m.group(3))) + close
    return {'_': '_', "'_: unbound": '_'}.get(s, s)

def norm_field(p):
    m = re.match(r'([a-z_][A-Za-z0-9_]*): (.*)$', p)
    return f'{m.group(1)}: {norm(m.group(2))}' if m else norm(p)

out = subprocess.run([spike], input='\n'.join(progs) + '\n', capture_output=True, text=True).stdout.split('\n')
tally = collections.Counter()
for prog, line in zip(progs, out):
    got, mine = (line.split('\t') + [''])[:2]
    if got != 'ok':
        tally['not accepted by the prototype'] += 1
        continue
    try:
        r = subprocess.run(['systemd-run', '--user', '--scope', '-q', '--collect', '-p', 'MemoryMax=4G',
                            '-p', 'MemorySwapMax=0', 'timeout', '-s', 'KILL', '10', graphix, '--no-cache'],
                           input=prog + '\n', capture_output=True, text=True).stdout
    except Exception as e:
        r = ''
    shown = [l[3:] for l in r.split('\n') if l.startswith('-: ')]
    if not shown:
        tally['no type from the REPL'] += 1
        continue
    today = shown[-1]
    if 'fn(' in today or 'fn(' in mine or 'fn<' in today:
        tally['holds a function'] += 1
        continue
    a, b = norm(today), norm(mine)
    kind = 'same' if a == b else 'differs'
    tally[kind] += 1
    if kind == 'differs' or verbose:
        print(f'{kind}: {prog}\n    today: {today}\n    spike: {mine}')
print(dict(tally), 'of', len(progs))
