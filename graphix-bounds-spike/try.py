#!/usr/bin/env python3
"""One program a line on stdin: the prototype's verdict, then the compiler's
with the check by bounds switched on, with how many cells the old relation
bound and the last line it printed.

usage: try.py [-t]      -t traces what binds a cell the old way
"""
import os, re, subprocess, sys, tempfile

spike = os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike')
graphix = os.environ.get('GRAPHIX', os.path.expanduser('~/tmp/target/debug/graphix'))
progs = [l.rstrip('\n') for l in sys.stdin if l.strip()]
proto = subprocess.run([spike], input='\n'.join(progs) + '\n',
                       capture_output=True, text=True).stdout.split('\n')
tmp = tempfile.mkdtemp(dir=os.path.expanduser('~/tmp'))
for i, prog in enumerate(progs):
    path = f'{tmp}/p.gx'
    open(path, 'w').write(prog + '\n')
    audit = 'trace' if '-t' in sys.argv else 'count'
    env = dict(os.environ, GRAPHIX_BOUNDS='1', GRAPHIX_BOUNDS_AUDIT=audit)
    r = subprocess.run(['prlimit', '--as=4294967296', 'timeout', '-s', 'KILL', '30',
                        graphix, '--no-cache', '--check', path],
                       capture_output=True, text=True, env=env)
    out = r.stdout + r.stderr
    old = sum(int(n) for n in re.findall(r'(\d+) bound by the old relation', out))
    lines = [l for l in out.strip().split('\n') if 'bounds audit' not in l]
    verdict = 'CRASH' if r.returncode < 0 or r.returncode > 100 or 'panicked' in out else \
        'ok' if r.returncode == 0 else 'no'
    want = proto[i].split('\t')
    mark = ' ' if verdict == want[0] else '!'
    print(f'{mark} {prog[:110]}\n    prototype: {want[0]} {" ".join(want[1:])[:90]}\n    tree: {verdict} (old binds {old}) {(lines[-1:] or [""])[0].strip()[:160]}')
    if '-t' in sys.argv:
        print('\n'.join(l for l in out.split('\n') if 'bounds audit' in l or 'graphix_' in l)[:3000])
os.unlink(f'{tmp}/p.gx'); os.rmdir(tmp)
