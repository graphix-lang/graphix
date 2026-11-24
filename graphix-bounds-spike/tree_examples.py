#!/usr/bin/env python3
"""Check every book example with the check by bounds switched on
(GRAPHIX_BOUNDS=1), and with it off: an example the old check accepts must
be accepted, by the solver alone.

usage: tree_examples.py [-v] [-j N] [-k TEXT]
"""
import collections, concurrent.futures, glob, os, re, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
root = os.path.dirname(here)
graphix = os.environ.get('GRAPHIX', os.path.expanduser('~/tmp/target/debug/graphix'))
jobs = int(sys.argv[sys.argv.index('-j') + 1]) if '-j' in sys.argv else 6
files = sorted(glob.glob(f'{root}/book/src/examples/**/*.gx', recursive=True))
if '-k' in sys.argv:
    files = [f for f in files if sys.argv[sys.argv.index('-k') + 1] in f]

def check(path, bounds):
    env = dict(os.environ)
    if bounds:
        env.update(GRAPHIX_BOUNDS='1', GRAPHIX_BOUNDS_AUDIT='count')
    r = subprocess.run(['prlimit', '--as=6442450944', 'timeout', '-s', 'KILL', '60',
                        graphix, '--no-cache', '--check', path],
                       capture_output=True, text=True, env=env, cwd=os.path.dirname(path))
    out = r.stdout + r.stderr
    old = sum(int(n) for n in re.findall(r'(\d+) bound by the old relation', out))
    lines = [l for l in out.strip().split('\n') if 'bounds audit' not in l and l.strip()]
    crash = r.returncode < 0 or r.returncode > 100 or 'panicked' in out
    return ('crash' if crash else 'ok' if r.returncode == 0 else 'no', old, lines[-1:] or [''])

def run(path):
    return check(path, False), check(path, True)

with concurrent.futures.ThreadPoolExecutor(jobs) as pool:
    got = list(pool.map(run, files))
tally = collections.Counter()
for path, ((today, _, _), (verdict, old, last)) in zip(files, got):
    name = os.path.relpath(path, f'{root}/book/src/examples')
    kind = 'same' if verdict == today else 'differs'
    tally[kind] += 1
    tally['by the solver alone'] += kind == 'same' and old == 0
    if kind == 'differs' or (old and '-v' in sys.argv):
        print(f'{name}: today {today}, by bounds {verdict} (old binds {old}) {last[0].strip()[:220]}')
print(dict(tally), 'of', len(files))
