#!/usr/bin/env python3
"""Run the skill's `graphix,cases` lines through the compiler with the check by
bounds switched on (GRAPHIX_BOUNDS=1) and compare each verdict with the
prototype's. Also counts, per program, how often the old relation still bound a
cell (GRAPHIX_BOUNDS_AUDIT): a program with none is checked by the solver alone.

usage: tree.py [-v] [-j N] [-k TEXT] [-f FILE]   (run it under a memory-capped scope)
  -k TEXT   only the cases whose place (file.md:line) holds TEXT
  -f FILE   only the cases a line of FILE names by its place: an earlier
            run's output, so the differences alone are run again
"""
import collections, concurrent.futures, glob, os, re, subprocess, sys, tempfile

here = os.path.dirname(os.path.abspath(__file__))
root = os.path.dirname(here)
spike = os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike')
graphix = os.environ.get('GRAPHIX', os.path.expanduser('~/tmp/target/debug/graphix'))
jobs = int(sys.argv[sys.argv.index('-j') + 1]) if '-j' in sys.argv else 8

cases = []
for f in sorted(glob.glob(f'{root}/ide/skills/graphix-lang/*.md')):
    fence = False
    for n, line in enumerate(open(f), 1):
        t = line.rstrip('\n')
        if t.startswith('```'):
            fence = t.startswith('```graphix,cases')
            continue
        m = fence and (re.match(r'ok (.*)$', t) or re.match(r'no "(?:[^"\\]|\\.)*" (.*)$', t))
        if m:
            cases.append((f'{os.path.basename(f)}:{n}', t.split(' ', 1)[0], m.group(1)))

if '-k' in sys.argv:
    text = sys.argv[sys.argv.index('-k') + 1]
    cases = [c for c in cases if text in c[0]]
if '-f' in sys.argv:
    named = set(re.findall(r'\w+\.md:\d+', open(sys.argv[sys.argv.index('-f') + 1]).read()))
    cases = [c for c in cases if c[0] in named]

proto = subprocess.run([spike], input='\n'.join(c[2] for c in cases) + '\n',
                       capture_output=True, text=True).stdout.split('\n')[:-1]
tmp = tempfile.mkdtemp(dir=os.path.expanduser('~/tmp'))

def run(i):
    path = f'{tmp}/c{i}.gx'
    open(path, 'w').write(cases[i][2] + '\n')
    env = dict(os.environ, GRAPHIX_BOUNDS='1', GRAPHIX_BOUNDS_AUDIT='count')
    try:
        # each run may hold 4 GB and run 30 s: a check that runs away is a crash
        r = subprocess.run(['prlimit', '--as=4294967296', 'timeout', '-s', 'KILL', '30',
                            graphix, '--no-cache', '--check', path],
                           capture_output=True, text=True, env=env)
    finally:
        os.unlink(path)
    out = r.stdout + r.stderr
    old = sum(int(n) for n in re.findall(r'(\d+) bound by the old relation', out))
    last = [l for l in out.strip().split('\n') if 'bounds audit' not in l][-1:] or ['']
    ran_away = r.returncode < 0 or r.returncode > 100 or 'memory allocation' in out or 'panicked' in out
    return ('crash' if ran_away else 'ok' if r.returncode == 0 else 'no', old, last[0].strip())

with concurrent.futures.ThreadPoolExecutor(jobs) as pool:
    got = list(pool.map(run, range(len(cases))))
os.rmdir(tmp)

tally, clean = collections.Counter(), 0
for (at, today, prog), line, (verdict, old, last) in zip(cases, proto, got):
    want = line.split('\t')[0]
    if want in ('skip', 'crash'):
        tally['prototype skips'] += 1
        continue
    kind = 'crash' if verdict == 'crash' else 'same' if verdict == want else 'differs'
    if kind == 'crash':
        print(f'CRASH {at}: {prog[:200]}')
    tally[kind] += 1
    clean += kind == 'same' and old == 0
    if kind == 'differs' and '-v' in sys.argv:
        print(f'{at}: {prog[:150]}\n    prototype: {want}   tree by bounds: {verdict} {last[:150]}   (today: {today}; old binds {old})')
print(dict(tally), 'of', len(cases))
print(f'{clean} of the same are checked by the solver alone (no cell bound by the old relation)')
