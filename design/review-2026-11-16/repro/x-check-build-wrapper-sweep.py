#!/usr/bin/env python3
# Title: the gate's fixtures, wrapped as a labeled default or a function body, find
#        check/build disagreements no gate or fuzzer mutation looks for
#
# Command: timeout -s KILL 170 python3 design/review-2026-11-16/repro/x-check-build-wrapper-sweep.py dflt lang/types.rs 850
#          timeout -s KILL 170 python3 design/review-2026-11-16/repro/x-check-build-wrapper-sweep.py fn lang/select_undecided.rs 100
#          (wrapper: dflt | fn | nest;  file under stdlib/graphix-tests/src;  index of the first
#          raw-string fixture to take; 150 are taken)
# Expected: no DISAGREE line: a program the check accepts builds, whatever it is wrapped in.
# Observed: the first prints 11 lines, fixtures 860..870, e.g.
#   DISAGREE 860: check accepts, build refuses: 7: fn(x: i64) -> i64 could go to any of 2
#   members of the union fn(x: i64) -> i64, which nothing has decided, ..
# the second 3 lines, fixtures 108, 109, 111, e.g.
#   DISAGREE 108: check accepts, build refuses: 2: an instance at fn(u__: i64) ->
#   [Array<_>, Array<i64>] of a definition typed fn(u__: i64) -> [Array<i64>, Array<'_8206: unbound>]
# Over every raw-string fixture of stdlib/graphix-tests/src (5988 programs) the sweep found
# 17 such programs as a labeled default (attribute assertions aside) and 4 as a function
# body; the fixtures as written disagree nowhere.
import os, re, subprocess, sys, tempfile, time
G = os.environ.get('G', '/home/eric/tmp/review-2026-11-16/bin/graphix')
ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), '../../..', 'stdlib/graphix-tests/src')
WRAP = {
    'dflt': lambda p: 'let w__ = |#d__ = { let u__ = 0; %s }, x__: i64| x__;\nw__(1)\n' % p,
    'fn': lambda p: 'let w__ = |u__: i64| { let u2__ = u__; %s };\nw__(0)\n' % p,
    'nest': lambda p: 'let w__ = |u__| { let v__ = |q__| { let q2__ = q__; %s }; v__(u__) };\nw__(0)\n' % p,
}
ATTR = re.compile(r'#\[(native|sync|async|pure|tail_recursive|parallel)\]')
def run(args, f):
    env = dict(os.environ, GRAPHIX_ELAB_AUDIT='1')
    p = subprocess.run(['timeout', '-s', 'KILL', '40', G, '-n', '--no-cache'] + args + [f],
                       capture_output=True, text=True, env=env, errors='replace')
    return p.returncode, p.stderr
wrap, src, first = WRAP[sys.argv[1]], sys.argv[2], int(sys.argv[3])
text = open(os.path.join(ROOT, src)).read()
progs = [m.group(1).strip() for m in re.finditer(r'r#"(.*?)"#', text, re.S)]
t0, n = time.time(), 0
with tempfile.TemporaryDirectory() as d:
    for i, p in enumerate(progs[first:first + 150], first):
        if time.time() - t0 > 150:
            break
        f = os.path.join(d, 'p.gx')
        open(f, 'w').write(wrap(p))
        n += 1
        if run(['--check'], f)[0] != 0:
            continue
        rc, err = run(['--expand'], f)
        last = [l.strip() for l in err.splitlines() if l.strip() and 'ELAB-AUDIT' not in l][-1:]
        if rc != 0 and not ATTR.search(err):
            print('DISAGREE %d: check accepts, build refuses: %s' % (i, (last or ['?'])[0][:160]))
print('%d fixtures' % n)
