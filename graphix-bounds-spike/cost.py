#!/usr/bin/env python3
"""Time the prototype and today's `--check` on generated programs of N blocks.
Each block is a few ordinary definitions that use the block before it.

usage: cost.py [N ..]     (default 50 200 800)
"""
import os, subprocess, sys, tempfile, time

spike = os.environ.get('SPIKE', os.path.expanduser('~/tmp/target/debug/graphix-bounds-spike'))
graphix = os.environ.get('GRAPHIX', os.path.expanduser('~/tmp/target/debug/graphix'))

def block(i):
    p = f'r{i - 1}' if i else '1'
    return f'''type Shape{i} = [`Circle(f64), `Rect(f64, f64), `Tag(string)];
let area{i} = |s: Shape{i}| select s {{ `Circle(r) => r * r, `Rect(w, h) => w * h, `Tag(_) => 0.0 }};
let rows{i} = [{{id: {p}, name: "n{i}", shape: `Circle(1.5)}}, {{id: {i}, name: "m{i}", shape: `Rect(2.0, 3.0)}}];
let total{i} = array::fold(array::map(rows{i}, |r| area{i}(r.shape)), 0.0, |acc, x| acc + x);
let pick{i} = |o: [i64, null], d| select o {{ null as _ => d, n => n + d }};
let r{i} = pick{i}(array::len(rows{i}), {i}) + array::fold(array::map(rows{i}, |r| r.id), 0, |a, b| a + b);
'''

for n in [int(a) for a in sys.argv[1:]] or [50, 200, 800]:
    text = ''.join(block(i) for i in range(n)) + f'r{n - 1}\n'
    with tempfile.NamedTemporaryFile('w', suffix='.gx', delete=False) as f:
        f.write(text)
    t = time.time()
    out = subprocess.run([spike, f.name], capture_output=True, text=True).stdout.strip().split('\t')
    mine = time.time() - t
    t = time.time()
    r = subprocess.run(['systemd-run', '--user', '--scope', '-q', '--collect', '-p', 'MemoryMax=8G', '-p',
                        'MemorySwapMax=0', 'timeout', '-s', 'KILL', '300', graphix, '--no-cache', '--check', f.name],
                       capture_output=True, text=True)
    today = time.time() - t
    os.unlink(f.name)
    lines = text.count('\n')
    print(f'{n} blocks, {lines} lines: prototype {out[0]} {out[1][:30]} in {mine * 1000:.0f} ms, {out[-1]} steps; '
          f'today rc={r.returncode} in {today * 1000:.0f} ms {r.stderr.strip()[-100:]}')
