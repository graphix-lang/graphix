#!/usr/bin/env python3
"""c-analysis-branch: planning a block's runs is quadratic in a run's reads.

`analysis::read_before` looks for a variable the run read that the next
statement publishes by scanning EVERY id the run has read
(`reads.ids.iter().find(|id| publishing.ids.contains(id))`), once per
statement. A run of n statements that each read a variable of their own
costs n^2 / 2 probes; `Vars::meeting` beside it probes the smaller set.

Two programs of 2n statements, n constants then n lets over them:
  distinct: `let b{i} = a{i} + 1`   (the run's read set grows by one a statement)
  same:     `let b{i} = a0 + 1`     (it stays one id)

command (debug binary, `timeout -s KILL 170` around it):
  python3 design/review-2026-11-16/repro/c-analysis-branch-plan-runs-quadratic.py \
      /home/eric/tmp/review-2026-11-16/bin/graphix <scratch dir> 32000
expected: the Analysis phase of the two programs within a small factor
observed (n = 32000):
  distinct: Analysis 2264 ms     (n = 8000: build - check 0.15 s; 16000: 0.71 s; 32000: 2.51 s)
  same:     Analysis   40 ms
"""
import os, re, subprocess, sys

graphix, scratch, n = sys.argv[1], sys.argv[2], int(sys.argv[3])
os.makedirs(scratch, exist_ok=True)


def program(read):
    lines = [f"let a{i} = {i};" for i in range(n)]
    lines += [f"let b{i} = {read(i)} + 1;" for i in range(n)]
    lines.append("b0")
    return "\n".join(lines)


for name, read in (("distinct", lambda i: f"a{i}"), ("same", lambda i: "a0")):
    path = os.path.join(scratch, f"plan_runs_{name}_{n}.gx")
    with open(path, "w") as f:
        f.write(program(read))
    env = dict(os.environ, GRAPHIX_PROFILE="1")
    out = subprocess.run(
        [graphix, "--no-cache", "--no-fusion", "--expand", path],
        capture_output=True, text=True, env=env, timeout=160,
    )
    spans = re.findall(r"phase=Analysis .*?total_ns=(\d+)", out.stdout + out.stderr)
    print(f"{name}: Analysis {max(map(int, spans), default=0) / 1e6:.0f} ms")
