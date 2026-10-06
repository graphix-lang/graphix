#!/usr/bin/env python3
# fuzz-lib-b-03: detcheck flaps on any program with two disjoint fused parts
# (the CLIF dump prints in compile-task order).
#
# detcheck_one_pair (graphix-fuzz/src/lib.rs:4268) runs `graphix-fuzz
# detcheck-one` twice at once and compares the normalize_clif'd stderr line by
# line (lib.rs:4303). maybe_dump_clif (graphix-compiler/src/fusion/emit/jit.rs:668)
# prints each kernel when it is emitted, and fusion::fuse_each
# (graphix-compiler/src/fusion/mod.rs:1097) emits disjoint parts in rayon tasks;
# the children get RAYON_NUM_THREADS=2 (child_command, lib.rs:3963). So the
# blocks come out in scheduling order, and normalize_clif keeps that order.
#
# This script runs the gate's pair by hand: two children at once with the env
# the gate gives them, stderr normalized by a line-for-line port of
# normalize_clif, compared like the gate (first_clif_difference). It also
# compares the dumps as multisets of blocks, each normalized alone.
# `--serial` adds GRAPHIX_FUSE_SERIAL=1 (read only by fuse_each) as the control.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/fuzz-lib-b-03.py <graphix-fuzz> [--serial]
#
# expected: every pair SAME (both children fuse the same kernels).
# observed (HEAD c722befe, debug build), parallel:
#   pair 1 (rc 0/0): FLAP line 113: `;; clif region_ExprId(#0) chunk` vs `;; clif region_ExprId(#8)`; blocks EQUAL (6)
#   pair 2 (rc 0/0): SAME; blocks EQUAL (6)
#   pair 3 (rc 0/0): FLAP line 320: `;; clif region_ExprId(#13) chunk` vs `;; clif wrapper`; blocks EQUAL (6)
#   pair 4 (rc 0/0): FLAP line 208: `;; clif wrapper` vs `;; clif region_ExprId(#13)`; blocks EQUAL (6)
#   pair 5 (rc 0/0): FLAP line 113: `;; clif region_ExprId(#8)` vs `;; clif region_ExprId(#0) chunk`; blocks EQUAL (6)
#   pair 6 (rc 0/0): SAME; blocks EQUAL (6)
#   parallel: 4 of 6 pairs flap
# --serial: 6 of 6 pairs SAME, blocks EQUAL (6) ("serial: 0 of 6 pairs flap").
# The committed corpus flaps the same way (one pair each, as the gate runs
# them): const-body-firing-jul2026/p6_two_folds.gx and
# dyncall-loop-stale-ride-aug2026/00_map_filter_refire_rides_shared_cache.gx
# FLAP with equal block multisets, so `graphix-fuzz detcheck` cannot pass.
import os, subprocess, sys, tempfile, threading
from collections import Counter

PROG = (
    "(array::map(array::iter([[1, 2], [3]]), |x| x * 2), "
    "array::map(array::iter([[4], [5, 6]]), |x| x + 3))\n"
)
BIN = next((a for a in sys.argv[1:] if not a.startswith("--")), "graphix-fuzz")
SERIAL = "--serial" in sys.argv
PAIRS = 6

PATS = ["ExprId(", "u0:", "kir_", "lambda#", "<abstract#", "'_"]
DIG = "0123456789"
HEX = "0123456789abcdefABCDEF"


def rust_lines(s):
    ls = s.split("\n")
    if ls and ls[-1] == "":
        ls.pop()
    return [l[:-1] if l.endswith("\r") else l for l in ls]


def normalize_clif(s):
    ids = {}
    out = []
    for line in rust_lines(s):
        if line.startswith("["):
            continue
        r, rest = [], line
        while True:
            best = None
            for pat in PATS:
                pos = rest.find(pat)
                if pos >= 0 and (best is None or pos < best[0]):
                    best = (pos, pat)
            if best is None:
                r.append(rest)
                break
            pos, pat = best
            pre, tail = rest[: pos + len(pat)], rest[pos + len(pat):]
            r.append(pre)
            end = 0
            while end < len(tail) and tail[end] in DIG:
                end += 1
            if end == 0:
                rest = tail
                continue
            key = pat + tail[:end]
            if key not in ids:
                ids[key] = len(ids)
            r.append("#%d" % ids[key])
            rest = tail[end:]
        line = "".join(r)
        i, n = 0, len(line)
        while i < n:
            c = line[i]
            if c == "0" and i + 1 < n and line[i + 1] == "x":
                j = i + 2
                while j < n and (line[j] in HEX or line[j] == "_"):
                    j += 1
                out.append("PTR" if j - (i + 2) >= 8 else line[i:j])
                i = j
            elif c in DIG:
                j = i + 1
                while j < n and line[j] in DIG:
                    j += 1
                out.append("BIGNUM" if j - i >= 9 else line[i:j])
                i = j
            else:
                out.append(c)
                i += 1
        out.append("\n")
    return "".join(out)


def first_clif_difference(a, b):
    la, lb = rust_lines(a), rust_lines(b)
    for i, (x, y) in enumerate(zip(la, lb)):
        if x != y:
            return "line %d: `%s` vs `%s`" % (i + 1, x, y)
    return "length: %d vs %d lines" % (len(la), len(lb))


def blocks(raw):
    bs, cur = [], None
    for l in rust_lines(raw):
        if l.startswith(";; clif"):
            if cur is not None:
                bs.append(cur)
            cur = [l]
        elif cur is not None:
            cur.append(l)
    if cur is not None:
        bs.append(cur)
    return Counter(normalize_clif("\n".join(b) + "\n") for b in bs)


def child(res, k):
    env = dict(os.environ)
    env.setdefault("RAYON_NUM_THREADS", "2")
    env.update(
        TOKIO_WORKER_THREADS="2",
        GRAPHIX_DUMP_CLIF="1",
        GRAPHIX_FUZZ_SANDBOXED="1",
        GRAPHIX_STACK_BUDGET=str(1 << 30),
    )
    if SERIAL:
        env["GRAPHIX_FUSE_SERIAL"] = "1"
    with tempfile.TemporaryDirectory() as d:
        p = subprocess.run([BIN, "detcheck-one"], input=PROG.encode(), cwd=d,
                           env=env, capture_output=True, timeout=120)
    res[k] = (p.returncode, p.stderr.decode("utf-8", "replace"))


flaps = 0
for i in range(1, PAIRS + 1):
    res = {}
    ts = [threading.Thread(target=child, args=(res, k)) for k in (0, 1)]
    for t in ts:
        t.start()
    for t in ts:
        t.join()
    (ca, ra), (cb, rb) = res[0], res[1]
    da, db = normalize_clif(ra), normalize_clif(rb)
    same_blocks = blocks(ra) == blocks(rb)
    nb = sum(blocks(ra).values())
    if ca != cb:
        verdict = "verdicts differ: %r vs %r" % (ca, cb)
    elif da != db:
        verdict = "FLAP " + first_clif_difference(da, db)
        flaps += 1
    else:
        verdict = "SAME"
    print("pair %d (rc %s/%s): %s; blocks %s (%d)"
          % (i, ca, cb, verdict, "EQUAL" if same_blocks else "DIFFER", nb))
print("%s: %d of %d pairs flap" % ("serial" if SERIAL else "parallel", flaps, PAIRS))
