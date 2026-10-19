"""Completion loses a block's locals when a `seq` follows later in the function.

Run: timeout -s KILL 170 python3 design/review-2026-10-19/repro/x-lsp-probe-seq-hides-block-locals.py
     (from the repo root; drives `graphix lsp` over stdio via x_lsp_probe_lspc.py)
Expected: both lines print `inner_local` among the completions at `|inner_local`.
Observed:  plain True / with_seq False: with a `seq` after the block, completion
           offers only the enclosing lambda's names. Completer::scope_at takes a
           seq-desugared ScopeMapEntry (no written end, end = (0,0)) as the statement
           "before" the cursor, whatever its position, and answers with its scope.
"""
import os, sys, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from x_lsp_probe_lspc import Client, pos_of

base = "let local = |q: i64| {\n  let r = {\n    let inner_local = q + 1;\n    inner_local\n  };\n  TAIL\n};\nlocal(1)\n"
tails = {"plain": "r + 1", "with_seq": "let n = 0;\n  seq q { n <- q + 1 };\n  r + n"}
root = tempfile.mkdtemp()
f = os.path.join(root, "main.gx")
open(f, "w").write("1\n")
c = Client(root)
for v, (name, tail) in enumerate(tails.items(), 1):
    t = base.replace("TAIL", tail)
    (c.open if v == 1 else lambda f, t, v: c.change(f, t, v))(f, t, v)
    c.barrier()
    l, ch = pos_of(t, "inner_local\n")
    labels = [x["label"] for x in (c.at("completion", f, l, ch).get("result") or [])]
    print(name, "inner_local" in labels, labels[:4])
c.shutdown()
