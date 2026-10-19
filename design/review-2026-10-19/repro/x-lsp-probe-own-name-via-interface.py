"""Completion offers a module's non-rec function inside its own body when the module has an interface.

Run: timeout -s KILL 170 python3 design/review-2026-10-19/repro/x-lsp-probe-own-name-via-interface.py
Expected: `f` is not offered at `|title + a` in m.gx (it is not in scope there:
          `f(..)` in its own body is refused "f not defined"), with or without m.gxi.
Observed:  with m.gxi present the list holds `f`; without it, it does not.
           Completer::names filters "not bound yet" names by in_file(b.ori, file),
           and the env's `f` is the interface's `val` (ori m.gxi).
"""
import os, sys, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from x_lsp_probe_lspc import Client, pos_of

root = tempfile.mkdtemp()
open(root + "/main.gx", "w").write("mod m;\nm::f(1, 2)\n")
t = "let f = |title: i64, a: i64| -> i64 {\n    let z = title + a;\n    z\n}\n"
open(root + "/m.gx", "w").write(t)
for gxi in [False, True]:
    if gxi:
        open(root + "/m.gxi", "w").write("val f: fn(title: i64, a: i64) -> i64;\n")
    c = Client(root)
    c.open(root + "/m.gx", t); c.barrier()
    l, ch = pos_of(t, "title + a")
    labels = [x["label"] for x in (c.at("completion", root + "/m.gx", l, ch).get("result") or [])]
    print("with gxi" if gxi else "no gxi  ", "f offered:", "f" in labels, labels[:4])
    c.shutdown()
