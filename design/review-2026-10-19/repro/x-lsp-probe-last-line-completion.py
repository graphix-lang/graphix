"""Completion on the empty line after a file's final newline returns nothing.

Run: timeout -s KILL 170 python3 design/review-2026-10-19/repro/x-lsp-probe-last-line-completion.py
Expected: completion at (2, 0) of "let abc = 1;\nabc\n" offers `abc`, as an empty line
          inside the file does.
Observed:  interior empty line: 142 items with abc; final empty line: 0 items.
           text::line_chars and position::position_to_char_col_in_text read the line
           with str::lines(), which yields no line after a trailing '\n'.
"""
import os, sys, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from x_lsp_probe_lspc import Client

root = tempfile.mkdtemp()
f = root + "/main.gx"
c = Client(root)
def names(r):
    res = r.get("result") or []
    return len(res), [i["label"] for i in res if i["label"].startswith("ab")]
c.open(f, "let abc = 1;\n\nabc\n"); c.barrier()
print("interior empty line (1,0):", names(c.at("completion", f, 1, 0)))
c.change(f, "let abc = 1;\nabc\n", 2); c.barrier()
print("final empty line (2,0):   ", names(c.at("completion", f, 2, 0)))
c.shutdown()
