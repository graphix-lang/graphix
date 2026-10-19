"""A lone interface's parse error outlives the interface becoming its implementation's.

Run: timeout -s KILL 170 python3 design/review-2026-10-19/repro/x-lsp-probe-lone-gxi-stale-error.py
Steps: open api.gxi (no api.gx anywhere) with a parse error; open api.gx (unsaved);
       fix api.gxi; edit api.gx.
Expected: after the fix, api.gxi has no diagnostics.
Observed:  the old "Unexpected `=`" error stays on api.gxi through every later edit
           (a fresh server over the same buffers reports none). Opening api.gx changes
           ServerState::roots_of(api.gxi) from api.gxi to api.gx, but set_document never
           retires the api.gxi root, and publish_files merges every root's diagnostics.
"""
import os, sys, json, tempfile
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from x_lsp_probe_lspc import Client

root = tempfile.mkdtemp()
gxi, gx = root + "/api.gxi", root + "/api.gx"
c = Client(root)
def show(step):
    print(step, json.dumps({k.rsplit("/", 1)[-1]: [d["message"].split("\n")[0] for d in v["diagnostics"]] for k, v in c.diags.items()}))
c.open(gxi, "val v: i64 = ;\n"); c.barrier(); show("lone gxi broken:")
c.open(gx, "let v = 1;\nv\n"); c.barrier(); show("gx opened:      ")
c.change(gxi, "val v: i64;\n", 2); c.barrier(); show("gxi fixed:      ")
c.change(gx, "let v = 2;\nv\n", 2); c.barrier(); show("gx edited:      ")
c.shutdown()
