# Title: a query in a file two projects share answers from a stale check
#   when the other project's check of the same buffer succeeded.
# Command: timeout -s KILL 175 python3 design/review-2026-10-26/repro/x-lsp-probe-stale-shared-root.py
# Expected: after the edit, hover on `f` shows `fn(x: i64) -> string` and
#   hover on `g` shows `g: i64` (tool_b.gx's check of the new buffer passed).
# Observed: hover f = `f: fn(x: i64) -> i64` (the old type), hover g = null,
#   references g = null: ServerState::checked_for picks tool_a.gx's root,
#   whose last check failed, so its Checked is the one before the edit.
import importlib.util, os, sys, tempfile
_s = importlib.util.spec_from_file_location("lspc", os.path.join(os.path.dirname(os.path.abspath(__file__)), "x-lsp-probe-client.py"))
lspc = importlib.util.module_from_spec(_s); _s.loader.exec_module(lspc)
Client, uri = lspc.Client, lspc.uri
D = tempfile.mkdtemp(prefix="x-lsp-probe-")
def write(files):
    for name, text in files.items():
        p = os.path.join(D, name)
        os.makedirs(os.path.dirname(p), exist_ok=True)
        open(p, "w").write(text)
    return {n: os.path.join(D, n) for n in files}
P = write({
    "tool_a.gx": 'mod util;\nutil::f(1) + 1\n',
    "tool_b.gx": 'mod util;\nutil::f(1)\n',
    "util.gx": 'let f = |x: i64| x + 1;\n',
})
c = Client(root=D)
u = P["util.gx"]
c.open(u, open(u).read())
c.sync()
c.change(u, 'let f = |x: i64| "s[x]";\nlet g = 2;\n', 2)
c.sync()
print("diagnostics:", {k.rsplit("/", 1)[1]: [d["message"][:60] for d in v["diagnostics"]] for k, v in c.diags.items()})
print("hover f:", c.hover(u, 0, 4).get("result"))
print("hover g:", c.hover(u, 1, 4).get("result"))
print("refs g:", c.references(u, 1, 4).get("result"))
c.close()
