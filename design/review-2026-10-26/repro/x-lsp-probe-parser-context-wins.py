# Title: an error raised at an implementation's expression is published on the
#   interface: error_location prefers an outer ParserContext to the ErrorSite.
# Command: timeout -s KILL 175 python3 design/review-2026-10-26/repro/x-lsp-probe-parser-context-wins.py
# Expected: "T is defined by its interface; delete this definition" on util.gx
#   line 3 (`type T = i64;`), as `graphix --check main.gx` names it (the
#   innermost frame, "at: line: 4, column: 1 in file .../util.gx").
# Observed: the diagnostic is on util.gxi, line 0, columns 0-4: the editor
#   tells the user to delete the interface's declaration.
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
    "main.gx": 'mod util;\nutil::f(1)\n',
    "util.gx": '\n\nlet f = |x: i64| -> i64 x + 1;\ntype T = i64;\n',
    "util.gxi": 'type T = i64;\nval f: fn(x: i64) -> i64;\n',
})
c = Client(root=D)
c.open(P["util.gx"], open(P["util.gx"]).read())
c.sync()
for k, v in c.diags.items():
    print(k.rsplit("/", 1)[1], [(d["message"], d["range"]) for d in v["diagnostics"]])
c.close()
