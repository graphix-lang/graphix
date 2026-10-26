# Title: in a document whose lines end with a lone "\r", every compiler position
#   after the first line is mapped onto LSP line 0.
# Command: timeout -s KILL 175 python3 design/review-2026-10-26/repro/x-lsp-probe-lone-cr.py
# Expected (position::lines, as LSP and VS Code number lines): the error on
#   line 1 chars 8-15 (`x + "a"`), symbol `y` on line 1, hover on `x` at
#   line 2 char 0 answers `x: i64`.
# Observed: the error at line 0 char 10..10, symbol y at line 0 char 10,
#   hover null. The compiler's SourcePosition breaks lines at "\n" only, so
#   the whole file is its line 0 and `y` stands at char 15 there, while
#   position::lines splits at "\r": encode clamps char 15 to the length of
#   LSP line 0 ("let x = 1;"), and decode sends line 2 to a compiler line
#   that does not exist.
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
text = 'let x = 1;\rlet y = x + "a";\rx\r'
P = write({"main.gx": text})
c = Client(root=D)
f = P["main.gx"]
c.open(f, text)
c.sync()
print("diagnostics:", [(d["message"][:40], d["range"]) for v in c.diags.values() for d in v["diagnostics"]])
print("symbols:", [(s["name"], s["selectionRange"]) for s in c.symbols(f)["result"]])
print("hover x on line 2:", c.hover(f, 2, 0).get("result"))
c.close()
