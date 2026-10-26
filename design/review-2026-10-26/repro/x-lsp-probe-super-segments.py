# Title: a `super` segment of a path names the wrong module: Query::target and
#   references assume each segment before the item adds one level.
# Command: timeout -s KILL 175 python3 design/review-2026-10-26/repro/x-lsp-probe-super-segments.py
# Expected: in a/b.gx, `super::super::top`'s first `super` is module `a`
#   (hover `mod a`, definition a.gx), its second the root; references of
#   the first `super` are the sites naming `a`.
# Observed: both `super`s hover as `mod ` (the root, printed empty), have no
#   definition, and their references are every `super` that names the root
#   (the first `super` of a/b.gx included, a.gx's `super::top` too).
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
    "main.gx": 'let top = 5;\nmod a;\na::y\n',
    "a.gx": 'let x = super::top;\nmod b;\nlet y = b::deep;\n',
    "a/b.gx": 'let deep = super::super::top + super::x;\n',
})
c = Client(root=D)
for p in P.values():
    c.open(p, open(p).read())
c.sync()
print("diagnostics:", [d["message"] for v in c.diags.values() for d in v["diagnostics"]])
b = P["a/b.gx"]
for col in (11, 18):
    h = c.hover(b, 0, col).get("result")
    d = c.definition(b, 0, col).get("result")
    r = c.references(b, 0, col).get("result") or []
    print("super at col", col, "hover:", h and h["contents"]["value"], "definition:", d,
          "references:", [(x["uri"].rsplit("/", 2)[-1], x["range"]["start"]["line"], x["range"]["start"]["character"]) for x in r])
c.close()
