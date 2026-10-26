# Title: the workspace scan descends hidden directories and blocks the server
#   until it has parsed every .gx under them.
# Command: timeout -s KILL 175 python3 design/review-2026-10-26/repro/x-lsp-probe-scan-hidden-dirs.py
# Expected: a `.claude/worktrees/..` (or `.jj`, `.direnv`, ..) tree is not
#   part of the project graph; the first answer comes as fast with it as
#   without it.
# Observed (debug build, this machine): 20 copies of book/src/examples and
#   stdlib under `.wt/` (6160 files) delay the first response from 0.11 s
#   to 2.82 s, the time the scan takes to parse them all. In Eric's checkout `.claude/worktrees` holds
#   40953 .gx files: with the repository as the workspace root the server
#   answers its first request after 27 s, and a didSave costs 0.38 s.
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
import time
def first_answer(root):
    t0 = time.time()
    c = Client(root=root)
    c.sync()
    dt = time.time() - t0
    t0 = time.time()
    c.notify("textDocument/didSave", {"textDocument": {"uri": uri(os.path.join(root, "main.gx"))}})
    c.sync()
    ds = time.time() - t0
    c.close()
    return dt, ds
write({"main.gx": "let x = 1;\nx\n"})
print("without hidden dir: first answer %.2fs, didSave %.2fs" % first_answer(D))
import shutil
REPO = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", ".."))
for i in range(20):
    for sub in ("book/src/examples", "stdlib"):
        shutil.copytree(os.path.join(REPO, sub), os.path.join(D, ".wt", "copy%d" % i, sub),
                        ignore=lambda d, names: [n for n in names if n == "target" or (os.path.isfile(os.path.join(d, n)) and not n.endswith((".gx", ".gxi", ".toml")))])
n = sum(1 for r, _, fs in os.walk(os.path.join(D, ".wt")) for f in fs if f.endswith((".gx", ".gxi")))
print("with %d .gx/.gxi files under .wt/: first answer %.2fs, didSave %.2fs" % ((n,) + first_answer(D)))
