# Title: the workspace scan does not descend a symlinked directory, so a module
#        the resolver loads through one is a lone root in the LSP.
# Command: timeout -s KILL 170 python3 -I design/review-2026-10-26/repro/lsp-symlinked-module-dir.py
# Expected: `graphix --check proj/main.gx` passes (rc=0), so the LSP shows no
#   diagnostic on lib/mod.gx, and after renaming `v` in lib/mod.gx main.gx
#   carries "lib::v not defined" at once (its root is rechecked; it does once
#   main.gx itself is touched).
# Observed:
#   check main: rc=0
#   lib/mod.gx diagnostics: ["`super` goes above the package root"]   (checked as a lone script)
#   after renaming v in lib/mod.gx, main.gx diagnostics: None         (main never rechecked)
#   hover super::x in lib/mod.gx: None
import os, shutil, tempfile, subprocess
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "lsp-client.py")).read())

base = tempfile.mkdtemp()
root = os.path.join(base, "proj")
shared = os.path.join(base, "shared")
os.makedirs(root); os.makedirs(shared)
open(os.path.join(root, "main.gx"), "w").write("let x = 1;\nmod lib;\nlib::v\n")
open(os.path.join(shared, "mod.gx"), "w").write("use super::x;\nlet v = x + 1;\n")
os.symlink("../shared", os.path.join(root, "lib"))
r = subprocess.run(["timeout", "-s", "KILL", "30", BIN, "--check", os.path.join(root, "main.gx")])
print("check main: rc=%d" % r.returncode)
s = Server(root)
s.open("lib/mod.gx", open(os.path.join(shared, "mod.gx")).read())
s.barrier()
print("lib/mod.gx diagnostics:", [d["message"] for d in s.diag("lib/mod.gx") or []])
s.open("main.gx", open(os.path.join(root, "main.gx")).read())
s.barrier()
s.change("lib/mod.gx", "use super::x;\nlet w = x + 1;\n")
s.barrier()
print("after renaming v in lib/mod.gx, main.gx diagnostics:", s.diag("main.gx"))
r = s.request("textDocument/hover", s.at("lib/mod.gx", 0, 11))
print("hover super::x in lib/mod.gx:", r.get("result"))
s.close_all()
shutil.rmtree(base)
