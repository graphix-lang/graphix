# Title: every LSP position is encoded by rescanning its text from the top, so
#        documentSymbol, workspace/symbol and references are quadratic in file size.
# Command: timeout -s KILL 170 python3 -I design/review-2026-10-26/repro/lsp-quadratic-positions.py
# Expected: a request costs about linear time in the file: doubling the file
#   about doubles documentSymbol/references, and both stay well under the check.
# Observed (debug binary):
#   N=2000 check 0.18 s  documentSymbol 0.23 s  workspace/symbol 0.13 s  references(2001 uses) 0.10 s
#   N=4000 check 0.25 s  documentSymbol 0.86 s  workspace/symbol 0.47 s  references(4001 uses) 0.38 s
import os, shutil, tempfile, time
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "lsp-client.py")).read())

for N in (2000, 4000):
    root = tempfile.mkdtemp()
    text = "let v0 = 0;\n" + "".join(f"let v{i} = v0;\n" for i in range(1, N)) + "v0\n"
    open(os.path.join(root, "big.gx"), "w").write(text)
    s = Server(root)
    t = time.time(); s.open("big.gx", text); s.barrier(); check = time.time() - t
    sym = {"textDocument": {"uri": uri(s.path("big.gx"))}}
    t = time.time(); r = s.request("textDocument/documentSymbol", sym); ds = time.time() - t
    t = time.time(); s.request("workspace/symbol", {"query": "v"}); ws = time.time() - t
    t = time.time()
    r = s.request("textDocument/references",
                  {**s.at("big.gx", N, 0), "context": {"includeDeclaration": True}})
    refs = time.time() - t
    print(f"N={N} check {check:.2f} s  documentSymbol {ds:.2f} s  workspace/symbol {ws:.2f} s  "
          f"references({len(r['result'])} uses) {refs:.2f} s")
    s.close_all()
    shutil.rmtree(root)
