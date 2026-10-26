# Title: in a document whose lines end with a lone "\r", the server's lines
#        (position::lines: \r, \n, \r\n) disagree with the compiler's (only \n).
# Command: timeout -s KILL 170 python3 -I design/review-2026-10-26/repro/lsp-lone-cr-positions.py
# Expected: the "nope not defined" error on LSP line 2, characters 12..16; a
#   hover on `a` in `let b = a;` (line 1, character 8) answers `a: i64`.
# Observed:
#   cr.gx diagnostics: [((0, 10), (0, 10), 'nope not defined')]   (line 0, past its end)
#   hover on a at line 1: None
import os, shutil, tempfile
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "lsp-client.py")).read())

root = tempfile.mkdtemp()
s = Server(root)
for name, text in [("cr.gx", "let a = 1;\rlet b = 2;\rlet c = a + nope;\rc\r"),
                   ("crok.gx", "let a = 1;\rlet b = a;\rb\r")]:
    open(os.path.join(root, name), "w", newline="").write(text)
    s.open(name, text)
    s.barrier()
d = s.diag("cr.gx") or []
print("cr.gx diagnostics:", [((x["range"]["start"]["line"], x["range"]["start"]["character"]),
                              (x["range"]["end"]["line"], x["range"]["end"]["character"]),
                              x["message"]) for x in d])
r = s.request("textDocument/hover", s.at("crok.gx", 1, 8))
print("hover on a at line 1:", r.get("result"))
s.close_all()
shutil.rmtree(root)
