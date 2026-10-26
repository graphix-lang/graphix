# Title: in a document whose lines end in a lone CR, every diagnostic lands on line 0 (x-formatter-04 variant)
# Command: TMPDIR=<scratch> timeout -s KILL 90 python3 design/review-2026-10-26/repro/x-rerun-5-lsp-lone-cr-diag.py
# Expected: the same range for both documents, line 1 characters 8..15 (LSP counts "\r"
#   as a line break, as position::lines does).
# Observed: lf [line 1, 8..15]; cr [line 0, 10..10]: the parser counts only "\n", so
#   the error's position is line 0, and the server passes it on unconverted.
import os, tempfile
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "x-rerun-5-lsp-client.py")).read())
root = tempfile.mkdtemp()
m = root + '/main.gx'
for name, text in [("lf", 'let x = 1;\nlet y = "a" + 1;\ny\n'), ("cr", 'let x = 1;\rlet y = "a" + 1;\ry\r')]:
    open(m, 'w', newline='').write(text)
    c = Client(root)
    c.open(m, text); c.sync()
    print(name, [(d['range'], d['message'][:30]) for p in c.diags.values() for d in p['diagnostics']])
    c.shutdown()
