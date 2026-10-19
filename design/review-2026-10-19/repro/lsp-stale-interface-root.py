# Title: a lone interface's root outlives its implementation's open; its stale
#        parse error stays published after the interface is fixed in the buffer.
# Command: timeout -s KILL 120 python3 design/review-2026-10-19/repro/lsp-stale-interface-root.py
# Expected: after "gxi fixed in buffer" api.gxi has no diagnostics.
# Observed: api.gxi keeps "Unexpected `;` ..." until the file is saved
#   (didSave runs ServerState::retire; didOpen/didChange never do).
import os, tempfile
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "lsp-client.py")).read())
root = tempfile.mkdtemp()
gxi, gx = root + '/api.gxi', root + '/api.gx'
open(gxi, 'w').write('val foo: ;\n')
c = Client(root)
c.open(gxi, 'val foo: ;\n'); c.sync()
def show(tag):
    print(tag, {u.split('/')[-1]: [d['message'][:40] for d in p['diagnostics']] for u, p in c.diags.items()})
show('gxi alone, broken:')
open(gx, 'w').write('let foo = 1;\n')
c.open(gx, 'let foo = 1;\n'); c.sync()
show('api.gx opened:')
c.change(gxi, 'val foo: i64;\n'); c.sync()
show('gxi fixed in buffer:')
c.save(gxi); c.sync()
show('gxi saved:')
c.shutdown()
