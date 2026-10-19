# Title: the LSP backend builds its module search path without the CLI's
#        resolver factories: a GRAPHIX_MODPATH `netidx:` entry becomes a file dir.
# Command: timeout -s KILL 120 python3 design/review-2026-10-19/repro/lsp-netidx-modpath.py
# Expected: the same resolution `graphix --check` does (it asks netidx: "...; not found; not found; ...").
# Observed: "...; netidx:/local/graphix/remote.gx or netidx:/local/graphix/remote/mod.gx: no such file; ..."
#   i.e. the entry is searched as the relative directory `netidx:/local/graphix`.
import os, tempfile
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "lsp-client.py")).read())
root = tempfile.mkdtemp()
f = root + '/main.gx'
open(f, 'w').write('mod remote;\nremote::x\n')
c = Client(root, env={'GRAPHIX_MODPATH': 'netidx:/local/graphix'})
c.open(f, open(f).read()); c.sync()
for u, p in c.diags.items(): print(u.split('/')[-1], [d['message'] for d in p['diagnostics']])
c.shutdown()
