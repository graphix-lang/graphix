# Title: a module reached through a symlinked DIRECTORY is a lone root: editing it never re-checks its project (lsp-02 variant)
# Command: TMPDIR=<scratch> timeout -s KILL 120 python3 design/review-2026-10-26/repro/x-rerun-5-lsp-symlinked-dir.py
#   (uses x-rerun-5-lsp-client.py beside it; binary /home/eric/tmp/review-2026-10-26/bin/graphix)
# Expected: after the edit, main.gx carries "cannot compute string + i64" at once, as it
#   does when util.gx itself is a symlink (lsp-02's fix).
# Observed: "util.v now a string: {}"; main.gx gets the error only once closed and reopened.
#   workspace::scan stats links to files but WalkDir does not descend a link to a directory.
import os, tempfile
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "x-rerun-5-lsp-client.py")).read())
top = tempfile.mkdtemp()
root = top + '/proj'
os.makedirs(root); os.makedirs(top + '/shared/util')
open(root + '/main.gx', 'w').write('mod util;\nutil::v + 1\n')
open(top + '/shared/util/mod.gx', 'w').write('let v = 1;\n')
os.symlink('../shared/util', root + '/util')
m, u = root + '/main.gx', root + '/util/mod.gx'
c = Client(root)
c.open(m, open(m).read()); c.open(u, open(u).read()); c.sync()
def show(tag):
    print(tag, {x.split('/')[-1]: [d['message'][:40] for d in p['diagnostics']] for x, p in c.diags.items()})
show('both open:')
c.change(u, 'let v = "s";\n'); c.sync()
show('util.v now a string:')
c.close(m); c.sync(); c.open(m, open(m).read()); c.sync()
show('main reopened:')
c.shutdown()
