# Title: the workspace scan skips symlinked .gx files, so a module reached
#        through a symlink is a lone root: editing it never re-checks its project.
# Command: timeout -s KILL 120 python3 design/review-2026-10-19/repro/lsp-symlinked-module.py
# Expected: after the edit, main.gx carries "cannot compute string + i64" at once
#   (as it does when util.gx is a regular file).
# Observed: "util.v now a string: {}" -- main.gx gets the error only once it is
#   closed and reopened. workspace::scan keeps `entry.file_type().is_file()`,
#   which is false for a symlink (WalkDir does not follow links).
import os, tempfile
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "lsp-client.py")).read())
top = tempfile.mkdtemp()
root = top + '/proj'
os.makedirs(root); os.makedirs(top + '/shared')
open(root + '/main.gx', 'w').write('mod util;\nutil::v + 1\n')
open(top + '/shared/util.gx', 'w').write('let v = 1;\n')
os.symlink('../shared/util.gx', root + '/util.gx')
m, u = root + '/main.gx', root + '/util.gx'
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
