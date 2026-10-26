# Title: the workspace scan walks and parses every hidden directory but
#        .git/.cache (agent worktrees, .direnv, .venv ...): their files become
#        projects of the workspace.
# Command: timeout -s KILL 170 python3 -I design/review-2026-10-26/repro/lsp-scan-hidden-dirs.py
# Expected: workspace/symbol for "only_in_hidden" finds nothing: a hidden
#   directory (here .worktrees/copy, a stand-in for .claude/worktrees) is not
#   part of the project.
# Observed: ['only_in_hidden -> .worktrees/copy/main.gx'].
# Scale, on this checkout (17 worktrees under .claude/worktrees, 41k .gx files),
# with the debug binary: the first message after initialize waits 26.8 s for
# the scan, and every didSave re-stats all 44k files (0.35 s):
#   python3 -I lsp-scan-hidden-dirs.py /home/eric/proj/graphix
import os, shutil, sys, tempfile, time
exec(open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "lsp-client.py")).read())

if len(sys.argv) > 1:
    t = time.time()
    s = Server(sys.argv[1])
    s.barrier()
    print("initialize + first answer %.2f s" % (time.time() - t))
    t = time.time(); s.save("x.gx"); s.barrier()
    print("didSave %.2f s" % (time.time() - t))
    s.close_all()
    sys.exit(0)
root = tempfile.mkdtemp()
open(os.path.join(root, "main.gx"), "w").write("let real = 1;\nreal\n")
os.makedirs(os.path.join(root, ".worktrees", "copy"))
open(os.path.join(root, ".worktrees", "copy", "main.gx"), "w").write("let only_in_hidden = 1;\nonly_in_hidden\n")
s = Server(root)
r = s.request("workspace/symbol", {"query": "only_in_hidden"})
print([x["name"] + " -> " + os.path.relpath(x["location"]["uri"][len("file://"):], root)
       for x in r["result"] or []])
s.close_all()
shutil.rmtree(root)
