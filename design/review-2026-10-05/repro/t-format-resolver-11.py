#!/usr/bin/env python3
# t-format-resolver-11: `graphix fmt` and the LSP find different
# graphixfmt.json files for a symlinked source.
#
# The CLI canonicalizes the file before discovery
# (graphix-shell/src/fmt.rs:60), so a symlink is governed by the config
# above its target; the LSP discovers from the path as opened
# (graphix-lsp/src/handlers/formatting.rs:42), and gxfmt from the path as
# given. Format-on-save and `graphix fmt --check` then disagree.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/t-format-resolver-11.py <graphix>
#
# layout: project/graphixfmt.json = {"width": 30};
#         project/link.gx -> ../shared/real.gx; project/copy.gx a copy of it
# expected: the CLI and the LSP lay out link.gx the same way.
# observed (HEAD c722befe, debug build):
#   CLI copy.gx: broken at width 30     LSP copy.gx: broken at width 30
#   CLI link.gx: one line (width 90)    LSP link.gx: broken at width 30
import atexit, json, os, queue, shutil, subprocess, sys, tempfile, threading, urllib.parse

graphix = sys.argv[1] if len(sys.argv) > 1 else "graphix"
tmp = os.path.realpath(tempfile.mkdtemp(prefix="t-format-resolver-11."))
atexit.register(shutil.rmtree, tmp, True)
proj = os.path.join(tmp, "project")
os.makedirs(proj)
os.makedirs(os.path.join(tmp, "shared"))
open(os.path.join(proj, "graphixfmt.json"), "w").write('{"width": 30}\n')
src = "let f = |a| { let b = a + 1; b * 2 };\nf(1)\n"
open(os.path.join(tmp, "shared", "real.gx"), "w").write(src)
open(os.path.join(proj, "copy.gx"), "w").write(src)
os.symlink("../shared/real.gx", os.path.join(proj, "link.gx"))
env = dict(os.environ, XDG_CACHE_HOME=os.path.join(tmp, "cache"))


def uri(p):
    return "file://" + urllib.parse.quote(p)


def lsp_format(files):
    proc = subprocess.Popen([graphix, "lsp"], env=env, stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    msgs = queue.Queue()

    def reader():
        while True:
            headers = {}
            while True:
                l = proc.stdout.readline()
                if not l:
                    return msgs.put(None)
                l = l.decode().strip()
                if not l:
                    break
                k, v = l.split(":", 1)
                headers[k.strip().lower()] = v.strip()
            msgs.put(json.loads(proc.stdout.read(int(headers["content-length"]))))

    threading.Thread(target=reader, daemon=True).start()

    def send(m):
        b = json.dumps(dict(m, jsonrpc="2.0")).encode()
        proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
        proc.stdin.flush()

    def wait_for(id):
        while True:
            m = msgs.get(timeout=90)
            if m is None:
                raise SystemExit("server exited early")
            if m.get("id") == id:
                return m

    send({"id": 1, "method": "initialize", "params": {
        "processId": os.getpid(), "rootUri": uri(proj),
        "workspaceFolders": [{"uri": uri(proj), "name": "p"}], "capabilities": {}}})
    wait_for(1)
    send({"method": "initialized", "params": {}})
    out = {}
    for i, f in enumerate(files):
        p = os.path.join(proj, f)
        send({"method": "textDocument/didOpen", "params": {"textDocument": {
            "uri": uri(p), "languageId": "graphix", "version": 1, "text": open(p).read()}}})
        send({"id": 10 + i, "method": "textDocument/formatting", "params": {
            "textDocument": {"uri": uri(p)}, "options": {"tabSize": 4, "insertSpaces": True}}})
        res = wait_for(10 + i).get("result")
        out[f] = res[0]["newText"] if res else src
    send({"id": 3, "method": "shutdown", "params": None})
    wait_for(3)
    send({"method": "exit", "params": None})
    proc.stdin.close()
    proc.wait(timeout=30)
    return out


def shape(text):
    return "one line (width 90)" if "{ let b" in text else "broken at width 30"


lsp = lsp_format(["copy.gx", "link.gx"])
for f in ["copy.gx", "link.gx"]:
    cli = subprocess.run([graphix, "fmt", "--stdout", os.path.join("project", f)],
                         cwd=tmp, env=env, capture_output=True, text=True).stdout
    print(f"CLI {f}: {shape(cli):22}  LSP {f}: {shape(lsp[f])}")
