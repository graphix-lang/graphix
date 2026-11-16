# lsp_modpath.rs's one assertion cannot fail when main.gx is never checked.
#
# The test asserts `diagnostics("main.gx") == []`. The server publishes a
# clean file only to clear one it published before (graphix-lsp
# state.rs::publish_files skips an empty, never-published file), so the
# passing run publishes NOTHING for main.gx: exactly what a check that
# panicked (server.rs::guarded publishes nothing) or never ran produces.
# A definition query on `m::b` does tell: it lands in the modpath file.
#
# Run:  timeout -s KILL 90 python3 design/review-2026-11-16/repro/tests-shell-compiler-a-lsp-modpath-vacuous.py buffer
#       timeout -s KILL 90 python3 design/review-2026-11-16/repro/tests-shell-compiler-a-lsp-modpath-vacuous.py disk
# Observed (debug binary at e1be2fbf2):
#   buffer: no PUBLISH line at all; DEFINITION -> .../lib/m.gx line 1, col 4
#   disk:   PUBLISH main.gx ['m::b not defined']; DEFINITION null
# Expected for a non-vacuous pin: the passing case shows something the
# server only produces after checking main.gx against the buffer.
"""Minimal LSP client: drive `graphix lsp` as lsp_modpath.rs does and print
every publishDiagnostics the server sends, and a definition answer."""
import json, os, subprocess, sys, tempfile, pathlib

GX = "/home/eric/tmp/review-2026-11-16/bin/graphix"
variant = sys.argv[1] if len(sys.argv) > 1 else "buffer"

work = pathlib.Path(tempfile.mkdtemp(dir=os.environ.get("TMPDIR", "/tmp")))
lib = work / "lib"; lib.mkdir()
root = work / "proj"; root.mkdir()
(lib / "m.gx").write_text("let a = 1;\n")
(root / "main.gx").write_text("mod m;\nm::b\n")
env = dict(os.environ, GRAPHIX_MODPATH=str(lib), XDG_CACHE_HOME=str(work / "cache"))
p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=subprocess.DEVNULL, env=env)

def send(msg):
    b = json.dumps(msg).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
    p.stdin.flush()

def recv():
    n = None
    while True:
        line = p.stdout.readline()
        if not line:
            return None
        if line in (b"\r\n", b"\n"):
            break
        if line.lower().startswith(b"content-length:"):
            n = int(line.split(b":")[1])
    return json.loads(p.stdout.read(n))

def uri(path):
    return pathlib.Path(path).as_uri()

def wait(id_):
    while True:
        m = recv()
        if m is None:
            print("server closed"); return None
        if m.get("method") == "textDocument/publishDiagnostics":
            ps = m["params"]
            print("PUBLISH", pathlib.Path(ps["uri"][7:]).name, [d["message"][:80] for d in ps["diagnostics"]])
        if m.get("id") == id_ and "method" not in m:
            return m

send({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
    "processId": None, "rootUri": uri(root), "capabilities": {},
    "workspaceFolders": [{"uri": uri(root), "name": "t"}]}})
wait(1)
send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
mtext = "let a = 1;\nlet b = 2;\n" if variant == "buffer" else "let a = 1;\n"
send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {
    "uri": uri(lib / "m.gx"), "languageId": "graphix", "version": 1, "text": mtext}}})
send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {
    "uri": uri(root / "main.gx"), "languageId": "graphix", "version": 1,
    "text": (root / "main.gx").read_text()}}})
send({"jsonrpc": "2.0", "id": 2, "method": "workspace/symbol", "params": {"query": "\u0000"}})
wait(2)
send({"jsonrpc": "2.0", "id": 3, "method": "textDocument/definition", "params": {
    "textDocument": {"uri": uri(root / "main.gx")}, "position": {"line": 1, "character": 3}}})
r = wait(3)
print("DEFINITION", json.dumps(r.get("result")) if r else None)
send({"jsonrpc": "2.0", "id": 4, "method": "shutdown", "params": None})
wait(4)
send({"jsonrpc": "2.0", "method": "exit", "params": None})
p.wait(timeout=20)
