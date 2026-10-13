#!/usr/bin/env python3
# graphix-shell/tests/lsp/main.rs an_interface_alone_is_checked_with_its_implementation
# asserts only that no errors stand, which a server that never checked the
# interface also satisfies (a clean check publishes nothing). This driver
# shows the same open DOES report a mismatch, so the test could pin that.
# usage: GRAPHIX=/path/to/graphix python3 tests-shell-compiler-04.py
import json, os, subprocess, tempfile, pathlib, urllib.parse
G = os.environ.get("GRAPHIX", "graphix")
root = pathlib.Path(tempfile.mkdtemp()).resolve()
(root / "api.gx").write_text('let double = |n: i64| -> string "x";\n')
(root / "api.gxi").write_text("/// twice n\nval double: fn(n: i64) -> i64;\n")
p = subprocess.Popen([G, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=subprocess.DEVNULL)
uri = lambda f: "file://" + urllib.parse.quote(str(root / f) if f else str(root))
def send(m):
    b = json.dumps(m).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b); p.stdin.flush()
def read():
    h = {}
    while (l := p.stdout.readline()) not in (b"\r\n", b""):
        k, v = l.decode().split(":", 1); h[k.strip().lower()] = v.strip()
    return json.loads(p.stdout.read(int(h["content-length"])))
diags = {}
def req(i, m, prm):
    send({"jsonrpc": "2.0", "id": i, "method": m, "params": prm})
    while True:
        r = read()
        if r.get("id") == i and "method" not in r: return r
        if r.get("method") == "textDocument/publishDiagnostics":
            diags[r["params"]["uri"]] = [d["message"] for d in r["params"]["diagnostics"]]
req(1, "initialize", {"workspaceFolders": [{"uri": uri(""), "name": "t"}], "capabilities": {}})
send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {
    "uri": uri("api.gxi"), "languageId": "graphix", "version": 1,
    "text": (root / "api.gxi").read_text()}}})
req(2, "workspace/symbol", {"query": "\u0000"})
print(diags)
req(3, "shutdown", None); send({"jsonrpc": "2.0", "method": "exit", "params": None})
p.wait(timeout=10)
import shutil; shutil.rmtree(root)
