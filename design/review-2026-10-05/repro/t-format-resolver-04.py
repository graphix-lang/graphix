#!/usr/bin/env python3
# t-format-resolver-04: LSP checks leave out the GRAPHIX_MODPATH and
# data-dir resolvers, giving a false "module could not be found".
#
# build_backend (graphix-shell/src/lsp_backend.rs:55-57) clones
# base_resolvers while the chain is the stdlib VFS alone; GX::new adds the
# GRAPHIX_MODPATH entries, or $XDG_DATA_HOME/graphix, only to the
# runtime's own chain (graphix-rt/src/gx.rs:251-266). Every LSP check
# passes resolvers_for(file) (VFS + the file's directory) as an override,
# and check_inner then uses the override alone (gx.rs:799-802), so a
# module the CLI finds through GRAPHIX_MODPATH or the init directory
# (book/src/shell.md, "Shared Libraries") is an error in the editor.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/t-format-resolver-04.py <graphix>
#
# expected: the CLI and the server agree: no error diagnostic on main.gx
# in any case, and the hover shows the module value's type.
#
# observed (HEAD c722befe, debug build):
#   modpath  : --check exit 0; lsp: ERROR main.gx:1: module mylib could
#              not be found: <tmp>/proj/mylib.gx or <tmp>/proj/mylib/mod.gx:
#              no such file; hover None
#   data dir : --check exit 0; lsp: ERROR main.gx:1: module common could
#              not be found: <tmp>/proj2/common.gx or ...; hover None
#   sibling  : --check exit 0; lsp: no diagnostics; hover
#              "```graphix\nsib::s: i64\n```" (the control)
import json, os, queue, subprocess, sys, tempfile, threading, urllib.parse

graphix = sys.argv[1] if len(sys.argv) > 1 else "graphix"
tmp = os.path.realpath(tempfile.mkdtemp(prefix="t-format-resolver-04."))
files = {
    "lib/mylib.gx": "let v = 42\n",
    "proj/main.gx": "mod mylib;\nmylib::v\n",
    "data/graphix/common.gx": "let w = 7\n",
    "proj2/main.gx": "mod common;\ncommon::w\n",
    "proj3/sib.gx": "let s = 1\n",
    "proj3/main.gx": "mod sib;\nsib::s\n",
}
for rel, text in files.items():
    os.makedirs(os.path.dirname(os.path.join(tmp, rel)), exist_ok=True)
    open(os.path.join(tmp, rel), "w").write(text)
os.makedirs(os.path.join(tmp, "empty"), exist_ok=True)


def uri(p):
    return "file://" + urllib.parse.quote(p)


def lsp(env, root, file, line, col):
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
        seen = []
        while True:
            m = msgs.get(timeout=90)
            if m is None:
                raise SystemExit("server exited early")
            if m.get("id") == id:
                return m, seen
            seen.append(m)

    send({"id": 1, "method": "initialize", "params": {
        "processId": os.getpid(), "rootUri": uri(root),
        "workspaceFolders": [{"uri": uri(root), "name": "p"}], "capabilities": {}}})
    wait_for(1)
    send({"method": "initialized", "params": {}})
    send({"method": "textDocument/didOpen", "params": {"textDocument": {
        "uri": uri(file), "languageId": "graphix", "version": 1,
        "text": open(file).read()}}})
    send({"id": 2, "method": "textDocument/hover", "params": {
        "textDocument": {"uri": uri(file)},
        "position": {"line": line, "character": col}}})
    hover, seen = wait_for(2)
    out = []
    for m in seen:
        if m.get("method") == "textDocument/publishDiagnostics":
            for d in m["params"]["diagnostics"]:
                sev = {1: "ERROR", 2: "WARNING"}.get(d.get("severity"))
                out.append(f"{sev} main.gx:{d['range']['start']['line'] + 1}: "
                           + d["message"].replace(tmp, "<tmp>"))
    res = hover.get("result")
    out.append("hover " + (json.dumps(res["contents"]["value"]) if res else "None"))
    send({"id": 3, "method": "shutdown", "params": None})
    wait_for(3)
    send({"method": "exit", "params": None})
    proc.stdin.close()
    proc.wait(timeout=30)
    return out


base = {k: v for k, v in os.environ.items() if k != "GRAPHIX_MODPATH"}
base["XDG_CACHE_HOME"] = os.path.join(tmp, "cache")
cases = [
    ("modpath", dict(base, GRAPHIX_MODPATH="file:" + os.path.join(tmp, "lib"),
                     XDG_DATA_HOME=os.path.join(tmp, "empty")), "proj", 7),
    ("data dir", dict(base, XDG_DATA_HOME=os.path.join(tmp, "data")), "proj2", 8),
    ("sibling", dict(base, XDG_DATA_HOME=os.path.join(tmp, "empty")), "proj3", 5),
]
for name, env, proj, col in cases:
    root = os.path.join(tmp, proj)
    main = os.path.join(root, "main.gx")
    rc = subprocess.run([graphix, "--no-cache", "--check", main], env=env,
                        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode
    print(f"{name:9}: --check exit {rc}; lsp: " + "; ".join(lsp(env, root, main, 1, col)))
