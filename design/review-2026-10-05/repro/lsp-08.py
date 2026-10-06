#!/usr/bin/env python3
# lsp-08: module-resolution errors carry no position; the LSP shows them
# at line 1, column 1 of the root and the CLI prints no line at all.
#
# resolve (graphix-types/src/expr/resolver.rs:618-622) bails "module X
# could not be found", a Broken resolution returns its bare error (602),
# and LoadChain::push bails "import cycle" (823, reached from 720).
# resolve_modules_int adds only the positionless CouldNotResolve (702), and
# nothing calls .at() with the `mod` statement it holds. error_location
# (graphix-lsp/src/diagnostics.rs:24) finds no ParserContext, ErrorSite or
# ErrorContext, so ServerState::diagnostic (graphix-lsp/src/state.rs:226-232)
# puts the error on the ROOT file at (0,0). Parse and type errors in the
# same submodule are placed correctly (shown as the control).
#
# Each case is a project in a temp dir; the script runs `graphix --check
# main.gx` and an LSP session that opens one file (a.gx, else main.gx).
#   1 missing   main.gx: let a = 1; let b = 2; mod missing; a + b   (open main.gx)
#   2 nested    main.gx: let z = 1; mod a; a::y   a.gx: let q = 2; mod nosuch; let y = 3
#   3 cycle     main.gx as 2                      a.gx: let q = 2; mod a; let y = 3
#   4 broken    main.gx as 2 (open main.gx)       a.gx: not UTF-8
#   5 control   main.gx as 2                      a.gx: let q = 2; let y = undefined_x
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-08.py <graphix>
#
# expected: each error on the `mod` statement that failed, in the file that
# wrote it (0-based): 1 main.gx (2,0), 2 a.gx (1,0), 3 a.gx (1,0),
# 4 main.gx (1,0); the CLI names that line and file, as it does for 5.
#
# observed (HEAD c722befe, debug build):
#   1 missing
#       CLI: Error: could not resolve module missing
#       CLI: module missing could not be found: .../missing.gx or .../missing/mod.gx: no such file; ..
#       LSP: main.gx (0,0)-(0,3) 'module missing could not be found: ...'
#   2 nested   (main.gx is not open; a.gx gets nothing)
#       CLI: Error: could not resolve module nosuch
#       CLI: module nosuch could not be found: .../a/nosuch.gx or .../a/nosuch/mod.gx: no such file; ..
#       LSP: main.gx (0,0)-(0,3) 'module nosuch could not be found: ...'
#   3 cycle
#       CLI: Error: import cycle: a -> a (File(".../a.gx"))
#       LSP: main.gx (0,0)-(0,3) 'import cycle: a -> a (File(".../a.gx"))'
#   4 broken
#       CLI: Error: could not resolve module a
#       CLI: 0: .../a.gx
#       LSP: main.gx (0,0)-(0,3) 'invalid utf-8 sequence of 1 bytes from index 9'
#   5 control
#       CLI: Error: in file .../main.gx
#       CLI: 1: at: line: 2, column: 9 in file .../a.gx, in: undefined_x
#       LSP: a.gx (1,8)-(1,19) 'undefined_x not defined'
import json, os, select, shutil, subprocess, sys, tempfile, time, urllib.parse

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"
MAIN2 = b"let z = 1;\nmod a;\na::y\n"
CASES = [
    ("1 missing", {"main.gx": b"let a = 1;\nlet b = 2;\nmod missing;\na + b\n"}, "main.gx"),
    ("2 nested", {"main.gx": MAIN2, "a.gx": b"let q = 2;\nmod nosuch;\nlet y = 3\n"}, "a.gx"),
    ("3 cycle", {"main.gx": MAIN2, "a.gx": b"let q = 2;\nmod a;\nlet y = 3\n"}, "a.gx"),
    ("4 broken", {"main.gx": MAIN2, "a.gx": b"let y = \"\xff\xfe\"\n"}, "main.gx"),
    ("5 control", {"main.gx": MAIN2, "a.gx": b"let q = 2;\nlet y = undefined_x\n"}, "a.gx"),
]


def uri(path):
    return "file://" + urllib.parse.quote(path, safe="/-._~")


def send(p, msg):
    body = json.dumps(msg).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
    p.stdin.flush()


def messages(p, quiet):
    buf, deadline = b"", time.time() + 90
    while time.time() < deadline:
        end = buf.find(b"\r\n\r\n")
        if end >= 0:
            n = int(buf[:end].split(b":")[1])
            if len(buf) >= end + 4 + n:
                yield json.loads(buf[end + 4:end + 4 + n])
                buf = buf[end + 4 + n:]
                continue
        if not select.select([p.stdout], [], [], quiet)[0]:
            return
        chunk = os.read(p.stdout.fileno(), 65536)
        if not chunk:
            return
        buf += chunk


def lsp(d, open_file):
    env = dict(os.environ, XDG_CACHE_HOME=d)
    p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                         stderr=subprocess.DEVNULL, env=env)
    send(p, {"jsonrpc": "2.0", "id": 1, "method": "initialize",
             "params": {"processId": None, "rootUri": uri(d), "capabilities": {}}})
    send(p, {"jsonrpc": "2.0", "method": "initialized", "params": {}})
    path = os.path.join(d, open_file)
    text = open(path, "rb").read().decode("utf-8", "replace")
    send(p, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {
        "uri": uri(path), "languageId": "graphix", "version": 1, "text": text}}})
    send(p, {"jsonrpc": "2.0", "id": 2, "method": "textDocument/documentSymbol",
             "params": {"textDocument": {"uri": uri(path)}}})
    for m in messages(p, 60):
        if m.get("method") == "textDocument/publishDiagnostics":
            f = os.path.basename(urllib.parse.unquote(m["params"]["uri"]))
            for g in m["params"]["diagnostics"]:
                s, e = g["range"]["start"], g["range"]["end"]
                msg = g["message"].split("\n")[0].replace(d, "...")[:90]
                print(f"    LSP: {f} ({s['line']},{s['character']})-"
                      f"({e['line']},{e['character']}) {msg!r}")
        if m.get("id") == 2:
            break
    p.kill()
    p.wait()


for name, files, open_file in CASES:
    d = os.path.realpath(tempfile.mkdtemp(prefix="lsp-08-"))
    for f, t in files.items():
        open(os.path.join(d, f), "wb").write(t)
    print(name)
    r = subprocess.run([GX, "--no-cache", "--check", os.path.join(d, "main.gx")],
                       capture_output=True, text=True, timeout=30,
                       env=dict(os.environ, XDG_CACHE_HOME=d))
    lines = [l.strip() for l in (r.stdout + r.stderr).splitlines()]
    lines = [l for l in lines if l and l != "Caused by:"]
    at = [l for l in lines if "at: line" in l][:1]
    for line in lines[:1] + (at or lines[1:2]):
        print("    CLI: " + line.replace(d, "...")[:100])
    lsp(d, open_file)
    shutil.rmtree(d)
