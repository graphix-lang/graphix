#!/usr/bin/env python3
# x-errors-06: interface (.gxi) conformance errors are placed at the
# parent's `mod` statement, not at the val, typedef or trait they concern.
#
# check_sig (graphix-compiler/src/node/module.rs:367-596) attaches no site
# (`.at`/`wrap!`/`bailat!`) to any of its errors: the val mismatch (395,
# a string `.with_context`), the typedef bails (440-480), the trait bail
# (563) and "sig item .. is missing an implementation" (589).
# Module::typecheck0 adds the string "compiling module m", so the first
# site is typecheck0_statements' `wrap!(n, ..)` (node/mod.rs:984) with `n`
# the parent's `mod m` statement. The LSP (graphix-lsp/src/state.rs:226)
# puts the diagnostic at that ErrorSite and shows only the chain leaf,
# which for a val mismatch does not name the val.
#
# Each case is a project in a temp dir (main.gx: `mod m;\nm::x`); the
# script runs `graphix --no-cache --check main.gx` and an LSP session that
# opens m.gx.
#   1 val       m.gx: let x = "s"                 m.gxi: val x: i64;
#   2 missing   m.gx: let x = 1                   m.gxi: val x: i64; val y: i64;
#   3 typedef   m.gx: type T = i64; let x = 1     m.gxi: type T = string; val x: i64;
#   4 trait     m.gx: trait Show {..-> i64}; ..   m.gxi: trait Show {..-> string}; ..
#   5 control   m.gx: let x: i64 = "s"            m.gxi: val x: i64;
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/x-errors-06.py <graphix>
#
# expected: cases 1-4 placed in m.gx (the implementing let/type/trait) or
# m.gxi (the interface item), as case 5 is, and the val case's message
# names the val.
# observed (HEAD c722befe, debug build):
#   1 val
#     CLI: at: line: 1, column: 1 in file .../main.gx, in: mod m
#     LSP: main.gx (0,0)-(0,5) 'type mismatch: signature has i64, implementation has string'
#   2 missing
#     CLI: at: line: 1, column: 1 in file .../main.gx, in: mod m
#     LSP: main.gx (0,0)-(0,5) 'sig item val y: i64 is missing an implementation'
#   3 typedef
#     CLI: at: line: 1, column: 1 in file .../main.gx, in: mod m
#     LSP: main.gx (0,0)-(0,5) 'signature mismatch in T, expected type T = string, found type T = i64'
#   4 trait
#     CLI: at: line: 1, column: 1 in file .../main.gx, in: mod m
#     LSP: main.gx (0,0)-(0,5) 'trait Show is declared by the interface as trait Show { val show: fn(self) -> string }; th'
#   5 control
#     CLI: at: line: 1, column: 14 in file .../m.gx, in: "s"
#     LSP: m.gx (0,13)-(0,16) 'type mismatch i64 does not contain string'
import json, os, select, shutil, subprocess, sys, tempfile, time, urllib.parse

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"
MAIN = "mod m;\nm::x\n"
CASES = [
    ("1 val", 'let x = "s"\n', "val x: i64;\n"),
    ("2 missing", "let x = 1\n", "val x: i64;\nval y: i64;\n"),
    ("3 typedef", "type T = i64;\nlet x = 1\n", "type T = string;\nval x: i64;\n"),
    ("4 trait", "trait Show { val show: fn(self) -> i64 };\nlet x = 1\n",
     "trait Show { val show: fn(self) -> string };\nval x: i64;\n"),
    ("5 control", 'let x: i64 = "s"\n', "val x: i64;\n"),
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
    send(p, {"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {
        "uri": uri(path), "languageId": "graphix", "version": 1,
        "text": open(path).read()}}})
    send(p, {"jsonrpc": "2.0", "id": 2, "method": "textDocument/documentSymbol",
             "params": {"textDocument": {"uri": uri(path)}}})
    for m in messages(p, 60):
        if m.get("method") == "textDocument/publishDiagnostics":
            f = os.path.basename(urllib.parse.unquote(m["params"]["uri"]))
            for g in m["params"]["diagnostics"]:
                s, e = g["range"]["start"], g["range"]["end"]
                msg = g["message"].split("\n")[0][:90]
                print(f"    LSP: {f} ({s['line']},{s['character']})-"
                      f"({e['line']},{e['character']}) {msg!r}")
        if m.get("id") == 2:
            break
    p.kill()
    p.wait()


for name, impl, sig in CASES:
    d = os.path.realpath(tempfile.mkdtemp(prefix="x-errors-06-"))
    for f, t in [("main.gx", MAIN), ("m.gx", impl), ("m.gxi", sig)]:
        open(os.path.join(d, f), "w").write(t)
    print(name)
    r = subprocess.run([GX, "--no-cache", "--check", os.path.join(d, "main.gx")],
                       capture_output=True, text=True, timeout=30,
                       env=dict(os.environ, XDG_CACHE_HOME=d))
    lines = [l.strip() for l in (r.stdout + r.stderr).splitlines()]
    at = [l for l in lines if "at: line" in l]
    print("    CLI: " + (at[-1] if at else "(no site)").split(": ", 1)[1].replace(d, "..."))
    lsp(d, "m.gx")
    shutil.rmtree(d)
