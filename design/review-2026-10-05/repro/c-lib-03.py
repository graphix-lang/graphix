#!/usr/bin/env python3
# c-lib-03: the language server never retires #[sync]/#[async]/
# #[tail_recursive] assertions, so every check leaks the file's text.
#
# compile_inner (graphix-compiler/src/node/compiler.rs:197-208) records a
# DefAssertion, holding the decorated statement's Expr, for every asserted
# definition it builds, under CFlag::CheckOnly too. Only
# check_def_assertions (analysis.rs:273) removes entries, and under
# CheckOnly check_and_fuse_inner returns before analysis::analyze
# (lib.rs:1958-1960). The LSP checks every edit with CheckOnly on one
# runtime (graphix-rt/src/gx.rs check_inner, lsp_backend.rs) and deletes
# the nodes, so each check leaves one entry per asserted definition, and
# each entry's Expr pins that check's AST and its Arc<Origin>: the whole
# source text.
#
# This writes two files: a 500 KB string literal, `let f = |x: i64| x + 1`
# and `let z = 0`; the second puts `#[sync]` on `f`. It opens each in
# `graphix lsp`, makes 200 edits (`let z = <n>`), syncing on a
# documentSymbol request after each one so every edit is exactly one
# check, and prints the server's VmRSS every 50 checks.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/c-lib-03.py <graphix>
#
# expected: both files plateau.
#
# observed (HEAD c722befe, debug build):
#   plain    RSS kB [60852, 95260, 95404, 95420, 95432]  1 kB per check after the first 50
#   #[sync]  RSS kB [61104, 127228, 152664, 177096, 201560]  496 kB per check after the first 50
# #[tail_recursive] in place of #[sync] grows the same way and #[serial]
# (not a definition assertion) is flat. With 40 asserted functions of 30
# statements each (22 KB file) the server grows about 1.2 MB per check:
# 115 MB -> 319 MB over checks 25-200, where the unannotated file stays at 90 MB.
import json, os, subprocess, sys, tempfile, threading, time

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"
EDITS, STEP = 200, 50
tmp = tempfile.mkdtemp(prefix="c-lib-03-")
src = 'let s = "' + "a" * 500_000 + '";\nlet f = |x: i64| x + 1;\nlet z = 0\n'
files = {"plain": src, "#[sync]": src.replace("let f =", "#[sync]\nlet f =")}


def server_pid(p):
    # a wrapper script may run the server below it
    todo = [p.pid]
    while todo:
        k = todo.pop()
        try:
            if os.path.basename(os.readlink(f"/proc/{k}/exe")) == "graphix":
                return k
            todo += map(int, open(f"/proc/{k}/task/{k}/children").read().split())
        except OSError:
            pass
    return None


def rss(pid):
    for line in open(f"/proc/{pid}/status"):
        if line.startswith("VmRSS"):
            return int(line.split()[1])


def run(name, text):
    path = os.path.join(tmp, "assert.gx" if "#" in name else "plain.gx")
    open(path, "w").write(text)
    uri = "file://" + path
    env = dict(os.environ, XDG_CACHE_HOME=tmp)
    p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env)
    got, cv = set(), threading.Condition()

    def reader():
        while line := p.stdout.readline():
            if line.lower().startswith(b"content-length:"):
                n = int(line.split(b":")[1])
                p.stdout.readline()
                msg = json.loads(p.stdout.read(n))
                with cv:
                    if "id" in msg and "method" not in msg:
                        got.add(msg["id"])
                        cv.notify_all()

    threading.Thread(target=reader, daemon=True).start()

    def send(msg):
        body = json.dumps(dict(msg, jsonrpc="2.0")).encode()
        p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        p.stdin.flush()

    def request(rid, method, params):
        send({"id": rid, "method": method, "params": params})
        with cv:
            if not cv.wait_for(lambda: rid in got, 60):
                raise SystemExit(f"no response to {method}")

    pid = None
    try:
        for _ in range(200):
            pid = server_pid(p)
            if pid:
                break
            time.sleep(0.05)
        request(1, "initialize", {"processId": None, "rootUri": "file://" + tmp,
                                  "capabilities": {}})
        send({"method": "initialized", "params": {}})
        send({"method": "textDocument/didOpen", "params": {"textDocument": {
            "uri": uri, "languageId": "graphix", "version": 1, "text": text}}})
        sym = {"textDocument": {"uri": uri}}
        request(2, "textDocument/documentSymbol", sym)
        out = [rss(pid)]
        for i in range(EDITS):
            send({"method": "textDocument/didChange", "params": {
                "textDocument": {"uri": uri, "version": i + 2},
                "contentChanges": [{"text": text.replace("let z = 0", f"let z = {i + 1}")}]}})
            # a dirty root is checked before a request is answered
            request(100 + i, "textDocument/documentSymbol", sym)
            if (i + 1) % STEP == 0:
                out.append(rss(pid))
        per = (out[-1] - out[1]) / (EDITS - STEP)
        print(f"{name:8} RSS kB {out}  {per:.0f} kB per check after the first {STEP}")
    finally:
        if pid:
            os.kill(pid, 9)
        p.kill()
        p.wait()


for name, text in files.items():
    run(name, text)
