#!/usr/bin/env python3
# x-expr-walks-03: the LSP workspace scan misses a `mod` declared below the
# top level and takes a top-level `mod m dynamic {..}` for a file `m.gx`.
#
# graphix-lsp/src/workspace.rs:125 `walk_expr_for_mods` looks only at the
# file's top-level `Module` statements. The runtime resolver
# (graphix-types/src/expr/resolver.rs:681 `resolve_modules_int`) loads every
# `ModuleKind::Unresolved` it finds through `for_each_child`, so `mod foo;`
# inside a block or a lambda body loads `foo.gx` beside the file. The scan
# misses it, so foo.gx is classified as a project root of its own: the server
# checks it standalone and never re-checks the file that really loads it.
# Conversely a `Dynamic` module loads no file, but the scan records its name,
# so a sibling `m.gx` joins the declaring file's project and is never checked.
#
# Each case writes a two-file workspace, starts `graphix lsp` in it, sends the
# listed didOpen/didChange notifications and prints every publishDiagnostics
# (a workspace/symbol request after each step is the barrier: the server
# checks dirty roots before answering a request).
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/x-expr-walks-03.py <graphix>
#
# Both main.gx layouts of cases 1/2 pass `graphix --check` and print 42 when
# run; the script prints the --check status of each main.gx first.
#
# expected:
#   1 nested-super  open foo.gx           -> nothing (checked under main.gx)
#   2 top-super     open foo.gx           -> nothing
#   3 nested-edit   edit foo.gx to "s"    -> main.gx: type mismatch Number does not contain string
#   4 top-edit      edit foo.gx to "s"    -> main.gx: type mismatch Number does not contain string
#   5 dynamic       open foo.gx           -> foo.gx: type mismatch i64 does not contain string
#   6 dynamic-let   open foo.gx           -> foo.gx: type mismatch i64 does not contain string
# observed (HEAD c722befe, debug build):
#   1 nested-super  foo.gx -> [(0, '`super` goes above the package root')]
#   2 top-super     (nothing)
#   3 nested-edit   (nothing: main.gx is never re-checked)
#   4 top-edit      main.gx -> [(1, 'type mismatch Number does not contain string')]
#   5 dynamic       (nothing: foo.gx is attributed to main.gx and never checked)
#   6 dynamic-let   foo.gx -> [(0, 'type mismatch i64 does not contain string')]
#                   (the walk does not see a dynamic mod under a `let` either,
#                   so here foo.gx is its own root and is checked)
import json, os, queue, shutil, subprocess, sys, tempfile, threading

G = sys.argv[1] if len(sys.argv) > 1 else "graphix"

CASES = [
    ("1 nested-super",
     {"main.gx": "let base = 40;\nlet f = |y| { mod foo; foo::x + y };\nf(1)\n",
      "foo.gx": "let x = super::base + 1\n"},
     [("open", "foo.gx")]),
    ("2 top-super",
     {"main.gx": "let base = 40;\nmod foo;\nlet f = |y| foo::x + y;\nf(1)\n",
      "foo.gx": "let x = super::base + 1\n"},
     [("open", "foo.gx")]),
    ("3 nested-edit",
     {"main.gx": "let f = |y| { mod foo; foo::x + y };\nf(1)\n",
      "foo.gx": "let x = 41\n"},
     [("open", "main.gx"), ("open", "foo.gx"), ("change", "foo.gx", 'let x = "s"\n')]),
    ("4 top-edit",
     {"main.gx": "mod foo;\nlet f = |y| foo::x + y;\nf(1)\n",
      "foo.gx": "let x = 41\n"},
     [("open", "main.gx"), ("open", "foo.gx"), ("change", "foo.gx", 'let x = "s"\n')]),
    ("5 dynamic",
     {"main.gx": "mod foo dynamic {\n    sandbox unrestricted;\n    sig { val x: i64 };\n"
                 "    source sys::fs::read_all(\"foo.gx\")$\n};\nfoo::x\n",
      "foo.gx": 'let x: i64 = "s"\n'},
     [("open", "foo.gx")]),
    ("6 dynamic-let",
     {"main.gx": "let status = mod foo dynamic {\n    sandbox unrestricted;\n"
                 "    sig { val x: i64 };\n    source sys::fs::read_all(\"foo.gx\")$\n};\n"
                 "status\n",
      "foo.gx": 'let x: i64 = "s"\n'},
     [("open", "foo.gx")]),
]


def run_case(name, files, actions):
    d = os.path.realpath(tempfile.mkdtemp(prefix="xew03-"))
    try:
        for f, text in files.items():
            with open(os.path.join(d, f), "w") as h:
                h.write(text)
        chk = subprocess.run([G, "--check", "main.gx"], cwd=d, capture_output=True,
                             text=True, timeout=60)
        print(f"{name}: --check main.gx rc={chk.returncode}")
        p = subprocess.Popen([G, "lsp"], cwd=d, stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        q = queue.Queue()

        def reader():
            while True:
                headers = {}
                while True:
                    line = p.stdout.readline()
                    if not line:
                        q.put(None)
                        return
                    line = line.strip()
                    if not line:
                        break
                    k, v = line.split(b":", 1)
                    headers[k.strip().lower()] = v.strip()
                q.put(json.loads(p.stdout.read(int(headers[b"content-length"]))))

        threading.Thread(target=reader, daemon=True).start()
        nid = [10]

        def send(obj):
            b = json.dumps(obj).encode()
            p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
            p.stdin.flush()

        def barrier():
            nid[0] += 1
            send({"jsonrpc": "2.0", "id": nid[0], "method": "workspace/symbol",
                  "params": {"query": ""}})
            while True:
                m = q.get(timeout=60)
                if m is None:
                    print("   server exited")
                    return
                if m.get("id") == nid[0]:
                    return
                if m.get("method") == "textDocument/publishDiagnostics":
                    dp = m["params"]
                    msgs = [(x["range"]["start"]["line"],
                             x["message"].splitlines()[-1]) for x in dp["diagnostics"]]
                    print("  ", os.path.basename(dp["uri"]), "->",
                          msgs if msgs else "cleared")

        uri = lambda f: "file://" + os.path.join(d, f)
        send({"jsonrpc": "2.0", "id": 1, "method": "initialize",
              "params": {"processId": os.getpid(), "rootUri": "file://" + d,
                         "capabilities": {},
                         "workspaceFolders": [{"uri": "file://" + d, "name": "ws"}]}})
        send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
        barrier()
        versions = {}
        for a in actions:
            print("  ", a[0], a[1])
            if a[0] == "open":
                versions[a[1]] = 1
                send({"jsonrpc": "2.0", "method": "textDocument/didOpen",
                      "params": {"textDocument": {"uri": uri(a[1]),
                                                  "languageId": "graphix", "version": 1,
                                                  "text": files[a[1]]}}})
            else:
                versions[a[1]] += 1
                send({"jsonrpc": "2.0", "method": "textDocument/didChange",
                      "params": {"textDocument": {"uri": uri(a[1]),
                                                  "version": versions[a[1]]},
                                 "contentChanges": [{"text": a[2]}]}})
            barrier()
        send({"jsonrpc": "2.0", "id": 2, "method": "shutdown"})
        send({"jsonrpc": "2.0", "method": "exit"})
        try:
            p.wait(timeout=10)
        except subprocess.TimeoutExpired:
            p.kill()
            p.wait()
    finally:
        shutil.rmtree(d, ignore_errors=True)


for c in CASES:
    run_case(*c)
