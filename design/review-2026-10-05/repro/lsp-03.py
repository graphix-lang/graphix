#!/usr/bin/env python3
# lsp-03: a file in two projects shows only the last-published root's
# diagnostics.
#
# ServerState::check (graphix-lsp/src/state.rs:191-199) returns, for one
# root, the whole list of every file that root has diagnostics on, and an
# empty list for every file it had diagnostics on before; close_document
# (state.rs:154-155) clears every file of a root nothing edits any more.
# server.rs::publish sends each as textDocument/publishDiagnostics, which
# REPLACES the client's list for that file. A file `mod`ed by two roots
# (the shared util workspace::scan models) therefore shows whichever root
# published last, and one root clears an error the other still has.
#
# Three files: tool_a.gx and tool_b.gx are both `mod util; util::g`.
#   1. util.gx reads `super::cfg`, which only tool_b defines, and has an
#      uncaught `?`. Opening util.gx checks tool_a (fails on util.gx), then
#      tool_b (passes, warns on util.gx).
#   2. util.gx has its own type error, which both roots report. A parse
#      error typed into tool_a.gx rechecks tool_a alone.
#   3. Same util.gx, tool_a.gx and tool_b.gx open; tool_b.gx is closed.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-03.py <graphix>
#
# expected: util.gx keeps an ERROR in all three, since tool_a (1) and
# tool_b (2), and tool_a (3), still do not compile.
#
# observed (HEAD c722befe, debug build):
#   1 publish util.gx [ERROR cfg not defined]
#     publish util.gx [WARNING error raised by ? will not be caught]
#     util.gx stands: [WARNING error raised by ? will not be caught]
#   2 publish tool_a.gx [ERROR Parse error at line: 2, column: 9]
#     publish util.gx []
#     util.gx stands: []
#   3 publish util.gx []
#     util.gx stands: []
import json, os, queue, subprocess, sys, tempfile, threading, time, urllib.parse

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"
SEV = {1: "ERROR", 2: "WARNING"}


def uri(path):
    return "file://" + urllib.parse.quote(path, safe="/-._~")


class Client:
    def __init__(self, files):
        self.dir = tempfile.mkdtemp(prefix="lsp-03-")
        for f, t in files.items():
            open(os.path.join(self.dir, f), "w").write(t)
        env = dict(os.environ, XDG_CACHE_HOME=self.dir)
        self.p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env)
        self.q, self.id, self.standing = queue.Queue(), 0, {}
        threading.Thread(target=self.reader, daemon=True).start()
        self.request("initialize", {"processId": None, "rootUri": None, "capabilities": {},
                                    "workspaceFolders": [{"uri": uri(self.dir), "name": "w"}]})
        self.notify("initialized", {})

    def reader(self):
        while line := self.p.stdout.readline():
            if line.lower().startswith(b"content-length:"):
                n = int(line.split(b":")[1])
                while self.p.stdout.readline() not in (b"\r\n", b"\n", b""):
                    pass
                self.q.put(json.loads(self.p.stdout.read(n)))
        self.q.put(None)

    def send(self, msg):
        body = json.dumps(dict(msg, jsonrpc="2.0")).encode()
        self.p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.p.stdin.flush()

    def notify(self, method, params):
        self.send({"method": method, "params": params})

    def request(self, method, params):
        self.id += 1
        self.send({"id": self.id, "method": method, "params": params})
        while (m := self.q.get(timeout=100)) is not None:
            if m.get("method") == "textDocument/publishDiagnostics":
                f = m["params"]["uri"].rsplit("/", 1)[-1]
                d = [f"{SEV[x['severity']]} {x['message'].splitlines()[0]}"
                     for x in m["params"]["diagnostics"]]
                print(f"    publish {f} {d}")
                self.standing[f] = d
            if m.get("id") == self.id and "method" not in m:
                return m.get("result")
        raise SystemExit(f"server died during {method}")

    def sync(self):
        # an idle server checks every dirty root before it answers
        self.request("workspace/symbol", {"query": "zzz"})

    def doc(self, name):
        return {"uri": uri(os.path.join(self.dir, name))}

    def open(self, name, text):
        self.notify("textDocument/didOpen", {"textDocument": dict(
            self.doc(name), languageId="graphix", version=1, text=text)})

    def done(self):
        print(f"    util.gx stands: {self.standing.get('util.gx')}")
        self.request("shutdown", None)
        self.notify("exit", None)
        self.p.wait(timeout=20)


tool = "mod util;\nutil::g\n"

print("1. tool_a fails on util.gx, tool_b warns on it; open util.gx")
util = 'use super::cfg;\nlet n = cast<i64>("1")?;\nlet g = cfg + n;\n'
c = Client({"util.gx": util, "tool_a.gx": tool, "tool_b.gx": "let cfg = 1;\n" + tool})
c.open("util.gx", util)
c.sync()
c.done()

print("2. both roots fail on util.gx; a parse error typed into tool_a.gx")
util = 'let g = 1 + "no";\n'
c = Client({"util.gx": util, "tool_a.gx": tool, "tool_b.gx": tool})
c.open("util.gx", util)
c.open("tool_a.gx", tool)
c.sync()
print("    --- edit")
c.notify("textDocument/didChange", {"textDocument": dict(c.doc("tool_a.gx"), version=2),
                                    "contentChanges": [{"text": "mod util;\nlet x = ;\nutil::g\n"}]})
c.sync()
c.done()

print("3. both roots fail on util.gx; close tool_b.gx, tool_a.gx stays open")
c = Client({"util.gx": util, "tool_a.gx": tool, "tool_b.gx": tool})
c.open("tool_a.gx", tool)
c.open("tool_b.gx", tool)
c.sync()
print("    --- close")
c.notify("textDocument/didClose", {"textDocument": c.doc("tool_b.gx")})
c.sync()
c.done()
