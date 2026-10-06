#!/usr/bin/env python3
# lsp-11: a cursor on a path's module segment (`util` in `util::bump`)
# answers for the item (`bump`).
#
# Query::target (graphix-lsp/src/query.rs:145-156) takes a reference
# when the cursor is anywhere on its written path and ANY segment equals
# the word under the cursor, then always returns the reference's bind.
# `use` items resolve each segment to its own prefix (use_segments);
# value and type paths do not.
#
# main.gx: `mod util;\nlet z = util::bump(3);\nz\n`, cursor on `util` in
# line 2.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-11.py <graphix>
#
# expected: hover `mod util`; definition util.gx 0:0 (as on `mod util`);
# references are the module's (main.gx 0:4 and this segment, 1:8).
#
# observed (HEAD c722befe, debug build):
#   hover: util::bump: fn(n: i64) -> i64
#   definition: util.gx 0:4                  (`let bump`)
#   references: main.gx 1:8, util.gx 0:4, util.gx 1:12   (bump's)
#   and references on `mod util` list only main.gx 0:4, not the segment.
import json, os, queue, subprocess, sys, tempfile, threading, urllib.parse

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"


def uri(path):
    return "file://" + urllib.parse.quote(path, safe="/-._~")


class Client:
    def __init__(self, files):
        self.dir = tempfile.mkdtemp(prefix="lsp-11-")
        for f, t in files.items():
            open(os.path.join(self.dir, f), "w").write(t)
        env = dict(os.environ, XDG_CACHE_HOME=self.dir)
        self.p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env)
        self.q, self.id = queue.Queue(), 0
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
            if m.get("id") == self.id and "method" not in m:
                return m.get("result")
        raise SystemExit(f"server died during {method}")

    def at(self, name, line, ch):
        return {"textDocument": {"uri": uri(os.path.join(self.dir, name))},
                "position": {"line": line, "character": ch}}

    def done(self):
        self.request("shutdown", None)
        self.notify("exit", None)
        self.p.wait(timeout=20)


def site(loc):
    s = loc["range"]["start"]
    return f"{loc['uri'].rsplit('/', 1)[-1]} {s['line']}:{s['character']}"


main = "mod util;\nlet z = util::bump(3);\nz\n"
c = Client({"main.gx": main, "util.gx": "let bump = |n: i64| -> i64 n + 1;\nlet other = bump(1);\n"})
c.notify("textDocument/didOpen", {"textDocument": dict(
    c.at("main.gx", 0, 0)["textDocument"], languageId="graphix", version=1, text=main)})
for label, line, ch in [("`util` in util::bump", 1, 9), ("`util` in mod util", 0, 5)]:
    print(label)
    h = c.request("textDocument/hover", c.at("main.gx", line, ch))
    print("  hover:", h and h["contents"]["value"].splitlines()[1])
    d = c.request("textDocument/definition", c.at("main.gx", line, ch))
    print("  definition:", d and site(d))
    r = c.request("textDocument/references",
                  dict(c.at("main.gx", line, ch), context={"includeDeclaration": True}))
    print("  references:", ", ".join(site(x) for x in r or []))
c.done()
