#!/usr/bin/env python3
# lsp-06: the server builds its own URIs, so an unsaved buffer's
# diagnostics are placed against the file on disk.
#
# ServerState keys `documents` by the URI string the client sent
# (state.rs:137; lsp_types::Uri compares as_str), but rebuilds a file's
# URI from its path with uri::path_to_uri, which encodes only
# space " # < > ? ` { } %. VS Code (vscode-languageclient, what
# ide/editors/vscode uses) also encodes ( ) ! $ & ' * + , ; = : @.
# For a file under `proj (copy)/` the lookups by the rebuilt URI miss:
#   state.rs:237  the error's range is encoded against the disk text,
#   server.rs:141 the publish carries no version,
#   symbols.rs:131 workspace/symbol parses the disk text,
# and every diagnostic is published under a URI the client never sent.
#
# Each case runs its own `graphix lsp`, opens three files as they are on
# disk, edits them (unsaved), and prints what the server publishes:
#   main.gx: disk 2 lines; buffer 5 lines, a type error on line 3 at `x`.
#   na.gx:   line 1 has "AAA" on disk and three U+1D49C in the buffer
#            (2 UTF-16 units each); the type error's `x` follows them.
#   pe.gx:   a parse error at (1,13) whose underline is the rest of a
#            line; on disk that line is an identifier.
# then asks workspace/symbol for `newsym`, declared only in main.gx's
# buffer.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-06.py <graphix>
#
# expected: all three cases print what `plain` prints.
#
# observed (HEAD c722befe, debug build):
#   == plain, client URIs as VS Code spells them
#     na.gx: client's URI, version 2, range (1,34)-(1,35)
#     pe.gx: client's URI, version 2, range (1,13)-(1,15)
#     main.gx: client's URI, version 2, range (3,16)-(3,17)
#     workspace/symbol newsym: [(1, 4)]
#   == proj (copy), client URIs as VS Code spells them
#     na.gx: OTHER URI .../proj%20(copy)/na.gx, version None, range (1,31)-(1,32)
#     pe.gx: OTHER URI .../proj%20(copy)/pe.gx, version None, range (1,13)-(1,30)
#     main.gx: OTHER URI .../proj%20(copy)/main.gx, version None, range (3,0)-(3,0)
#     workspace/symbol newsym: []
#   == proj (copy), client URIs as path_to_uri (Helix, Neovim) spells them
#     na.gx: client's URI, version 2, range (1,34)-(1,35)
#     pe.gx: client's URI, version 2, range (1,13)-(1,15)
#     main.gx: client's URI, version 2, range (3,16)-(3,17)
#     workspace/symbol newsym: [(1, 4)]
import json, os, queue, shutil, subprocess, sys, tempfile, threading, urllib.parse

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"


def uri_vscode(path):
    # vscode-uri: everything but unreserved characters and `/` is encoded
    return "file://" + urllib.parse.quote(path, safe="/-._~")


def uri_server(path):
    # graphix-lsp's path_to_uri
    return "file://" + urllib.parse.quote(path, safe="/-._~!$&'()*+,;=:@")


A3 = "\U0001D49C" * 3
FILES = {
    "na.gx": ('let x = 1;\nlet s = "AAA"; let y: string = x;\ny\n',
              'let x = 1;\nlet s = "%s"; let y: string = x;\ny\n' % A3),
    "pe.gx": ("let x = 1;\nlet abcdefghijklmnopqrstuvwxyz = 2;\nx\n",
              "let x = 1;\nlet y = (x + );\ny\n"),
    "main.gx": ("let x = 1;\nx\n",
                "let x = 1;\nlet newsym = 2;\nlet b = 3;\nlet y: string = x;\ny\n"),
}


class Client:
    def __init__(self, root, mk):
        self.mk = mk
        env = dict(os.environ, XDG_CACHE_HOME=root)
        self.p = subprocess.Popen([GX, "lsp"], stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, env=env)
        self.q, self.id, self.published = queue.Queue(), 0, []
        threading.Thread(target=self.reader, daemon=True).start()
        self.request("initialize", {"processId": None, "rootUri": None, "capabilities": {},
                                    "workspaceFolders": [{"uri": mk(root), "name": "w"}]})
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
                self.published.append(m["params"])
            if m.get("id") == self.id and "method" not in m:
                return m.get("result")
        raise SystemExit(f"server died during {method}")

    def sync(self):
        # an idle server checks every dirty root before it answers
        self.request("workspace/symbol", {"query": "zzz"})

    def done(self):
        self.request("shutdown", None)
        self.notify("exit", None)
        self.p.wait(timeout=20)


def case(top, n, d, mk, label):
    root = os.path.join(top, str(n), d)
    os.makedirs(root)
    for f, (disk, _) in FILES.items():
        open(os.path.join(root, f), "w").write(disk)
    print(f"== {d}, client URIs as {label} spells them")
    c = Client(root, mk)
    uris = {f: mk(os.path.join(root, f)) for f in FILES}
    for f, (disk, _) in FILES.items():
        c.notify("textDocument/didOpen", {"textDocument": {
            "uri": uris[f], "languageId": "graphix", "version": 1, "text": disk}})
    c.sync()
    c.published.clear()
    for f, (_, buf) in FILES.items():
        c.notify("textDocument/didChange", {
            "textDocument": {"uri": uris[f], "version": 2}, "contentChanges": [{"text": buf}]})
    c.sync()
    for p in c.published:
        f = p["uri"].rsplit("/", 1)[1]
        tail = "/".join(p["uri"].rsplit("/", 2)[1:])
        spelled = "client's URI" if p["uri"] == uris[f] else f"OTHER URI .../{tail}"
        for dg in p["diagnostics"]:
            s, e = dg["range"]["start"], dg["range"]["end"]
            print(f"  {f}: {spelled}, version {p.get('version')}, range "
                  f"({s['line']},{s['character']})-({e['line']},{e['character']})")
    syms = c.request("workspace/symbol", {"query": "newsym"}) or []
    at = [(s["location"]["range"]["start"]["line"],
           s["location"]["range"]["start"]["character"]) for s in syms]
    print(f"  workspace/symbol newsym: {at}")
    c.done()


top = tempfile.mkdtemp(prefix="lsp-06-")
try:
    case(top, 1, "plain", uri_vscode, "VS Code")
    case(top, 2, "proj (copy)", uri_vscode, "VS Code")
    case(top, 3, "proj (copy)", uri_server, "path_to_uri (Helix, Neovim)")
finally:
    shutil.rmtree(top, ignore_errors=True)
