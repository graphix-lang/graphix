#!/usr/bin/env python3
# x-panics-03: the LSP panics when a failing file's path holds [ ] ^ | \
# or is not UTF-8 (expect on path_to_uri, graphix-lsp/src/state.rs:231).
#
# ServerState::diagnostic does path_to_uri(&path).or_else(|| path_to_uri(root))
# .expect(..). path_to_uri (graphix-lsp/src/uri.rs:47) percent-encodes only
# PATH_ENCODE (uri.rs:13), and lsp_types 0.97's Uri (fluent-uri 0.1.4) refuses
# [ ] ^ | \ in a path, so Uri::from_str fails; a non-UTF-8 path fails at
# to_str. Both calls return None and the server's main thread panics. The
# same hole makes ServerState::warned (state.rs:207) drop the file's warnings.
#
# Each case starts `graphix lsp` on a fresh workspace, opens a file by its
# percent-encoded file:// URI (as VS Code and Neovim send it), then hovers.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/x-panics-03.py <graphix> [workdir]
#
# expected: every case publishes its diagnostic and the hover is answered.
#
# observed (HEAD c722befe, debug build):
#   plain dir, type error : error published, hover OK
#   [x] dir, type error   : server exits 101: "panicked at
#                           graphix-lsp/src/state.rs:231:14: a checked root
#                           is an absolute path" (a^b, a|b, a\b: the same)
#   plain dir, warning    : warning published
#   [x] dir, warning      : nothing published, hover OK (warning dropped)
#   \xff.gx root (mod foo;, error in the root), foo.gx opened: exits 101,
#                           the same panic
import json, os, queue, subprocess, sys, tempfile, threading
from urllib.parse import quote

ERR = 'let x: i64 = "not an int";\nx\n'
WARN = 'let n = cast<i64>("1")?;\nn + 1\n'


def frame(msg):
    body = json.dumps(msg).encode()
    return b"Content-Length: %d\r\n\r\n" % len(body) + body


def reader(stream, q):
    while True:
        n = None
        while True:
            line = stream.readline()
            if not line:
                return q.put(None)
            line = line.strip()
            if not line:
                break
            k, v = line.split(b":", 1)
            if k.strip().lower() == b"content-length":
                n = int(v)
        q.put(json.loads(stream.read(n)))


def uri(path):
    return "file://" + quote(os.fsencode(path), safe="/")


def case(graphix, work, name, files, opened):
    ws = os.path.join(work, "ws_%d" % case.n)
    case.n += 1
    for rel, text in files:
        p = os.path.join(os.fsencode(ws), os.fsencode(rel))
        os.makedirs(os.path.dirname(p), exist_ok=True)
        open(p, "w").write(text)
    env = dict(os.environ, RUST_BACKTRACE="0",
               XDG_CACHE_HOME=os.path.join(work, "cache"),
               XDG_CONFIG_HOME=os.path.join(work, "config"))
    p = subprocess.Popen(["timeout", "-s", "KILL", "120", graphix, "lsp"],
                         stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE, env=env)
    q = queue.Queue()
    threading.Thread(target=reader, args=(p.stdout, q), daemon=True).start()
    err = []
    t = threading.Thread(target=lambda: err.extend(p.stderr), daemon=True)
    t.start()
    ids = iter(range(1, 100))

    def send(msg):
        try:
            p.stdin.write(frame(dict(msg, jsonrpc="2.0")))
            p.stdin.flush()
        except OSError:
            pass

    def request(method, params):
        rid = next(ids)
        send({"id": rid, "method": method, "params": params})
        while True:
            try:
                m = q.get(timeout=90)
            except queue.Empty:
                return "TIMEOUT"
            if m is None:
                return "EOF, exit %s" % p.wait()
            if m.get("method") == "textDocument/publishDiagnostics":
                d = m["params"]["diagnostics"]
                published.append([x["message"] for x in d])
            elif m.get("id") == rid:
                return "OK"

    published = []
    root = uri(ws)
    request("initialize", {"processId": os.getpid(), "rootUri": root, "capabilities": {},
                           "workspaceFolders": [{"uri": root, "name": "ws"}]})
    send({"method": "initialized", "params": {}})
    doc = os.path.join(os.fsencode(ws), os.fsencode(opened))
    text = dict(files)[opened]
    send({"method": "textDocument/didOpen", "params": {"textDocument": {
        "uri": uri(doc), "languageId": "graphix", "version": 1, "text": text}}})
    hover = request("textDocument/hover", {"textDocument": {"uri": uri(doc)},
                                           "position": {"line": 0, "character": 4}})
    if hover == "OK":
        request("shutdown", None)
        send({"method": "exit", "params": None})
        p.wait(timeout=20)
    else:
        p.kill()
        p.wait()
    t.join(timeout=5)
    print("%-26s published %s, hover %s" % (name, published, hover))
    for line in err:
        line = line.decode(errors="replace").strip()
        if "panicked" in line or "checked root" in line:
            print("    " + line)


case.n = 0


def main():
    graphix = sys.argv[1]
    work = os.path.realpath(sys.argv[2] if len(sys.argv) > 2 else tempfile.mkdtemp())
    case(graphix, work, "plain dir, type error", [("plain/main.gx", ERR)], "plain/main.gx")
    for d in ["[x]", "a^b", "a|b", "a\\b"]:
        case(graphix, work, "%s dir, type error" % d, [(d + "/main.gx", ERR)], d + "/main.gx")
    case(graphix, work, "plain dir, warning", [("plain/main.gx", WARN)], "plain/main.gx")
    case(graphix, work, "[x] dir, warning", [("[x]/main.gx", WARN)], "[x]/main.gx")
    root = b"\xff.gx".decode("utf-8", "surrogateescape")
    case(graphix, work, "non-UTF-8 root, error",
         [(root, 'mod foo;\nlet x: i64 = "not an int";\nx\n'), ("foo.gx", "let y = 1\n")],
         "foo.gx")


main()
