#!/usr/bin/env python3
# lsp-04: a root that a save absorbs into a project keeps its stale
# diagnostics and its check.
#
# ServerState keeps per-root state (checked, warnings, diagnosed, dirty)
# keyed by root path (graphix-lsp/src/state.rs:72-77). `saved()` (163)
# rescans the project graph but never retires a root the rescan removed,
# and `close_document` (142) only visits the roots `roots_of(path)` names
# now. helper.gx, opened alone, is its own root and gets an error. Once
# main.gx gains `mod helper;` and is saved, helper.gx is checked only
# under main (hover answers from main's clean check), but nothing ever
# publishes for helper.gx again: main's check had nothing on it before
# or after, and the old helper root is never checked, cleared or dropped.
# The stale list goes only if main's check later publishes for helper.gx
# (an error on it, then fixed) or helper.gx becomes a root again.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/lsp-04.py <graphix>
#
# expected: from step 2 on, helper.gx has no diagnostics (main's project
#   checks clean, and helper.gx is no longer a root).
# observed (HEAD c722befe, debug build):
#   1 helper.gx alone          helper.gx: ['`super` goes above the package root']
#   2 main.gx mods it, saved   helper.gx: ['`super` goes above the package root']
#     hover on helper's cfg:   cfg: i64   (answered from main's check)
#     graphix --check main.gx: exit 0
#   3 helper.gx edited         helper.gx: ['`super` goes above the package root']
#   4 helper.gx closed         helper.gx: ['`super` goes above the package root']
#   5 main.gx closed           helper.gx: ['`super` goes above the package root']
#   publishDiagnostics for helper.gx in the whole session: 1 (the step 1 error)
import atexit, json, os, queue, shutil, subprocess, sys, tempfile, threading, urllib.parse

graphix = sys.argv[1] if len(sys.argv) > 1 else "graphix"
tmp = os.path.realpath(tempfile.mkdtemp(prefix="lsp-04."))
atexit.register(shutil.rmtree, tmp, True)
proj = os.path.join(tmp, "proj")
os.makedirs(proj)
MAIN1 = "let cfg = 1;\ncfg\n"
MAIN2 = "let cfg = 1;\nmod helper;\nhelper::h\n"
HELPER = "use super::cfg;\nlet h = cfg + 1;\n"
open(os.path.join(proj, "main.gx"), "w").write(MAIN1)
open(os.path.join(proj, "helper.gx"), "w").write(HELPER)
env = dict(os.environ, XDG_CACHE_HOME=os.path.join(tmp, "cache"))


def uri(name):
    return "file://" + urllib.parse.quote(os.path.join(proj, name))


proc = subprocess.Popen([graphix, "lsp"], env=env, stdin=subprocess.PIPE,
                        stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
atexit.register(proc.kill)
inbox = queue.Queue()


def reader():
    f = proc.stdout
    while True:
        line = f.readline()
        if not line:
            inbox.put(None)
            return
        if line.lower().startswith(b"content-length:"):
            n = int(line.split(b":")[1])
            while f.readline() not in (b"\r\n", b"\n", b""):
                pass
            inbox.put(json.loads(f.read(n)))


threading.Thread(target=reader, daemon=True).start()
standing = {}
published = []
next_id = [0]


def send(msg):
    b = json.dumps(dict(msg, jsonrpc="2.0")).encode()
    proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
    proc.stdin.flush()


def notify(method, params):
    send({"method": method, "params": params})


def request(method, params):
    next_id[0] += 1
    rid = next_id[0]
    send({"id": rid, "method": method, "params": params})
    while True:
        m = inbox.get(timeout=150)
        if m is None:
            sys.exit("the server died")
        if m.get("method") == "textDocument/publishDiagnostics":
            p = m["params"]
            name = p["uri"].rsplit("/", 1)[-1]
            standing[name] = [d["message"] for d in p["diagnostics"]]
            published.append(name)
        if m.get("id") == rid and "method" not in m:
            return m.get("result")


def sync():
    # the server checks what is dirty before it answers a request
    request("workspace/symbol", {"query": "\u0000"})


def show(step):
    sync()
    print(f"{step:<26} helper.gx: {standing.get('helper.gx', [])}")


request("initialize", {"processId": None, "rootUri": None, "capabilities": {},
                       "workspaceFolders": [{"uri": "file://" + urllib.parse.quote(proj),
                                             "name": "proj"}]})
notify("initialized", {})
version = {"main.gx": 1, "helper.gx": 1}


def open_doc(name, text):
    notify("textDocument/didOpen", {"textDocument": {
        "uri": uri(name), "languageId": "graphix", "version": 1, "text": text}})


def edit(name, text):
    version[name] += 1
    notify("textDocument/didChange", {
        "textDocument": {"uri": uri(name), "version": version[name]},
        "contentChanges": [{"text": text}]})


open_doc("helper.gx", HELPER)
open_doc("main.gx", MAIN1)
show("1 helper.gx alone")
edit("main.gx", MAIN2)
open(os.path.join(proj, "main.gx"), "w").write(MAIN2)
notify("textDocument/didSave", {"textDocument": {"uri": uri("main.gx")}})
show("2 main.gx mods it, saved")
hover = request("textDocument/hover", {"textDocument": {"uri": uri("helper.gx")},
                                       "position": {"line": 1, "character": 9}})
hover = (hover or {}).get("contents", {}).get("value", "")
print("  hover on helper's cfg:  ", " ".join(l for l in hover.splitlines() if not l.startswith("```")))
check = subprocess.run([graphix, "--check", os.path.join(proj, "main.gx")], env=env,
                       capture_output=True, text=True, timeout=60)
print("  graphix --check main.gx: exit", check.returncode, check.stderr.strip()[-200:])
edit("helper.gx", HELPER + "let z = h;\n")
show("3 helper.gx edited")
notify("textDocument/didClose", {"textDocument": {"uri": uri("helper.gx")}})
show("4 helper.gx closed")
notify("textDocument/didClose", {"textDocument": {"uri": uri("main.gx")}})
show("5 main.gx closed")
print("publishDiagnostics for helper.gx in the whole session:", published.count("helper.gx"))
request("shutdown", None)
notify("exit", None)
proc.wait(timeout=20)
