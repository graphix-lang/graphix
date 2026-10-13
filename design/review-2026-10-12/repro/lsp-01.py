"""lsp-01: Project graph treats non-root files as roots, mis-resolving their
`mod`s and un-rooting real scripts (graphix-lsp/src/workspace.rs, build_projects).

Tree:  app.gx        mod util; util::v
       util.gx       mod tool; let v = tool::w   (a module: tool is util/tool.gx)
       util/tool.gx  let w = 1
       tool.gx       mod lib; lib::x + 1         (a root script of its own)
       lib.gx        let x = 1

build_projects runs bfs_from_root(util.gx) as if util.gx were a root, which
resolves its `mod tool` beside it to ./tool.gx; tool.gx is then "imported",
roots no project, and lib.gx belongs to no project, so an edit to lib.gx
re-checks lib.gx alone and never tool.gx.

Run:      timeout -s KILL 170 python3 -I lsp-01.py path/to/graphix
Control:  timeout -s KILL 170 python3 -I lsp-01.py path/to/graphix --control
          (util.gx without `mod tool`)
Expected (both):  after lib edit: tool.gx ['lib::x not defined']
Observed (HEAD 7324207f, debug):
  bug run:  nothing published for tool.gx after the lib edit; only
            "after tool touch: tool.gx ['lib::x not defined']"
  control:  "after lib edit: tool.gx ['lib::x not defined']"
"""
import json, os, subprocess, sys, tempfile, threading, queue, time

BIN = sys.argv[1] if len(sys.argv) > 1 else "graphix"
CONTROL = "--control" in sys.argv
root = tempfile.mkdtemp()
files = {
    "app.gx": "mod util;\nutil::v\n",
    "util.gx": "mod tool;\nlet v = tool::w;\n",
    "util/tool.gx": "let w = 1;\n",
    "tool.gx": "mod lib;\nlib::x + 1\n",
    "lib.gx": "let x = 1;\n",
}
if CONTROL:
    files["util.gx"] = "let v = 1;\n"
for name, text in files.items():
    os.makedirs(os.path.dirname(os.path.join(root, name)), exist_ok=True)
    open(os.path.join(root, name), "w").write(text)

env = dict(os.environ, XDG_CACHE_HOME=os.path.join(root, ".cache"))
p = subprocess.Popen([BIN, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=subprocess.DEVNULL, env=env)
q = queue.Queue()

def reader():
    while True:
        hdr = b""
        while not hdr.endswith(b"\r\n\r\n"):
            c = p.stdout.read(1)
            if not c:
                return
            hdr += c
        n = int([l for l in hdr.decode().split("\r\n") if l.lower().startswith("content-length")][0].split(":")[1])
        q.put(json.loads(p.stdout.read(n)))

threading.Thread(target=reader, daemon=True).start()
ids = [0]

def send(m):
    b = json.dumps(m).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
    p.stdin.flush()

def request(method, params):
    ids[0] += 1
    send({"jsonrpc": "2.0", "id": ids[0], "method": method, "params": params})
    seen = []
    while True:
        m = q.get(timeout=120)
        if m.get("id") == ids[0] and "method" not in m:
            return seen
        seen.append(m)

def notify(method, params):
    send({"jsonrpc": "2.0", "method": method, "params": params})

def uri(f):
    return "file://" + os.path.join(root, f)

def barrier(label):
    for m in request("workspace/symbol", {"query": "\u0000"}):
        if m.get("method") == "textDocument/publishDiagnostics":
            prm = m["params"]
            print(label, os.path.basename(prm["uri"]), [d["message"] for d in prm["diagnostics"]])

request("initialize", {"processId": None, "rootUri": "file://" + root, "capabilities": {}})
notify("initialized", {})
for f in ["tool.gx", "lib.gx"]:
    notify("textDocument/didOpen", {"textDocument": {"uri": uri(f), "languageId": "graphix",
                                                     "version": 1, "text": files[f]}})
barrier("open:")
notify("textDocument/didChange", {"textDocument": {"uri": uri("lib.gx"), "version": 2},
                                  "contentChanges": [{"text": "let y = 1;\n"}]})
print("-- renamed x to y in lib.gx; tool.gx should now fail:")
barrier("after lib edit:")
notify("textDocument/didChange", {"textDocument": {"uri": uri("tool.gx"), "version": 2},
                                  "contentChanges": [{"text": files["tool.gx"]}]})
print("-- touched tool.gx:")
barrier("after tool touch:")
request("shutdown", None)
notify("exit", None)
p.wait(timeout=10)
