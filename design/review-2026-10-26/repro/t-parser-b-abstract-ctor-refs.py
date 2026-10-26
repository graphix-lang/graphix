# Title: LSP references/definition miss an abstract type's constructor `T(v)` and pattern `T(p)` sites
# Command: timeout -s KILL 60 python3 -I t-parser-b-abstract-ctor-refs.py [/path/to/graphix]
# Program checked (written to a temp dir):
#   type T = Abstract<i64>;
#   let t: T = T(1);
#   let r = select t { T(n) => n };
#   r
# Expected: references on `T` (line 0 col 5) list (0,5), (1,7), (1,11), (2,19); definition on
#   the `T` of `T(1)` and of the pattern `T(n)` is (0,5).
# Observed: references [(0, 5), (1, 7)] only; definition at (1,11) and (2,19) is None.
import json, subprocess, sys, os, tempfile
G = sys.argv[1] if len(sys.argv) > 1 else "/home/eric/tmp/review-2026-10-26/bin/graphix"
d = tempfile.mkdtemp(); path = os.path.join(d, "prog.gx")
open(path, "w").write("type T = Abstract<i64>;\nlet t: T = T(1);\nlet r = select t { T(n) => n };\nr\n")
text = open(path).read(); uri = "file://" + path
p = subprocess.Popen([G, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
def send(m):
    b = json.dumps(m).encode(); p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b); p.stdin.flush()
def recv():
    h = b""
    while not h.endswith(b"\r\n\r\n"): h += p.stdout.read(1)
    n = int([l for l in h.split(b"\r\n") if l.lower().startswith(b"content-length")][0].split(b":")[1])
    return json.loads(p.stdout.read(n))
def call(i, method, params):
    send({"jsonrpc":"2.0","id":i,"method":method,"params":params})
    while True:
        m = recv()
        if m.get("id") == i: return m
send({"jsonrpc":"2.0","id":0,"method":"initialize","params":{"processId":None,"rootUri":"file://"+os.path.dirname(path),"capabilities":{}}})
while recv().get("id") != 0: pass
send({"jsonrpc":"2.0","method":"initialized","params":{}})
send({"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":uri,"languageId":"graphix","version":1,"text":text}}})
for i,(l,c) in enumerate([(0,5),(1,7),(1,11),(2,19)], start=1):
    r = call(i, "textDocument/references", {"textDocument":{"uri":uri},"position":{"line":l,"character":c},"context":{"includeDeclaration":True}})
    print("references at", (l,c), [(x["range"]["start"]["line"], x["range"]["start"]["character"]) for x in (r.get("result") or [])])
    r = call(100+i, "textDocument/definition", {"textDocument":{"uri":uri},"position":{"line":l,"character":c}})
    res = r.get("result"); 
    print("definition at", (l,c), res and (res["range"]["start"]["line"], res["range"]["start"]["character"]))
p.kill()
