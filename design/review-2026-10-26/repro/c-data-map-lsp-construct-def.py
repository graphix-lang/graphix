# Title: LSP go-to-definition on an abstract type's constructor `Counter(1)`
#        (and its pattern `Counter(n)`) answers nothing; the same name in an
#        annotation answers the typedef.
# Command:
#   timeout -s KILL 60 python3 -I design/review-2026-10-26/repro/c-data-map-lsp-construct-def.py \
#       /home/eric/tmp/review-2026-10-26/bin/graphix
# Expected: all three lines resolve to `type |Counter` (line 0, characters 5..12).
# Observed:
#   Counter(1) ctor -> null
#   let d: Counter annot -> {"range": {"start": {"line": 0, "character": 5}, ...}}
#   pattern Counter(n) -> null
import json, os, shutil, subprocess, sys, tempfile, time

SRC = """type Counter = Abstract<i64>;
let c = Counter(1);
let d: Counter = c;
select d { Counter(n) => n }
"""
root = tempfile.mkdtemp()
path = os.path.join(root, "main.gx")
open(path, "w").write(SRC)
uri = "file://" + path
p = subprocess.Popen([sys.argv[1], "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=subprocess.DEVNULL)

def send(m):
    b = json.dumps(m).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b)
    p.stdin.flush()

def recv():
    h = b""
    while not h.endswith(b"\r\n\r\n"):
        c = p.stdout.read(1)
        if not c:
            return None
        h += c
    n = int([l for l in h.split(b"\r\n") if l.lower().startswith(b"content-length")][0].split(b":")[1])
    return json.loads(p.stdout.read(n))

def answer(i):
    while True:
        m = recv()
        if m is None or m.get("id") == i:
            return m

send({"jsonrpc": "2.0", "id": 1, "method": "initialize",
      "params": {"rootUri": "file://" + root, "capabilities": {}}})
answer(1)
send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {
    "uri": uri, "languageId": "graphix", "version": 1, "text": SRC}}})
time.sleep(3)
for i, (line, col, what) in enumerate(
        [(1, 9, "Counter(1) ctor"), (2, 8, "let d: Counter annot"), (3, 12, "pattern Counter(n)")], 10):
    send({"jsonrpc": "2.0", "id": i, "method": "textDocument/definition", "params": {
        "textDocument": {"uri": uri}, "position": {"line": line, "character": col}}})
    print(what, "->", json.dumps(answer(i).get("result")))
p.kill()
shutil.rmtree(root)
