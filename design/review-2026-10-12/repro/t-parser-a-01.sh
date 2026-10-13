#!/bin/sh
# t-parser-a-01: a lambda parsed as an operator's operand (`a ~ |y| ..`) or
# under `&` gets no end (lambda() never calls `.ending`; operand() and
# byref_lambda() do not add one). The LSP scope map then records it ending
# at NOWHERE, so Completer::scope_at picks it as the "statement that ended
# before the cursor" from anywhere after its start's enclosing expression.
# Run: sh design/review-2026-10-12/repro/t-parser-a-01.sh [path-to-graphix]
# Expected: completion on the blank line 3 (inside the outer block, before
#   `let h`) does not offer `zzinner`, a local of a block that does not
#   enclose the cursor (the control, `a + zzinner`, does not).
# Observed: bug -> "zzinner offered: True", byref -> True, ctl -> False.
G=${1:-/tmp/claude-1000/-home-eric-proj-graphix/4ed578a5-7145-438d-b21b-e50e92617f4d/scratchpad/bin/graphix}
D=$(mktemp -d); mkdir -p $D/bug $D/ctl $D/byref
cat > $D/bug/main.gx <<'GX'
let a = 1;
let g = {

    let h = {
        let zzinner = 5;
        a ~ |y| y + zzinner
    };
    h
};
g
GX
sed 's/a ~ |y| y + zzinner/a + zzinner/' $D/bug/main.gx > $D/ctl/main.gx
sed 's/a ~ |y| y + zzinner/\&|y| y + zzinner/' $D/bug/main.gx > $D/byref/main.gx
cat > $D/drive.py <<'PY'
import json, subprocess, sys, os, time
G = sys.argv[4]
root = os.path.abspath(sys.argv[1]); fname = "main.gx"; line=int(sys.argv[2]); col=int(sys.argv[3])
p = subprocess.Popen([G, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
def send(m):
    b = json.dumps(m).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b); p.stdin.flush()
def recv():
    h = {}
    while True:
        l = p.stdout.readline()
        if l in (b"\r\n", b""): break
        k, v = l.decode().split(":", 1); h[k.strip()] = v.strip()
    return json.loads(p.stdout.read(int(h["Content-Length"])))
def req(i, method, params):
    send({"jsonrpc":"2.0","id":i,"method":method,"params":params})
    while True:
        m = recv()
        if m.get("id") == i: return m
        if m.get("method") == "textDocument/publishDiagnostics": print("diag", json.dumps(m["params"]["diagnostics"]))
uri = "file://" + os.path.join(root, fname)
req(1, "initialize", {"capabilities":{}, "workspaceFolders":[{"uri":"file://"+root,"name":"t"}]})
send({"jsonrpc":"2.0","method":"initialized","params":{}})
text = open(os.path.join(root, fname)).read()
send({"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":uri,"languageId":"graphix","version":1,"text":text}}})
time.sleep(2)
r = req(10, "textDocument/completion", {"textDocument":{"uri":uri},"position":{"line":line,"character":col}})
res = r.get("result") or []
if isinstance(res, dict): res = res.get("items", [])
labels = sorted(x["label"] for x in res)
print(len(labels), "items; zzinner offered:", "zzinner" in labels, "; h offered:", "h" in labels)
p.kill()
PY
for v in bug byref ctl; do printf '%s: ' $v; timeout -s KILL 60 python3 -I $D/drive.py $D/$v 2 4 "$G"; done
rm -rf $D
