import json, subprocess, sys, os, time
G = "/tmp/claude-1000/-home-eric-proj-graphix/4ed578a5-7145-438d-b21b-e50e92617f4d/scratchpad/bin/graphix"
root = os.path.abspath(sys.argv[1]); fname = sys.argv[2]; reqs = json.loads(sys.argv[3])
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
uri = "file://" + os.path.join(root, fname)
req(1, "initialize", {"capabilities":{}, "workspaceFolders":[{"uri":"file://"+root,"name":"t"}]})
send({"jsonrpc":"2.0","method":"initialized","params":{}})
text = open(os.path.join(root, fname)).read()
send({"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":uri,"languageId":"graphix","version":1,"text":text}}})
lines = text.split("\n")
i = 10
for method, line, col in reqs:
    params = {"textDocument":{"uri":uri},"position":{"line":line,"character":col}}
    if method == "textDocument/references": params["context"] = {"includeDeclaration": True}
    r = req(i, method, params); i += 1
    res = r.get("result")
    print(method, line, col, "->")
    if isinstance(res, list):
        for loc in res:
            s = loc["range"]["start"]; e = loc["range"]["end"]
            l = lines[s["line"]]
            print("   ", s, e, repr(l[s["character"]:e["character"]]))
    else:
        print("   ", json.dumps(res))
p.kill()
