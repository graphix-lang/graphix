import json, subprocess, sys, os, time, select
B = sys.argv[1]; root = os.path.abspath(sys.argv[2]); files = sys.argv[3:]
p = subprocess.Popen([B, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
def send(m):
    b = json.dumps(m).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b); p.stdin.flush()
def recv(timeout):
    r,_,_ = select.select([p.stdout],[],[],timeout)
    if not r: return None
    hdr = b""
    while not hdr.endswith(b"\r\n\r\n"):
        c = p.stdout.read(1)
        if not c: return None
        hdr += c
    n = int([l for l in hdr.split(b"\r\n") if l.lower().startswith(b"content-length")][0].split(b":")[1])
    return json.loads(p.stdout.read(n))
send({"jsonrpc":"2.0","id":1,"method":"initialize","params":{"rootUri":"file://"+root,"capabilities":{}}})
while True:
    m = recv(20)
    if m is None or m.get("id")==1: break
send({"jsonrpc":"2.0","method":"initialized","params":{}})
for f in files:
    path = os.path.abspath(f)
    send({"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file://"+path,"languageId":"graphix","version":1,"text":open(path).read()}}})
end = time.time()+40
while time.time() < end:
    m = recv(5)
    if m is None: break
    if m.get("method") == "textDocument/publishDiagnostics":
        print(json.dumps(m["params"]))
p.kill()
