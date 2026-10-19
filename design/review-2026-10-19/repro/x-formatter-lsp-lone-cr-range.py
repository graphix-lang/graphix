# Title: LSP formatting edit ends on line 0 for a document whose lines end in a lone CR
# Command: python3 x-formatter-lsp-lone-cr-range.py <path-to-graphix>
# Expected: one edit whose range covers the whole document; LSP counts "\r" as a
#   line break, so the end is {line: 3, character: 0}.
# Observed: {"end": {"character": 30, "line": 0}} -- formatting::edits counts only
#   '\n'. A client clamps character 30 to the end of line 0, replaces
#   "let x =   1;" alone, and leaves the old second and third lines after the
#   formatted text (duplicated statements). `graphix fmt` itself accepts the
#   file (the parser treats a lone CR as whitespace).
import subprocess, json, sys, os
G=sys.argv[1]; import tempfile; d=tempfile.mkdtemp(); path=os.path.join(d,"cr.gx"); text="let x =   1;\rlet y = 2;\rx + y\r"; open(path,"w",newline="").write(text)
p=subprocess.Popen([G,"lsp"],stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.DEVNULL)
def send(m):
    b=json.dumps(m).encode(); p.stdin.write(b"Content-Length: %d\r\n\r\n"%len(b)+b); p.stdin.flush()
def recv():
    h=b""
    while not h.endswith(b"\r\n\r\n"): h+=p.stdout.read(1)
    n=int([l for l in h.split(b"\r\n") if l.lower().startswith(b"content-length")][0].split(b":")[1])
    return json.loads(p.stdout.read(n))
uri="file://"+path
send({"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":None,"rootUri":"file://"+os.path.dirname(path),"capabilities":{}}})
while True:
    m=recv()
    if m.get("id")==1: break
send({"jsonrpc":"2.0","method":"initialized","params":{}})
send({"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":uri,"languageId":"graphix","version":1,"text":text}}})
send({"jsonrpc":"2.0","id":2,"method":"textDocument/formatting","params":{"textDocument":{"uri":uri},"options":{"tabSize":4,"insertSpaces":True}}})
while True:
    m=recv()
    if m.get("id")==2: print(json.dumps(m["result"])); break
send({"jsonrpc":"2.0","id":3,"method":"shutdown"}); 
send({"jsonrpc":"2.0","method":"exit"}); p.wait(timeout=5)
