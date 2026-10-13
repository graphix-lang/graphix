# lsp-02: a compiler panic during an LSP check permanently kills the backend runtime.
# One buffer whose check panics (here genn::apply_inner's assert_eq, reached by
# binding 'array_group with a zero-argument callback) ends the GX task: ToGX::Check
# in graphix-rt/src/gx.rs has no unwind guard. Every later check of ANY root then
# publishes "runtime is dead" at 0:0 and queries return nothing, until restart.
#
# command: timeout -s KILL 60 python3 -I design/review-2026-10-12/repro/lsp-02.py /path/to/graphix
# expected: bad.gx gets an error (or a "the check panicked" diagnostic); good.gx
#           gets no diagnostics and the hover answers "x: i64" (as it does when
#           good.gx is opened alone)
# observed (debug build at 7324207f):
#   DIAG bad.gx  [(0:0, 'runtime did not respond')]
#   DIAG good.gx [(0:0, 'runtime is dead')]
#   RESP 2 None            (the hover on good.gx)
#   stderr: panicked at graphix-compiler/src/node/genn.rs:105:5:
#           a generated call supplies every formal  left: 2 right: 0
import tempfile
D = tempfile.mkdtemp()
open(D + '/bad.gx', 'w').write("let g = |a: i64, f: fn() -> i64| -> i64 'array_group;\n0\n")
open(D + '/good.gx', 'w').write("let x: i64 = 1;\nx + 1\n")
import subprocess, json, sys, os, threading, queue, time
G, d = sys.argv[1], D
env = dict(os.environ, XDG_CACHE_HOME=os.path.join(d, '..', 'cache'))
p = subprocess.Popen([G, 'lsp'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env)
q = queue.Queue()
def reader():
    f = p.stdout
    while True:
        hdr = b''
        while not hdr.endswith(b'\r\n\r\n'):
            c = f.read(1)
            if not c: return
            hdr += c
        n = int([l for l in hdr.split(b'\r\n') if l.lower().startswith(b'content-length')][0].split(b':')[1])
        q.put(json.loads(f.read(n)))
threading.Thread(target=reader, daemon=True).start()
def send(m):
    b = json.dumps(m).encode(); p.stdin.write(b'Content-Length: %d\r\n\r\n' % len(b) + b); p.stdin.flush()
def drain(t):
    end = time.time() + t
    while time.time() < end:
        try: m = q.get(timeout=0.2)
        except queue.Empty: continue
        if m.get('method') == 'textDocument/publishDiagnostics':
            pr = m['params']; print('DIAG', os.path.basename(pr['uri']), [(x['range']['start'], x['message'][:120]) for x in pr['diagnostics']])
        elif 'id' in m and 'method' not in m:
            r = m.get('result'); print('RESP', m['id'], json.dumps(r)[:200] if r is not None else m.get('error'))
uri = lambda n: 'file://' + os.path.join(d, n)
send({'jsonrpc':'2.0','id':1,'method':'initialize','params':{'processId':None,'rootUri':None,'capabilities':{}}})
drain(3)
send({'jsonrpc':'2.0','method':'initialized','params':{}})
def open_(n, ver=1):
    send({'jsonrpc':'2.0','method':'textDocument/didOpen','params':{'textDocument':{'uri':uri(n),'languageId':'graphix','version':ver,'text':open(os.path.join(d,n)).read()}}})
which = ['bad.gx', 'good.gx']
for n in which:
    print('--- open', n); open_(n); drain(6)
print('--- hover good.gx')
send({'jsonrpc':'2.0','id':2,'method':'textDocument/hover','params':{'textDocument':{'uri':uri('good.gx')},'position':{'line':1,'character':0}}})
drain(4)
send({'jsonrpc':'2.0','id':3,'method':'shutdown'}); drain(1)
send({'jsonrpc':'2.0','method':'exit'})
try: p.wait(5)
except Exception: p.kill()
print('STDERR:', p.stderr.read().decode()[-1500:])
