# x-panics-03: http::get / http::request read a response body whole with no
# limit (resp.text() / resp.bytes(), then a second copy into an ArcStr), so a
# server that streams an endless chunked body grows the client until the
# kernel kills it. http::serve bounds request bodies (#max_body); the client
# side has no bound and the default client no timeout.
#
# usage: timeout -s KILL 120 python3 -I x-panics-03.py /path/to/graphix
# expected: an HTTPError once some limit is passed
# observed: the graphix process is OOM-killed (6 GB cap) after ~6 s, ~6 GB sent
import socket, subprocess, sys, threading, time, tempfile, os
G = sys.argv[1]
s = socket.socket(); s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("127.0.0.1", 0)); s.listen(1)
port = s.getsockname()[1]
sent = [0]
def serve():
    c, _ = s.accept()
    c.recv(65536)
    c.sendall(b"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nContent-Type: text/plain\r\n\r\n")
    chunk = b"a" * (1 << 20)
    hdr = b"%x\r\n" % len(chunk)
    try:
        while True:
            c.sendall(hdr + chunk + b"\r\n"); sent[0] += 1
    except Exception:
        pass
threading.Thread(target=serve, daemon=True).start()
prog = f'''let c = http::default_client(null)$;
let r = http::get(c, "http://127.0.0.1:{port}/");
select r {{ error as e => "error [e]", v => "len [str::len(v.body)]" }}
'''
d = tempfile.mkdtemp()
p = os.path.join(d, "x-panics-03.gx")
open(p, "w").write(prog)
t = time.time()
r = subprocess.run(["timeout", "-s", "KILL", "100", G, "--no-cache", p], capture_output=True, text=True)
print("rc", r.returncode, "secs", round(time.time() - t, 1), "MiB sent", sent[0])
print(r.stdout[-400:], r.stderr[-400:])
