# x-security-01: http::serve has no header-read, keep-alive-idle or body timeout;
# max_connections idle sockets lock every other client out for as long as they stay open.
# usage: timeout -s KILL 170 python3 -I x-security-01.py /path/to/graphix
# expected: every curl after the 35 s wait answers (rc 0): hyper's own default is a
#   30 s header-read timeout, and an idle/stalled connection should be dropped.
# observed (HEAD 7324207f):
#   baseline curl rc 0
#   4 silent conns, curl rc 28
#   35 s later, curl rc 28
#   closed, curl rc 0
#   4 idle keep-alive conns after 35 s, curl rc 28
#   4 stalled bodies after 35 s, curl rc 28
#   closed, curl rc 0
# (with the default #max_connections 768 the same holds with 768 silent sockets)
import os, signal, socket, subprocess, sys, tempfile, time
PORT, N, WAIT = 38932, 4, 35
PROG = """let s = http::serve(
    #addr: "127.0.0.1:%d",
    #max_connections: %d,
    #handler: |req: http::Request| { body: "ok [req.path]", headers: [], status: u16:200, url: "" }
);
s
""" % (PORT, N)
d = tempfile.mkdtemp(prefix="xsec01-")
prog = os.path.join(d, "xsec01.gx")
open(prog, "w").write(PROG)
g = subprocess.Popen([sys.argv[1], "--no-cache", prog], stdout=subprocess.DEVNULL,
                     stderr=subprocess.DEVNULL, start_new_session=True)
def req():
    return subprocess.run(["curl", "-s", "-m", "3", f"http://127.0.0.1:{PORT}/x"],
                          capture_output=True).returncode
def conn(first=b""):
    s = socket.create_connection(("127.0.0.1", PORT))
    if first: s.sendall(first)
    return s
try:
    time.sleep(6)
    print("baseline curl rc", req(), flush=True)
    socks = [conn() for _ in range(N)]
    time.sleep(1)
    print(f"{N} silent conns, curl rc", req(), flush=True)
    time.sleep(WAIT)
    print(f"{WAIT} s later, curl rc", req(), flush=True)
    for s in socks: s.close()
    time.sleep(1)
    print("closed, curl rc", req(), flush=True)
    socks = []
    for _ in range(N):
        s = conn(b"GET /k HTTP/1.1\r\nHost: x\r\n\r\n"); s.recv(4096); socks.append(s)
    time.sleep(WAIT)
    print(f"{N} idle keep-alive conns after {WAIT} s, curl rc", req(), flush=True)
    for s in socks: s.close()
    socks = [conn(b"POST /b HTTP/1.1\r\nHost: x\r\nContent-Length: 100\r\n\r\nab") for _ in range(N)]
    time.sleep(WAIT)
    print(f"{N} stalled bodies after {WAIT} s, curl rc", req(), flush=True)
    for s in socks: s.close()
    time.sleep(1)
    print("closed, curl rc", req(), flush=True)
finally:
    os.killpg(g.pid, signal.SIGKILL)
    subprocess.run(["pkill", "-9", "-f", prog])
