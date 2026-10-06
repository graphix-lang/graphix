#!/usr/bin/env python3
# x-panics-14: http::serve reads every request body whole, with no size
# limit, before the handler sees the request.
#
# handle_http_request (stdlib/graphix-package-http/src/lib.rs:496) does
# body.collect().await on the hyper body: no Content-Length check, no
# Limited wrapper, and serve has no option for one. The bytes are then
# copied into an ArcStr while body_bytes stays alive until the reply, so a
# request costs about twice its body. The handler cannot refuse it: it runs
# only after the whole body has arrived. A body that never arrives (the
# client leaves mid-upload) reaches the handler as body: null.
#
# This writes the server below (its handler prints each request's body
# length, -1 for null), runs CMD... --no-cache <server.gx>, then:
#   1. declares a 100 GiB body, sends 1 MiB, waits 3 s for a reply, closes;
#   2. POSTs a --body-mib body (default 32) and reports the reply and the
#      server's peak RSS (VmHWM), or how the server ended.
#
# commands (HEAD c722befe, debug build). probe-run CAP REAL ARGS runs REAL
# under systemd-run --user --scope -p MemoryMax=CAP -p MemorySwapMax=0 and
# reports an OOM kill:
#   timeout -s KILL 170 python3 design/review-2026-10-05/repro/x-panics-14.py graphix
#   timeout -s KILL 170 python3 design/review-2026-10-05/repro/x-panics-14.py --body-mib 12 probe-run 80M graphix
#   timeout -s KILL 170 python3 design/review-2026-10-05/repro/x-panics-14.py --body-mib 36 probe-run 80M graphix
#
# expected: a body over a limit is answered 413 without being buffered; a
# declared 100 GiB Content-Length is refused at once; the abandoned upload
# never reaches the handler.
#
# observed:
#   graphix (in this review, the 6G-capped wrapper):
#     1: declared 100 GiB, sent 1 MiB -> no reply after 3 s (no 413); closed
#     2: POST 33554432 bytes -> HTTP/1.1 200 OK, reply 33554432; VmHWM 61 -> 124 MiB
#     handler: POST body length 16 / -1 (the abandoned upload) / 33554432
#   probe-run 80M, --body-mib 12:
#     2: POST 12582912 bytes -> HTTP/1.1 200 OK, reply 12582912; VmHWM 60 -> 82 MiB
#   probe-run 80M, --body-mib 36:
#     2: POST 37748736 bytes -> no reply; server exited 251
#     [probe] OOM-KILLED: this run exceeded its 80M memory cap
#   Under a 1G cap the scope's memory.peak went 31 -> 95 MiB for the 32 MiB
#   body. Each of the 768 default connections can hold one such body, and on
#   an uncapped host one large enough POST takes the process down.
import os, re, signal, socket, subprocess, sys, tempfile

SERVER = """\
let handler = |req: http::Request| {
    let n = select req.body { null as _ => -1, s => str::len(s) };
    println("handler: [req.method] body length [n]");
    { body: "[n]", headers: [], status: u16:200, url: "" }
};
let server = http::serve(#addr: "127.0.0.1:0", #handler: handler)$;
http::server_addr(server)
"""
MIB = 1024 * 1024
CHUNK = b"a" * (64 * 1024)


def server_pid(prog):
    for p in os.listdir("/proc"):
        if p.isdigit():
            try:
                with open(f"/proc/{p}/cmdline", "rb") as f:
                    cmd = f.read().split(b"\0")
            except OSError:
                continue
            if os.path.basename(cmd[0]) == b"graphix" and prog.encode() in cmd:
                return int(p)


def hwm(pid):
    with open(f"/proc/{pid}/status") as f:
        for line in f:
            if line.startswith("VmHWM:"):
                return int(line.split()[1]) // 1024


def send(s, n):
    while n > 0:
        k = min(n, len(CHUNK))
        s.sendall(CHUNK[:k])
        n -= k


def post(addr, n):
    s = socket.create_connection(addr)
    s.sendall(
        f"POST / HTTP/1.1\r\nHost: x\r\nContent-Length: {n}\r\nConnection: close\r\n\r\n".encode()
    )
    data = b""
    try:
        send(s, n)
        while b := s.recv(65536):
            data += b
    except OSError:
        pass
    s.close()
    head, _, body = data.partition(b"\r\n\r\n")
    return head.split(b"\r\n")[0].decode(), body.decode()


def main():
    args = sys.argv[1:]
    body_mib = 32
    if args[:1] == ["--body-mib"]:
        body_mib, args = int(args[1]), args[2:]
    tmp = tempfile.mkdtemp()
    prog = os.path.join(tmp, "server.gx")
    with open(prog, "w") as f:
        f.write(SERVER)
    env = dict(os.environ, XDG_CACHE_HOME=os.path.join(tmp, "cache"))
    # timeout(1) leads its own process group: killpg ends the whole chain.
    srv = subprocess.Popen(
        ["timeout", "-s", "KILL", "150"] + args + ["--no-cache", prog],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        env=env,
    )
    try:
        m = re.search(rb"127\.0\.0\.1:(\d+)", srv.stdout.readline())
        addr = ("127.0.0.1", int(m.group(1)))
        pid = server_pid(prog)
        post(addr, 16)

        s = socket.create_connection(addr)
        s.sendall(f"POST / HTTP/1.1\r\nHost: x\r\nContent-Length: {100 << 30}\r\n\r\n".encode())
        send(s, MIB)
        s.settimeout(3.0)
        try:
            print("1: declared 100 GiB, sent 1 MiB -> reply:", s.recv(4096)[:60])
        except socket.timeout:
            print("1: declared 100 GiB, sent 1 MiB -> no reply after 3 s (no 413); closed")
        s.close()

        n = body_mib * MIB
        before = hwm(pid)
        status, body = post(addr, n)
        if status:
            print(f"2: POST {n} bytes -> {status}, reply {body}; VmHWM {before} -> {hwm(pid)} MiB")
        else:
            print(f"2: POST {n} bytes -> no reply")
            try:
                srv.wait(timeout=10)
                print(f"   server exited {srv.returncode}")
            except subprocess.TimeoutExpired:
                print("   server still running")
    finally:
        try:
            os.killpg(srv.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        out = srv.communicate()[0].decode()
        print("server stdout after the address:")
        for line in out.splitlines():
            print("  ", line)


main()
