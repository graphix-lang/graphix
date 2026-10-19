# Title: http::serve's accept loop spins at full CPU once the process is out of file
# descriptors (EMFILE): serve_loop `continue`s straight back into accept.
# Command: timeout -s KILL 60 python3 -u -I design/review-2026-10-19/repro/http-sqlite-db1-accept-spin.py 64
#   (then again with 1024 as the baseline)
# Expected: the server idles (about 0 CPU ticks over 3 s) while connections wait in the backlog.
# Observed: with RLIMIT_NOFILE 64 and 150 client connections: "graphix cpu ticks over 3s: 348"
#   (over one core busy); with 1024: "graphix cpu ticks over 3s: 0".
import subprocess, socket, time, os, resource, sys
env = dict(os.environ, RUST_LOG="error")
lim_n = int(sys.argv[1])
def lim():
    resource.setrlimit(resource.RLIMIT_NOFILE, (lim_n, lim_n))
p = subprocess.Popen(["timeout", "-s", "KILL", "20", "/home/eric/tmp/review-2026-10-19/bin/graphix", "--no-cache", "/home/eric/proj/graphix/design/review-2026-10-19/repro/http-sqlite-db1-accept-spin.gx"],
                     stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, env=env, preexec_fn=lim, stdin=subprocess.DEVNULL)
time.sleep(3)
socks = []
for i in range(150):
    try:
        socks.append(socket.create_connection(("127.0.0.1", 47391), timeout=2))
    except Exception as e:
        print("connect fail", i, e); break
print("opened", len(socks))
time.sleep(1)
t0 = open(f"/proc/{p.pid}/stat").read()
out = subprocess.run(["pgrep", "-f", "^/home/eric/tmp/review-2026-10-19/bin/real/graphix --no-cache /home/eric/proj/graphix/design/review-2026-10-19/repro/http-sqlite-db1-accept-spin.gx"], capture_output=True, text=True).stdout.split()
pid = out[0] if out else None
def cpu(pid):
    f = open(f"/proc/{pid}/stat").read().split(")")[1].split()
    return int(f[11]) + int(f[12])
print("fds", len(os.listdir(f"/proc/{pid}/fd")), [l for l in open(f"/proc/{pid}/limits") if "open files" in l])
c0 = cpu(pid); time.sleep(3); c1 = cpu(pid)
print("graphix cpu ticks over 3s:", c1 - c0)
for s in socks: s.close()
_, err = p.communicate()
lines = err.decode(errors="replace").splitlines()
print("stderr lines", len(lines))
print("\n".join(lines[:3]))
