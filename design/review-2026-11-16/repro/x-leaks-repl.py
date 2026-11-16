#!/usr/bin/env python3
# repl.py SECS TEMPLATE_FILE BATCHES PER_BATCH [extra graphix args]
# Feeds the REPL lines from TEMPLATE_FILE (python format with {i}) in batches,
# syncing on a marker after each batch, sampling the real process's RSS.
import os, subprocess, sys, time, threading, queue

REAL = "/home/eric/tmp/review-2026-11-16/bin/real/graphix"
WRAP = "/home/eric/tmp/review-2026-11-16/bin/graphix"

def find_pid(marker):
    for p in os.listdir("/proc"):
        if not p.isdigit():
            continue
        try:
            if os.readlink(f"/proc/{p}/exe") != REAL:
                continue
            cmd = open(f"/proc/{p}/cmdline", "rb").read().split(b"\0")
            if any(marker.encode() in c for c in cmd):
                return int(p)
        except Exception:
            pass
    return None

def stat(pid):
    d = {}
    try:
        for l in open(f"/proc/{pid}/status"):
            k, _, v = l.partition(":")
            if k in ("VmRSS", "Threads", "RssAnon"):
                d[k] = v.strip()
        d["fds"] = len(os.listdir(f"/proc/{pid}/fd"))
    except Exception:
        return None
    return d

secs, tfile, batches, per = int(sys.argv[1]), sys.argv[2], int(sys.argv[3]), int(sys.argv[4])
extra = sys.argv[5:]
tmpl = open(tfile).read().strip("\n").split("\n")
marker = f"--log-dir=/home/eric/tmp/review-2026-11-16/work/x-leaks/logs/{os.path.basename(tfile)}"
os.makedirs(marker.split("=",1)[1], exist_ok=True)
p = subprocess.Popen(["timeout", "-s", "KILL", str(secs), WRAP, "--no-cache", "-n", marker] + extra,
                     stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
q = queue.Queue(); tail = []
def rd():
    for l in p.stdout:
        s = l.decode(errors="replace").rstrip()
        tail.append(s)
        if len(tail) > 30: tail.pop(0)
        q.put(s)
    q.put(None)
threading.Thread(target=rd, daemon=True).start()
t0 = time.time(); pid = None; i = 0
def sync(tag):
    p.stdin.write(f'"SYNC{tag}"\n'.encode()); p.stdin.flush()
    while True:
        s = q.get()
        if s is None: return False
        if f'SYNC{tag}' in s and not s.startswith("-:"): return True
for b in range(batches):
    for k in range(per):
        for line in tmpl:
            if line.startswith("ONCE:"):
                if i == 0: p.stdin.write((line[5:] + "\n").encode())
                continue
            p.stdin.write((line.format(i=i) + "\n").encode())
        i += 1
    p.stdin.flush()
    if not sync(b):
        print("process ended"); break
    if pid is None: pid = find_pid(marker)
    s = stat(pid) if pid else None
    print(f"{time.time()-t0:6.1f}s batch={b} stmts={i} rss={s and s.get('RssAnon')} thr={s and s.get('Threads')} fds={s and s.get('fds')}", flush=True)
try:
    p.stdin.close()
except Exception: pass
p.wait()
print("rc", p.returncode)
for l in tail[-12:]: print("OUT", l[:300])
