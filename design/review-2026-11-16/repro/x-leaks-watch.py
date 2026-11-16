#!/usr/bin/env python3
# watch.py SECS [--every S] -- ARGS...: run the graphix wrapper with ARGS under
# timeout -s KILL SECS, sample the real graphix process (RSS, threads, fds)
# every S seconds, and print a table plus the program's last output lines.
import os, subprocess, sys, time, threading

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
            if k in ("VmRSS", "Threads", "VmHWM", "RssAnon"):
                d[k] = v.strip()
        d["fds"] = len(os.listdir(f"/proc/{pid}/fd"))
        kids = 0; zomb = 0
        for q in os.listdir("/proc"):
            if not q.isdigit(): continue
            try:
                st = open(f"/proc/{q}/stat").read()
                parts = st.rsplit(")", 1)[1].split()
                if int(parts[1]) == pid:
                    kids += 1
                    if parts[0] == "Z": zomb += 1
            except Exception: pass
        d["kids"] = f"{kids}/z{zomb}"
    except Exception:
        return None
    return d

def main():
    args = sys.argv[1:]
    secs = int(args.pop(0))
    every = 2.0
    if args and args[0] == "--every":
        args.pop(0); every = float(args.pop(0))
    if args and args[0] == "--":
        args.pop(0)
    marker = [a for a in args if a.endswith(".gx")][-1]
    env = dict(os.environ)
    p = subprocess.Popen(["timeout", "-s", "KILL", str(secs), WRAP] + args,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env)
    lines = []; nlines = [0]; errs = []
    def rd():
        for l in p.stdout:
            nlines[0] += 1
            lines.append(l.decode(errors="replace").rstrip())
            if len(lines) > 6: lines.pop(0)
    def rde():
        for l in p.stderr:
            errs.append(l.decode(errors="replace").rstrip())
            if len(errs) > 20: errs.pop(0)
    t1 = threading.Thread(target=rd, daemon=True); t1.start()
    t2 = threading.Thread(target=rde, daemon=True); t2.start()
    t0 = time.time(); pid = None
    while p.poll() is None:
        if pid is None:
            pid = find_pid(marker)
        if pid is not None:
            s = stat(pid)
            if s:
                print(f"{time.time()-t0:6.1f}s rss={s.get('RssAnon','?'):>12} hwm={s.get('VmHWM','?'):>12} thr={s.get('Threads')} fds={s.get('fds')} kids={s.get('kids')} out={nlines[0]} last={lines[-1][:80] if lines else ''}", flush=True)
        time.sleep(every)
    t1.join(1); t2.join(1)
    print("rc", p.returncode, "lines", nlines[0])
    for l in lines: print("OUT", l[:300])
    for l in errs[-10:]: print("ERR", l[:300])

main()
