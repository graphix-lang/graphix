#!/usr/bin/env python3
# c-bind-11: ByRef::delete never removes its cell's store entry, so every
# deleted reference keeps the last value it mirrored for the rest of the run.
#
# ByRef::publish (graphix-compiler/src/node/bind.rs:1184-1198) and
# bottom_mirror (bind.rs:1210) write the referent's value to the store
# under the cell id (store_insert / store_insert_standing). ByRef::delete
# (bind.rs:1288-1292) drops the byref chain and the place registration but
# never calls store_remove, unlike a let's pattern (pattern.rs:1068) or a
# call's argument binds (callsite.rs:1993). Nothing else clears the store.
# A collection slot or a recursive activation that holds `&e` therefore
# leaves one entry per rebuild, pinning the value.
#
# The script runs three programs under `graphix --no-cache`:
#   leak     10 map slots, each `&str::concat("[i]", big)` (64 KB), deleted
#            and rebuilt every other 1 ms tick (the shape of netidx-admin's
#            `array::map(rows, |e| &row([..]))` tables);
#   control  the same without the `&`;
#   dangling `&x` minted in a slot, kept in an outer variable, the slot then
#            deleted while x goes on counting.
#
# command: timeout -s KILL 120 python3 design/review-2026-10-05/repro/c-bind-11.py <graphix>
#
# expected: leak's RSS stays flat like control's.
# observed (HEAD c722befe, debug build):
#   leak     rss MB by second: 1s:210 2s:359 3s:506 4s:653 5s:795 6s:943  (2806 lines)
#   control  rss MB by second: 1s:71 2s:71 3s:71 4s:71 5s:71 6s:71  (2838 lines)
#   (one output line per tick: both rebuilt the slots ~1400 times). With
#   16 KB slots for 20 s the leak run grows linearly, 101 -> 790 MB.
#   The recursion form (`let rec f = |n| select n { 0 => 0, k => { let s =
#   str::concat("[k]", big); let r = &s; str::len(*r) + f(k - 1) } }`, depth
#   toggled 10/0) grows the same, 192 -> 842 MB in 6 s, also with
#   --no-fusion, and under --no-fusion also without the `*r`; with
#   `let r = s` it stays at 69 MB.
#   dangling: (6, 105, 104) .. (11, 110, 104): after the slot's deletion at
#   c = 5, `*keep` reads the stale mirror 104 while x is 110, where a deleted
#   let's value is gone (absence reads as bottom).
import os, subprocess, sys, tempfile, threading, time

G = sys.argv[1] if len(sys.argv) > 1 else "graphix"

BIG = """let x64 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
let x4k = str::replace(#pat: "x", #rep: x64, x64);
let big = str::replace(#pat: "x", #rep: "xxxxxxxxxxxxxxxx", x4k);
let c = count(sys::time::timer(duration:1.ms, true));
let rows = array::init(select c % 2 { 0 => 10, _ => 0 }, |i| i);
"""
LEAK = BIG + """let cells = array::map(rows, |i| &str::concat("[i]", big));
sys::exit(sys::time::after_idle(duration:6.s, 0));
array::len(cells)
"""
CONTROL = BIG + """let cells = array::map(rows, |i| str::concat("[i]", big));
sys::exit(sys::time::after_idle(duration:6.s, 0));
array::len(cells)
"""
DANGLING = """let c = count(sys::time::timer(duration:20.ms, true));
let x = 100;
x <- c ~ x + 1;
let rows = array::init(select c < 5 { true => 1, false => 0 }, |i| i);
let refs = array::map(rows, |i| &x);
let keep = never();
keep <- (refs[0])$;
sys::exit(select c { 12 => 0, _ => never() });
(c, c ~ x, c ~ *keep)
"""


def descendants(pid):
    out = [pid]
    try:
        for t in os.listdir(f"/proc/{pid}/task"):
            with open(f"/proc/{pid}/task/{t}/children") as f:
                for c in f.read().split():
                    out.extend(descendants(int(c)))
    except FileNotFoundError:
        pass
    return out


def graphix_rss_mb(root):
    best = None
    for d in descendants(root):
        try:
            with open(f"/proc/{d}/comm") as f:
                if not f.read().startswith("graphix"):
                    continue
            with open(f"/proc/{d}/status") as f:
                for line in f:
                    if line.startswith("VmRSS:"):
                        kb = int(line.split()[1])
                        best = kb if best is None else max(best, kb)
        except (FileNotFoundError, ProcessLookupError):
            continue
    return None if best is None else best // 1024


def run(name, src, dir, sample):
    path = os.path.join(dir, f"{name}.gx")
    with open(path, "w") as f:
        f.write(src)
    p = subprocess.Popen([G, "--no-cache", path], stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT)
    lines = []
    th = threading.Thread(target=lambda: lines.extend(p.stdout), daemon=True)
    th.start()
    t0, series = time.time(), []
    while sample and p.poll() is None and time.time() - t0 < 30:
        time.sleep(1.0)
        mb = graphix_rss_mb(p.pid)
        if mb is not None:
            series.append(f"{round(time.time() - t0)}s:{mb}")
    try:
        p.wait(timeout=30)
    except subprocess.TimeoutExpired:
        p.kill()
        p.wait()
    th.join(timeout=5)
    if sample:
        print(f"{name:8} rss MB by second: {' '.join(series)}  ({len(lines)} lines)")
    else:
        print(f"{name}:")
        sys.stdout.write("".join(l.decode(errors="replace") for l in lines))


with tempfile.TemporaryDirectory() as d:
    run("leak", LEAK, d, True)
    run("control", CONTROL, d, True)
    run("dangling", DANGLING, d, False)
