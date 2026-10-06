#!/usr/bin/env python3
# x-node-contract-05: after_idle/timer re-arm drops the armed id: refs and
# store entries leak, stray updates.
# AfterIdle::update (stdlib/graphix-package-sys/src/time.rs:109-121) arms a
# new timer id, or takes the Err arm, without `release` of the armed one;
# Timer's schedule! from the (Some(s), Some(r), _) arm (time.rs:337-345)
# and its error!() (time.rs:293-302) do the same. The old id keeps its
# by_ref entry for good; when its timer fires, graphix-rt's push_var_event
# (graphix-rt/src/gx.rs:455) stores the dead id and schedules the
# statement (for a script, the whole program) for nothing. A released id
# is no better off once its timer fires late: push_var_event stores every
# fire and nothing removes it (case E: sleep() releases, the store grows).
#
# command: GRAPHIX=/path/to/graphix python3 design/review-2026-10-05/repro/x-node-contract-05.py
#          (about a minute; each run is under timeout -s KILL)
#
# expected (stdlib/graphix-tests/src/lib_tests/leaks.rs pins "a periodic
# timer's fires are private to it: each is gone from the store once the
# timer has read it"):
#   A, B. the ids left referenced at exit do not depend on how many times
#         the timer was re-armed (diff 0)
#   C. no update of the program between the last re-arm and the idle fire
#   D, E. peak RSS does not grow with the number of re-arms / arm sleeps
# observed (HEAD c722befe, debug build):
#   A after_idle re-armed 10 vs 30 times: never-unreferenced ids 1339 vs 1359 (diff 20, expected 0)
#   B timer re-armed 10 vs 30 times: never-unreferenced ids 1341 vs 1361 (diff 20, expected 0)
#   C updates of the program after the last re-arm and before the idle fire: 10 (expected 0; 10 ids leaked)
#   D peak RSS, 800000 re-arms 224 MB vs armed once 62 MB
#   E peak RSS, 400000 sleeps of an arm with an armed timer 90 MB vs no timer armed 61 MB
#   (D: ~200 bytes per re-arm, a by_ref entry and a store entry; E: ~75
#   bytes per abandoned armed timer, the store entry alone.)
import atexit, os, re, shutil, subprocess, sys, tempfile

G = os.environ.get("GRAPHIX", "graphix")
D = tempfile.mkdtemp()
atexit.register(shutil.rmtree, D, True)
ENV = dict(os.environ, XDG_CACHE_HOME=os.path.join(D, "cache"))

def run(name, src, extra_env=None, secs=60):
    path = os.path.join(D, name + ".gx")
    open(path, "w").write(src)
    err = open(os.path.join(D, name + ".err"), "w")
    env = dict(ENV, **(extra_env or {}))
    p = subprocess.Popen(["timeout", "-s", "KILL", str(secs), G, "--no-cache", path],
                         env=env, stdout=subprocess.DEVNULL, stderr=err)
    _, status, ru = os.wait4(p.pid, 0)
    err.close()
    return open(os.path.join(D, name + ".err")).read(), ru.ru_maxrss / 1024

def unreleased(trace):
    refs = set(re.findall(r"^REF_VAR (BindId\(\d+\))", trace, re.M))
    unrefs = set(re.findall(r"^UNREF_VAR (BindId\(\d+\))", trace, re.M))
    return len(refs - unrefs)

dbg = {"GRAPHIX_DBG_VARS": "1"}
idle = lambda k: f"""let t = sys::time::timer(duration:5.ms, true);
let n = 0;
n <- t ~ (n + 1);
let idle = sys::time::after_idle(duration:40.ms, n);
sys::exit(select n {{ {k} => 0, _ => never() }});
idle
"""
a10, _ = run("a10", idle(10), dbg)
a30, _ = run("a30", idle(30), dbg)
print(f"A after_idle re-armed 10 vs 30 times: never-unreferenced ids "
      f"{unreleased(a10)} vs {unreleased(a30)} (diff {unreleased(a30) - unreleased(a10)}, expected 0)")

timer = lambda k: f"""let k = sys::time::timer(duration:5.ms, true);
let d = duration:500.ms;
d <- k ~ duration:500.ms;
let t = sys::time::timer(d, false);
let n = 0;
n <- k ~ (n + 1);
sys::exit(select n {{ {k} => 0, _ => never() }});
t
"""
b10, _ = run("b10", timer(10), dbg)
b30, _ = run("b30", timer(30), dbg)
print(f"B timer re-armed 10 vs 30 times: never-unreferenced ids "
      f"{unreleased(b10)} vs {unreleased(b30)} (diff {unreleased(b30) - unreleased(b10)}, expected 0)")

stray, _ = run("stray", """let t = sys::time::timer(duration:5.ms, 10);
let n = 0;
n <- t ~ (n + 1);
let idle = sys::time::after_idle(duration:200.ms, n);
sys::exit(select idle { 10 => 0, _ => never() });
idle
""", {"GXDBG_CS": "1"})
site = "sys::time::after_idle(duration:200.ms, n)"
disp = re.findall(r"^CS spec=" + re.escape(site) + r" bound=\S+ kind=\S+ argfired=(\S+)", stray, re.M)
last = max(i for i, f in enumerate(disp) if f == "true")
res = re.findall(r"^CS-RES spec=" + re.escape(site) + r" res=(.*)$", stray, re.M)
fire = next(i for i, r in enumerate(res) if i > last and r == "Some(Tag(0))")
print(f"C updates of the program after the last re-arm and before the idle fire: "
      f"{fire - last - 1} (expected 0; 10 ids leaked)")

N = 800000
body = lambda arm: f"""let x = 0;
x <- x + 1;
{arm}
sys::exit(select x {{ {N} => 0, _ => never() }});
v
"""
_, leak = run("leak", body("let v = sys::time::after_idle(duration:1.ms, x);"), secs=170)
_, ctl = run("ctl", body("let v = sys::time::after_idle(duration:1.ms, 0);"), secs=170)
print(f"D peak RSS, {N} re-arms {leak:.0f} MB vs armed once {ctl:.0f} MB")
_, slp = run("slp", body("let v = select x % 2 { 0 => sys::time::after_idle(duration:1.ms, x), _ => never() };"), secs=170)
_, noarm = run("noarm", body("let v = select x % 2 { 0 => sys::time::after_idle(-1, x), _ => never() };"), secs=170)
print(f"E peak RSS, {N // 2} sleeps of an arm with an armed timer {slp:.0f} MB vs no timer armed {noarm:.0f} MB")
