"""f-helpers-01: an interrupt seen by graphix_stack_check is not a kernel abort:
the fused recursion's result becomes a fresh bottom instead of riding.

Needs the REPL (script mode exits on SIGINT), so this drives `graphix` on a pty,
answers reedline's cursor queries, and sends SIGINT to the graphix process
itself (not a wrapper) while a fused non-tail recursion is recomputing.

  timeout -s KILL 170 python3 -I f-helpers-01.py <graphix binary>

Expected (GXHandle::interrupt: "each value keeps its last result"; the node-walk,
run here with --no-fusion, does this):
  node-walk: ... after: 15000
  jit:       ... after: 15000
Observed at 7324207f:
  node-walk: r=15000 / r=15000 / after: 15000
  jit:       r=15000, then KERNEL RESULT f-region tag=Tag(64) pending=false
             (FRESH_BOTTOM, not PENDING) and no "after:" line: r is bottom.
The same session with an array::fold body (interrupt seen at a loop head)
prints "KERNEL RESULT ... PENDING" and "after: <old value>" under the JIT.
"""
import os, pty, re, select, signal, sys, time

S0 = '"' + "a" * 100 + '"'
REP = 'str::replace(#pat: "a", #rep: "aaaaaaaaaa", str::replace(#pat: "a", #rep: "aaaaaaaaaa", {}))'
SESSION = [
    'let rec f = |n: i64, s: string| -> i64 select n { 0 => 0, n => select str::contains(#part: "b[n]", s) { true => 2, false => 1 } + f(n - 1, s) }',
    "let s0 = " + S0,
    "let s1 = " + REP.format("s0"),
    "let s = " + REP.format("s1"),
    "let r = f(15000, s)",
    'let p = println("r=[r]")',
    ("sleep", 10),
    's <- sys::time::timer(duration:1.s, false) ~ str::concat(s, "b7")',
    ("sleep", 1.8),
    ("sigint",),
    ("sleep", 12),
    "let t2 = sys::time::timer(duration:200.ms, false)",
    'let q = println(t2 ~ "after: [r]")',
    ("sleep", 2),
]


def graphix_pids(root):
    kids = {}
    for d in os.listdir("/proc"):
        if d.isdigit():
            try:
                st = open("/proc/%s/stat" % d).read()
            except OSError:
                continue
            kids.setdefault(int(st.rsplit(")", 1)[1].split()[1]), []).append(int(d))
    todo, out = [root], []
    while todo:
        for k in kids.get(todo.pop(), []):
            todo.append(k)
            try:
                if open("/proc/%d/comm" % k).read().strip() == "graphix":
                    out.append(k)
            except OSError:
                pass
    return out


def run(exe, extra):
    env = dict(os.environ, GRAPHIX_DBG_INVOKE="1")
    pid, fd = pty.fork()
    if pid == 0:
        os.execve(exe, [exe, "--no-cache", *extra], env)
    out = bytearray()

    def pump(t):
        end = time.time() + t
        while (rem := end - time.time()) > 0:
            r, _, _ = select.select([fd], [], [], rem)
            if not r:
                return
            try:
                d = os.read(fd, 65536)
            except OSError:
                return
            if not d:
                return
            out.extend(d)
            for _ in range(d.count(b"\x1b[6n")):
                os.write(fd, b"\x1b[1;1R")

    pump(1.5)
    for step in SESSION:
        if isinstance(step, str):
            os.write(fd, (step + "\r").encode())
            pump(0.5)
        elif step[0] == "sleep":
            pump(step[1])
        else:
            for p in graphix_pids(pid):
                os.kill(p, signal.SIGINT)
    os.write(fd, b"\x04")
    pump(1.0)
    try:
        os.kill(pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    os.waitpid(pid, 0)
    txt = re.sub(r"\x1b\[[0-9;?]*[A-Za-z]|\x1b[78]", "", out.decode("utf-8", "replace"))
    for l in txt.replace("\r", "\n").split("\n"):
        for m in re.finditer(r"(r=\d+|after: \d+|KERNEL RESULT .*?(?:PENDING|pending=false))", l):
            yield m.group(1)


exe = sys.argv[1]
for name, extra in (("node-walk", ["--no-fusion"]), ("jit", [])):
    lines = list(run(exe, extra))
    print("%s: %s" % (name, " / ".join(l for l in lines if not l.startswith("KERNEL")) or "(nothing)"))
    ks = [l for l in lines if l.startswith("KERNEL")]
    if ks:
        print("  last kernel result: %s" % ks[-1])
