#!/usr/bin/env python3
# c-lib-02: batch_connect_targets grows for the whole run: every `<-`
# target of every instance built at run time stays in it.
#
# Connect::compile (graphix-compiler/src/node/mod.rs:1445) calls
# mark_connect_target (lib.rs:1346), which inserts the target into
# batch_connect_targets and connect_targets. Bind::delete
# (node/bind.rs:402-408) removes the ids from connect_targets and
# bind_to_lambda only. batch_connect_targets is emptied only by the
# embedder's next compile, compile_root or load_program (graphix-rt/src/
# gx.rs:678-679, 708-709, 912-913), so a script, which compiles once,
# keeps one entry per `<-` of every collection slot, activation or seq
# machine (pc and result) it ever builds, though each id dies with its
# instance.
#
# The REPL shows the set's size: each input line is a gx.compile, whose
# prune_static_resolution walks the whole set and then clears it. This
# types a program into the REPL that builds 50 slots per tick for 1500
# ticks and then idles (an instance of `f` per slot; 20 `<-` targets per
# instance that never fire), waits for the runtime to go idle, records
# VmRSS, then times three trivial input lines. Same again without the `<-`
# statements.
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/c-lib-02.py <graphix>
#
# expected: both programs end at about the same RSS, and the first
# trivial line compiles as fast as the next two.
#
# observed (HEAD c722befe, debug build, this script as written):
#   with `<-`:    RSS after the churn 256 MB; compiles 0.413 s, 0.004 s, 0.004 s
#   without `<-`: RSS after the churn 146 MB; compiles 0.006 s, 0.003 s, 0.003 s
# Typing `let z = 1` every second during the churn (each line clears the
# set) leaves the `<-` program at ~165 MB. A seq-per-instance churn
# (10 seqs per instance, no `<-` written) gives a first compile of
# 0.102 s, then 0.002 s. Run as a script (`graphix --no-cache prog.gx`,
# fusion on or off), the `<-` program goes from ~100 to ~310 MB in 60 s
# and never gives the memory back. Control: the same 20 never-firing
# `<-` per instance aimed at ONE outer variable (`o <- never<i64>()`)
# stays flat at ~109 MB over 60 s, so the growth is per distinct target
# id, and batch_connect_targets is the only per-id record Bind::delete
# leaves behind.
import os, pty, select, signal, sys, time, fcntl, termios, struct

G = sys.argv[1] if len(sys.argv) > 1 else "graphix"
TICKS = 1500


def program(connects):
    lets = "; ".join(
        f"let s{i} = x" + (f"; s{i} <- never<i64>()" if connects else "")
        for i in range(20)
    )
    total = " + ".join(f"s{i}" for i in range(20))
    return (
        f"let f = |x: i64| {{ {lets}; {total} }}; "
        "let n = 0; let ticks = 0; "
        "let clock = sys::time::timer(duration:10.ms, true); "
        "ticks <- clock ~ ticks + 1; "
        f"n <- clock ~ select ticks < {TICKS} {{ true => (n + 50) % 400, false => 0 }}; "
        "let churn = array::len(array::init(n, |i| f(i)))"
    )


def run(connects):
    pid, fd = pty.fork()
    if pid == 0:
        os.environ["TERM"] = "xterm"
        os.execvp(G, [G, "--no-cache", "--no-fusion"])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 50, 300, 0, 0))

    def pump(timeout):
        got = bytearray()
        end = time.time() + timeout
        while time.time() < end:
            r, _, _ = select.select([fd], [], [], 0.002)
            if r:
                data = os.read(fd, 65536)
                got += data
                # the line editor asks where the cursor is before each prompt
                for _ in range(data.count(b"\x1b[6n")):
                    os.write(fd, b"\x1b[1;1R")
        return bytes(got)

    def graphix_pid():
        todo = [pid]
        while todo:
            q = todo.pop()
            try:
                if open(f"/proc/{q}/comm").read().strip() == "graphix":
                    return q
                todo += [int(c) for c in open(f"/proc/{q}/task/{q}/children").read().split()]
            except OSError:
                pass

    def cpu(g):
        st = open(f"/proc/{g}/stat").read().rsplit(")", 1)[1].split()
        return (int(st[11]) + int(st[12])) / os.sysconf("SC_CLK_TCK")

    def rss_mb(g):
        for l in open(f"/proc/{g}/status"):
            if l.startswith("VmRSS"):
                return int(l.split()[1]) / 1024

    def compile_line(line):
        # Enter repaints the line (ending in CRLF); the next read opens with
        # a cursor query once gx.compile has returned
        os.write(fd, line.encode() + b"\r")
        t0 = time.time()
        seen = bytearray()
        while time.time() - t0 < 60:
            seen += pump(0.002)
            i = seen.find(b"\r\n")
            if i >= 0 and seen.find(b"\x1b[6n", i) >= 0:
                return time.time() - t0
        return float("nan")

    try:
        pump(3)
        os.write(fd, program(connects).encode() + b"\r")
        pump(3)
        g = graphix_pid()
        last, idle = cpu(g), 0
        for _ in range(150):
            pump(1)
            now = cpu(g)
            idle = idle + 1 if now - last < 0.15 else 0
            last = now
            if idle >= 2:
                break
        rss = rss_mb(g)
        lats = [compile_line(f"let z{i} = {i}") for i in range(3)]
    finally:
        for q in {pid, graphix_pid()} - {None}:
            try:
                os.kill(q, signal.SIGKILL)
            except ProcessLookupError:
                pass
        os.waitpid(pid, 0)
    label = "with `<-`:   " if connects else "without `<-`:"
    print(
        f"{label} RSS after the churn {rss:.0f} MB; compiles "
        + ", ".join(f"{l:.3f} s" for l in lats)
    )


run(True)
run(False)
