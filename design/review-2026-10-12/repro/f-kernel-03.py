# f-kernel-03: a fused kernel's activation blocks freed outside a cycle are
# never uncharged from the runtime's activation count.
#
# Drives the REPL: defines a non-tail recursive f whose kernel makes ~50
# per-activation blocks per run of f(50), shows f(k), Ctrl-C (which drops
# the CompExp -> ToGX::Delete, handled outside `with_control`), and repeats.
# FusedKernel::drop -> free_self_block_tree -> stack::record_self_blocks(-1)
# finds no current Control, so the count only grows: with
# GRAPHIX_ACTIVATION_BUDGET=300 the 7th f(k) aborts the runtime (and the
# shell then panics "called after complete", see the shell finding).
# `--no-fusion` (GXLambda::delete uncharges through ctx.control) runs all 8.
#
# usage: timeout -s KILL 40 python3 -I f-kernel-03.py /path/to/graphix out.txt [--no-fusion]
import subprocess, sys, time, signal, os
g = sys.argv[1]
outf = sys.argv[2]
extra = sys.argv[3:]
env = dict(os.environ, GRAPHIX_ACTIVATION_BUDGET=os.environ.get("BUDGET","300"), RUST_LOG="error", RUST_BACKTRACE="1")
with open(outf, "w") as f:
    p = subprocess.Popen([g, "--no-cache"] + extra, stdin=subprocess.PIPE, stdout=f, stderr=subprocess.STDOUT, env=env, start_new_session=True)
def send(l):
    p.stdin.write((l + "\n").encode()); p.stdin.flush()
def intr():
    os.killpg(p.pid, signal.SIGINT)
try:
    send("let g = |x: i64| x + 1"); time.sleep(0.4)
    send("let rec f = |n: i64| -> i64 select n < 1 { true => 0, false => f(n - 1) + g(n) }"); time.sleep(0.4)
    send("let k = 50"); time.sleep(0.4)
    for i in range(8):
        if p.poll() is not None: break
        send("f(k)"); time.sleep(0.6)
        if p.poll() is not None: break
        intr(); time.sleep(0.4)
    try:
        p.stdin.close()
    except Exception:
        pass
    for _ in range(20):
        if p.poll() is not None: break
        time.sleep(0.1)
finally:
    if p.poll() is None:
        os.killpg(p.pid, signal.SIGKILL)
        p.wait()
print("rc", p.returncode)
