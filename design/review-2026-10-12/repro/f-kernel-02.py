# f-kernel-02: kernel activation blocks freed outside a cycle are never
# uncharged: spurious activation-budget abort.
#
# command: timeout -s KILL 40 python3 -I f-kernel-02.py /path/to/graphix out.txt [--no-fusion]
#          (BUDGET=<n> in the environment sets GRAPHIX_ACTIVATION_BUDGET, default 300)
# expected: 8 lines of 1325 in out.txt, rc 0, under any budget >= ~51
#           (each f(50) holds 50 activations at once, and is deleted before the next).
# observed (debug build, HEAD 7324207f): fused, BUDGET=300 -> 6 outputs then the
#   runtime aborts (rc 1); BUDGET=240 -> 4; BUDGET=160 -> 3; i.e. 50 activations leak
#   per deleted run. --no-fusion BUDGET=300 -> 8 outputs, rc 0; fused BUDGET=100000 -> 8.
#
# Drives the REPL: defines a non-tail recursive f whose kernel makes ~50
# per-activation blocks per run of f(50), shows f(k), Ctrl-C (which drops
# the CompExp -> ToGX::Delete, handled outside `with_control`), and repeats.
# FusedKernel::drop -> free_self_block_tree -> stack::record_self_blocks(-1)
# finds no current Control, so the count only grows.
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
