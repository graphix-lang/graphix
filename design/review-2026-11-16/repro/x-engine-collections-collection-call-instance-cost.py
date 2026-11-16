#!/usr/bin/env python3
# A collection call in a function body costs about 17-25 KB of resident memory (and
# ~100 us in the debug build) for every run-time instance of that body, whether or
# not the call is ever reached.
#
# command (GRAPHIX = path of a graphix binary, default "graphix"):
#   GRAPHIX=/home/eric/tmp/review-2026-11-16/bin/graphix timeout -s KILL 170 \
#       python3 x-engine-collections-collection-call-instance-cost.py
#
# It runs each program at two sizes, polls the child's VmHWM, and prints the slope.
#
# expected: an arm that never runs holds (nearly) nothing: the `unreached` rows
#   close to their `plain` rows.
# observed (debug build at e1be2fbf2, KB per recursion level / per slot):
#   rec_plain                 ~11-13
#   rec_unreached_userfn      ~14
#   rec_unreached_map         ~29-33
#   rec_unreached_2folds      ~47
#   slot_stateful             ~8      (fusion on; `count` keeps the slots in the node-walk)
#   slot_stateful_inner_map   ~31
#   slot_unreached_inner_map  ~34
# So a 100k-row map whose callback holds one nested collection call keeps about 3 GB,
# and a recursion 200k levels deep through array::fold/array::map callbacks passes
# 6 GB under --no-fusion (the fused twin answers at once).
import glob, os, subprocess, sys, tempfile, time

B = os.environ.get("GRAPHIX", "graphix")
TMP = tempfile.mkdtemp()

def peak(src, args, tag):
    path = os.path.join(TMP, tag + ".gx")
    open(path, "w").write(src)
    p = subprocess.Popen(["timeout", "-s", "KILL", "100", B, "--no-cache"] + args + [path],
                         stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    hwm = 0
    while p.poll() is None:
        for st in glob.glob("/proc/[0-9]*/cmdline"):
            try:
                c = open(st, "rb").read()
                if path.encode() in c and b"systemd-run" not in c and b"timeout" not in c.split(b"\0")[0]:
                    for line in open(st.replace("cmdline", "status")):
                        if line.startswith("VmHWM"):
                            hwm = max(hwm, int(line.split()[1]))
            except Exception:
                pass
        time.sleep(0.1)
    return hwm, p.stdout.read().strip().split("\n")[-1][:40]

REC = "let rec f = |n: i64| -> i64 select n { 0 => %s, m => m + f(m - 1) };\nlet r = f(N);\n"
SLOT = "let r = array::len(array::map(array::init(N, |i| i), |x| %s));\n"
CASES = [
    ("rec_plain", ["--no-fusion"], REC % "0", (4000, 8000)),
    ("rec_unreached_userfn", ["--no-fusion"], "let g = |x: i64| x + 1;\n" + REC % "g(1)", (4000, 8000)),
    ("rec_unreached_map", ["--no-fusion"], REC % "array::len(array::map([1], |x| x + 1))", (4000, 8000)),
    ("rec_unreached_2folds", ["--no-fusion"],
     REC % "array::fold([1], 0, |a, x| a + x) + array::fold([2], 0, |a, x| a + x)", (4000, 8000)),
    ("slot_stateful", [], SLOT % "count(x) + 1", (10000, 20000)),
    ("slot_stateful_inner_map", [], SLOT % "count(x) + array::len(array::map([x], |y| y + 1))", (10000, 20000)),
    ("slot_unreached_inner_map", [],
     SLOT % "count(x) + select x { -1 => array::len(array::map([x], |y| y + 1)), v => v }", (10000, 20000)),
]
for name, args, body, (n1, n2) in CASES:
    m = []
    for n in (n1, n2):
        src = body.replace("N", str(n)) + "sys::exit(r ~ 0);\nr\n"
        m.append(peak(src, args, f"{name}{n}"))
    print(f"{name}: {(m[1][0] - m[0][0]) / (n2 - n1):.1f} KB each  [{m[0][1]} | {m[1][1]}]", flush=True)
