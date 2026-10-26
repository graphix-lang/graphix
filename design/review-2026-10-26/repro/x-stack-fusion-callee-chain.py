# x-stack: fusing a static call chain over a leaf that does not fuse is quadratic in
# the chain's length: every f_i's region runs discover_lambda_calls, which walks the
# whole transitive callee closure (f_{i-1} .. f_1) again although the emission already
# holds those kernels. GRAPHIX_PROFILE=1 puts the time in the Callees phase
# (1501 calls, 6.0 s self at n = 1500) and JitBuild.
# run: python3 x-stack-fusion-callee-chain.py N > /tmp/cc.gx;
#      timeout -s KILL 170 graphix --no-cache /tmp/cc.gx
# expected: roughly linear in N, as the same chain with a fusable leaf
#   (`|x: i64| -> i64 x`: 0.27 s at 750, 0.39 s at 1500).
# observed (debug build): 3.1 s at N = 750, 13.4 s at 1500, 59.5 s at 3000; prints N + 1.
import sys
n = int(sys.argv[1]) if len(sys.argv) > 1 else 1500
print("let f0 = |x: i64| -> i64 once(x);")
for i in range(1, n + 1):
    print(f"let f{i} = |x: i64| -> i64 f{i-1}(x) + 1;")
print("sys::exit(sys::time::after_idle(duration:50.ms, 0));")
print(f"f{n}(1)")
