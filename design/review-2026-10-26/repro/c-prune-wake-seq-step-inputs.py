# A seq step's entry costs O(every input any step of the machine reads):
# TrackedFires::open rebuilds `keys` (an IntSet of all tracked inputs) after
# each deselect's refresh, and TrackedFires::deliver scans every pending bit.
# Usage: python3 c-prune-wake-seq-step-inputs.py N K > p.gx
#        timeout -s KILL 170 graphix --no-cache p.gx    (prints (result, run time))
# N steps run in one cycle; step i reads K outer lets of its own besides v{i-1}.
# Expected: run time linear in N, and in the inputs per step K.
# Observed (debug build, 2026-10-26, noisy shared machine):
#   N=800  K=0: 10.0 ms   N=1600 K=0:  32.0 ms
#   N=800  K=3: 45.0 ms   N=1600 K=3: 153.6 ms
#   (N=400/800/1600/3200 at K=0 with x-perf-runtime-03.py: 3.1/8.9/47/125 ms)
import sys
n = int(sys.argv[1]); k = int(sys.argv[2])
ws = "\n".join(f"let w{i}_{j} = go + {i};" for i in range(n) for j in range(k))
def step(i):
    extra = "".join(f" + w{i}_{j}" for j in range(k))
    return (f"let v{i} = v{i-1} + 1{extra}" if i else f"let v0 = go{extra}")
steps = ";\n  ".join(step(i) for i in range(n))
print(f"""let go = 1;
{ws}
let t0 = sys::time::now(go);
let r = seq go {{
  {steps};
  v{n-1}
}};
let t1 = sys::time::now(r);
sys::exit(sys::time::after_idle(duration:50.ms, t1 ~ 0));
(r, sys::time::diff(t1, t0))""")
