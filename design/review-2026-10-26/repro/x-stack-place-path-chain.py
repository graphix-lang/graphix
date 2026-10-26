# x-stack: a chain of place references `&mut (*r)[0]`, each one path step longer,
# overflows the stack when the last one is written through: node::place::write_path
# recurses once per path step with no stack guard (read_path is a loop).
# run: python3 x-stack-place-path-chain.py 10000 > /tmp/pc.gx;
#      timeout -s KILL 60 graphix --no-cache /tmp/pc.gx
# expected: prints 1 (array::len(a)) and exits 0, as it does at n = 2000 and 5000.
# observed (n = 10000): prints 1, then "thread 'tokio-rt-worker' has overflowed its
#   stack", abort (exit 134); gdb: ~8500 frames of graphix_compiler::node::place::write_path
#   under coretraits::with_hooks <- GX::deliver. At n = 20000 the run is instead
#   OOM-killed past 6 GB before the write: each reference holds its own copy of the
#   whole path, so the chain's paths take O(n^2) memory.
import sys
n = int(sys.argv[1]) if len(sys.argv) > 1 else 10000
print("type N = Array<N>;")
print("let rec b = |n: i64, acc: N| -> N select n { 0 => acc, n => b(n - 1, [acc]) };")
print(f"let a = b({n}, []);")
print("let r0 = &mut a;")
for i in range(1, n + 1):
    print(f"let r{i} = &mut (*r{i-1})[0];")
print("let t = sys::time::timer(duration:20.ms, false);")
print(f"*r{n} <- t ~ [[]];")
print("sys::exit(sys::time::after_idle(duration:200.ms, 0));")
print("array::len(a)")
