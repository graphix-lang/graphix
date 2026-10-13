# x-stack-01: Type/TVar Debug recurses without ensure_sufficient; fusion's
# {t:?} error formatting (emit_cmp_node) aborts the process on a deep type.
#   python3 x-stack-01.py > p.gx
#   graphix --no-cache p.gx
# expected: true (as with --no-fusion; --check passes)
# observed: "thread 'tokio-rt-worker' has overflowed its stack", exit 134.
#   gdb: ~1900 <Type as Debug>::fmt frames (20k frames total) under
#   anyhow::__private::format_err <- emit_cmp_node <- emit_return_from_node
#   <- compile_region <- build_region <- try_fuse <- check_and_fuse_inner.
# The same program ending in `x6000` alone (no `==`) runs fine with fusion;
# d=3000 does not overflow on the runtime thread's stack.
d = 6000
print("let x0 = 1;")
for i in range(1, d + 1):
    print("let x%d = [x%d];" % (i, i - 1))
print("sys::exit(sys::time::after_idle(duration:1.s, 0));")
print("x%d == x%d" % (d, d))
