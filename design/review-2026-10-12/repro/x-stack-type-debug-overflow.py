# Generates a program whose `x6000 == x6000` makes fusion format a
# 6000-deep type with `{:?}`; the derived Debug of Type/TVar recurses
# without ensure_sufficient and the process aborts with a stack overflow.
#   python3 x-stack-type-debug-overflow.py > p.gx
#   graphix --no-cache p.gx          -> "has overflowed its stack", exit 134
#   graphix --no-cache --no-fusion p.gx -> true
d = 6000
print("let x0 = 1;")
for i in range(1, d + 1):
    print("let x%d = [x%d];" % (i, i - 1))
print("sys::exit(sys::time::after_idle(duration:1.s, 0));")
print("x%d == x%d" % (d, d))
