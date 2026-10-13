#!/bin/sh
# A bare `let` of a literal builds a JIT kernel that returns the constant.
# So a program whose only computation is an async builtin "fuses", and a
# run! fixture's FuseExpect::Jit holds for it.
# Expect: 1 kernel defined (for the literal 0), CLIF returns iconst 0 with no inputs.
GX=${GX:-graphix}
d=$(mktemp -d)
echo '{ let x = 0; throttle(x) }' > "$d/k.gx"
GRAPHIX_DBG_KERNELS=1 GRAPHIX_DUMP_CLIF=1 timeout -s KILL 5 "$GX" --no-cache "$d/k.gx" 2>&1 | head -40
