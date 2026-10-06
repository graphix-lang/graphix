#!/bin/bash
# f-mod-lowering-05: a JIT that cannot be built fails every compile
# instead of falling back to the node-walk.
#
# FusionCtx::emission() builds the JIT on first use (host ISA + a 256MB
# arena reservation); build_region (fusion/mod.rs:1262, 1292) and
# fuse_each (1122, 1141) propagate its error with `?`, so check_and_fuse
# refuses the program.
#
# command: GRAPHIX=<path to graphix> bash design/review-2026-10-05/repro/f-mod-lowering-05.sh
# expected: every run prints 41 (fusion lost, the node-walk runs the program)
# observed (HEAD c722befe):
#   fused, arena unreservable: Error: in file .../arena.gx
#                              Caused by: jit arena reservation failed:
#                              System call failed: Cannot allocate memory (os error 12)
#   --no-fusion, same env:     41
#   fused under `ulimit -v 1400000` (no env var; the window is
#   machine-dependent, 1.3-1.4GB on a 16-core box): the same error;
#   --no-fusion under the same limit: 41
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
cat > "$dir/arena.gx" <<'EOF'
let f = |x: i64| x * 2 + 1;
let r = f(20);
sys::exit(sys::time::after_idle(duration:100.ms, 0));
r
EOF
echo "== fused, GRAPHIX_JIT_ARENA beyond the address space"
GRAPHIX_JIT_ARENA=1000000000000000 timeout -s KILL 60 "$GRAPHIX" --no-cache "$dir/arena.gx"
echo "rc=$?"
echo "== --no-fusion, same env"
GRAPHIX_JIT_ARENA=1000000000000000 timeout -s KILL 60 "$GRAPHIX" --no-cache --no-fusion "$dir/arena.gx"
echo "rc=$?"
echo "== fused, ulimit -v 1400000 (machine-dependent window)"
bash -c "ulimit -v 1400000; timeout -s KILL 60 '$GRAPHIX' --no-cache '$dir/arena.gx'"
echo "rc=$?"
echo "== --no-fusion, ulimit -v 1400000"
bash -c "ulimit -v 1400000; timeout -s KILL 60 '$GRAPHIX' --no-cache --no-fusion '$dir/arena.gx'"
echo "rc=$?"
