#!/usr/bin/env bash
# f-jit-05: a warm start installs and finalizes each restored region on its
# own (FusedKernel::image_decode / SlotShare::image_decode -> Jit::load_wrapped
# -> Generation::install -> finalize_definitions, graphix-compiler/src/fusion/
# emit/jit.rs). cranelift-jit's ArenaMemoryProvider never extends a finalized
# segment, so every restored region starts a fresh page-aligned segment and
# pays an mprotect (and on aarch64 a membarrier) of its own; a cold link
# installs a whole batch with one finalize.
#
# The program has 600 one-line #[native] regions (the jit_arena_rotation.rs
# shape). The first run compiles cold and writes the image, the second starts
# warm from it.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/f-jit-05.sh
#
# expected: a warm start finalizes about as often as the cold link and fits
#   the arena the cold run fits.
# observed (HEAD c722befe, debug build):
#   GRAPHIX_PROFILE Finalize: cold 3 calls (0.13-0.24 ms), warm 601 calls
#   (1.8-3.5 ms)
#   GRAPHIX_JIT_ARENA=1048576: cold rotates 0 times, warm 2 times
#   ("JIT code arena exhausted: retired generation 1/2" in the --log-dir log);
#   every run prints 539700.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT

python3 - "$dir/regions.gx" <<'EOF'
import sys
n = 600
s = "let x = sys::time::after_idle(duration:1.ms, 3);\n"
s += "".join(f"let r{i} = #[native] (x * {i} + 1);\n" for i in range(n))
s += "let total = " + " + ".join(f"r{i}" for i in range(n)) + ";\n"
s += "println(total);\n"
s += f"sys::exit(select total {{ {sum(3 * i + 1 for i in range(n))} => 0, _ => 1 }})\n"
open(sys.argv[1], "w").write(s)
EOF

run() {
  local log="$dir/log-$RANDOM"
  mkdir -p "$log"
  env "${@:2}" timeout -s KILL 120 "$GRAPHIX" --no-netidx --log-dir "$log" \
    "$dir/regions.gx" > "$dir/out" 2> "$dir/err"
  echo "$1: exit $?, prints $(cat "$dir/out")," \
    "finalize: $(grep -h 'phase=Finalize ' "$dir/err" | awk '{print $4, $5}' | tr '\n' ' ')," \
    "rotations: $(cat "$log"/*.log 2>/dev/null | grep -c 'retired generation')"
}

export XDG_CACHE_HOME="$dir/cache1"
run "cold, 256 MiB arena" GRAPHIX_PROFILE=1
run "warm, 256 MiB arena" GRAPHIX_PROFILE=1
export XDG_CACHE_HOME="$dir/cache2"
run "cold, 1 MiB arena" RUST_LOG=warn GRAPHIX_JIT_ARENA=1048576
run "warm, 1 MiB arena" RUST_LOG=warn GRAPHIX_JIT_ARENA=1048576
