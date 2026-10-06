#!/usr/bin/env bash
# x-image-04: decode_at's cycle check is defeated; a corrupt program/registration
# image drives ImageDecoder recursion without bound.
#
# ROOT CAUSE
#   graphix-types/src/image/mod.rs:1011  decode_at's guard refuses re-entry of an
#     ordinal only when `prev == built`, i.e. only when NO object was entered
#     since that ordinal's previous entry.
#   graphix-types/src/image/mod.rs:396   enter() does `self.built += 1` on EVERY
#     call (even one whose slot is already filled: "keeps its first").
#   A corrupt definition that decodes at least one inline object per round
#   (the Type -> TypeRef params(Vec<Type>) -> Type -> REF X path does) advances
#   `built` each round, so `prev != built` forever and the same ordinals recurse.
#   Each level runs under stack::ensure_sufficient, which mmaps a fresh 32 MB
#   segment (graphix-types/src/stack.rs:19,97,180): memory grows without bound.
#   design/program_image.md (~l.296) promises the opposite: "entered one
#   definition at most twice; a third entry is a corrupt image and fails the
#   read." CLAUDE.md: "an entry that fails to read ... starts cold." Neither holds.
#
# PROGRAM (04_mixed.gx below) is a correct program; the fault is in the restore
# of its CACHED image, not the program. The fixture x-image-04.img.gz is one
# confirmed single-bit-corrupted program image (BuildID 9e40907a..., the review
# binary); the trigger offset is build/layout-specific (~0.2% of random single
# corruptions hit it) so a fresh binary needs its own, found by the reviewer's
# corrupt.py (scratch work dir) -- this script reuses the saved fixture.
#
# COMMAND
#   GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-image-04.sh
#   (RUNAWAY=1 also demonstrates the uncapped memory runaway in a private 8G
#    cgroup scope, needs systemd-run --user.)
#
# EXPECTED (the documented contract): a bad entry fails to read and the shell
#   compiles cold, printing the program's normal output ("... 2 ...").
#
# OBSERVED (HEAD c722befe, review debug binary):
#   capped (prlimit --as=4G): thread 'tokio-rt-worker' panicked ... stacker
#     mmap_stack_restore_guard.rs: "mmap failed to allocate stack: Cannot
#     allocate memory", then "Error: loading initial modules / channel closed",
#     exit 1 -- never cold.  (a registration-entry corruption instead aborts
#     with "memory allocation of 4096 bytes failed", SIGABRT/exit 134.)
#   uncapped: RSS climbs ~2-2.5 GB/s unbounded (14.7 GB at 5 s, 19.5 GB at 8 s
#     on the reviewer's box) until the process -- or the user session -- is
#     OOM-killed.
set -u
GRAPHIX=${GRAPHIX:-/tmp/claude-1000/-home-eric-proj-graphix/2045fc9c-aba3-4ce1-8aab-15cf5ab9093c/scratchpad/bin/real/graphix}
HERE=$(cd "$(dirname "$0")" && pwd)
FIX="$HERE/x-image-04.img.gz"
REGKEY=ff042299e1a1ad7e5173f42b767c779f
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

cat > "$dir/04_mixed.gx" <<'GX'
type Shape = [`Circle(f64), `Rect(f64, f64)];
let area = |s: Shape| -> f64 select s { `Circle(r) => 3.0 * r * r, `Rect(w, h) => w * h };
let clk = sys::time::timer(duration:10.ms, 10);
let n = 0;
n <- clk ~ n + 1;
let shapes: Array<Shape> = array::map([1, 2, 3], |i| `Rect(cast<f64>(i)$, 2.0));
let late = select n {
  x if x > 5 => array::fold(shapes, 0.0, |acc, s| acc + area(s)) + cast<f64>(x)$,
  _ => 0.0
};
let m = {"a" => 1, "b" => 2};
let s = str::join(#sep: ",", array::map([n, n + 1], |x| "[x]"));
sys::exit(sys::time::after_idle(duration:150.ms, n ~ 0));
"[n] [late] [m{"b"}$] [s]"
GX

echo "== cold build (populates the image cache)"
timeout -s KILL 60 "$GRAPHIX" "$dir/04_mixed.gx" >/dev/null 2>&1 || true
reg=$(ls "$XDG_CACHE_HOME"/graphix/registration/*/ 2>/dev/null | head -1)
dd=$(dirname "$(ls "$XDG_CACHE_HOME"/graphix/registration/*/*.img 2>/dev/null | head -1)")
prog=$(ls "$dd"/*.img 2>/dev/null | grep -v "$REGKEY" | head -1)
if [ -z "${prog:-}" ]; then echo "no program image was cached; aborting"; exit 2; fi
echo "   program entry: $prog"

echo "== install the corrupt program image and run warm, capped at 4G AS"
gunzip -c "$FIX" > "$prog"
out=$( (ulimit -c 0; prlimit --as=4294967296 timeout -s KILL 30 "$GRAPHIX" "$dir/04_mixed.gx") 2>&1 )
rc=$?
echo "$out" | tail -4
echo "exit $rc"
echo
echo "   A correct restore would print the program value and exit 0, or fail to"
echo "   read the entry and compile cold. Instead the decode recurses until the"
echo "   stack mmap fails; the shell dies with 'channel closed' (exit 1)."

if [ "${RUNAWAY:-0}" = 1 ]; then
  echo
  echo "== uncapped memory runaway (private 8G cgroup scope; needs systemd-run --user)"
  gunzip -c "$FIX" > "$prog"
  systemd-run --user --scope --quiet --collect -p MemoryMax=8G -p MemorySwapMax=0 -- \
    bash -c 'timeout -s KILL 12 "$0" "$1" >/dev/null 2>&1 & pid=$!;
             for i in $(seq 1 12); do sleep 1; r=$(awk "/VmRSS/{print \$2}" /proc/$pid/status 2>/dev/null); [ -n "$r" ] && echo "  t=${i}s RSS $((r/1024)) MB" || break; done;
             kill -9 $pid 2>/dev/null' "$GRAPHIX" "$dir/04_mixed.gx"
  echo "   (RSS climbs without bound; the scope OOM-kills it at 8G.)"
fi
