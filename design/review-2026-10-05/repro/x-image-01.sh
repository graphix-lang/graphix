#!/usr/bin/env bash
# x-image-01: the program image key covers only the root file's bytes, so a
# warm start runs stale module code and masks its compile errors.
#
# RegistrationCache::new (graphix-shell/src/cache.rs:136) keys the program
# entry by (image format, package root, flags, root file bytes). Nothing the
# resolvers read while compiling the program is in the key or re-checked on
# load: `mod m;` files beside the script, GRAPHIX_MODPATH, the script's own
# path. A hit restores the whole compiled program (graphix-rt/src/gx.rs:306,
# t.program is Some, load_program never runs). design/program_image.md
# ("Key", "Correctness") specifies a depfile re-verified on the next run and
# "a cache never masks a compile error"; CLAUDE.md: "a hit must be
# indistinguishable from a cold compile" is the image's contract.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-image-01.sh
#
# expected: every warm run prints what --no-cache prints (or fails as it does).
#
# observed (HEAD c722befe, debug build):
#   edit:      cold 1; m.gx -> `let x = 2`: warm 1, --no-cache 2
#   typeerr:   m.gx -> `let x: i64 = "oops"`: warm prints 1, exit 0;
#              --no-cache exit 1, "type mismatch i64 does not contain string"
#              (a parse error in m.gx, or deleting m.gx, is masked the same way)
#   twin:      a/main.gx == b/main.gx, different m.gx: b warm prints "project A"
#   modpath:   GRAPHIX_MODPATH=file:libA cold "libA"; =file:libB warm "libA",
#              --no-cache "libB"
#   origin:    second/main.gx warm: a caught error's ori.source is
#              `File(".../first/main.gx")
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
run() { timeout -s KILL 60 "$GRAPHIX" "$@" 2>&1 | tail -1; }
main='mod m;
sys::exit(sys::time::after_idle(duration:100.ms, m::x ~ 0));
m::x
'

echo "=== edit"
export XDG_CACHE_HOME="$dir/cache-edit"
mkdir -p "$dir/p" && cd "$dir/p"
printf '%s' "$main" > main.gx
printf 'let x = 1\n' > m.gx
echo "cold:       $(run main.gx)"
printf 'let x = 2\n' > m.gx
echo "warm:       $(run main.gx)"
echo "--no-cache: $(run --no-cache main.gx)"

echo "=== typeerr"
printf 'let x: i64 = "oops"\n' > m.gx
out=$(timeout -s KILL 60 "$GRAPHIX" main.gx 2>&1); rc=$?
echo "warm:       exit $rc: $(echo "$out" | tail -1)"
out=$(timeout -s KILL 60 "$GRAPHIX" --no-cache main.gx 2>&1); rc=$?
echo "--no-cache: exit $rc: $(echo "$out" | tail -1)"

echo "=== twin"
export XDG_CACHE_HOME="$dir/cache-twin"
for d in a b; do mkdir -p "$dir/$d"; printf '%s' "$main" > "$dir/$d/main.gx"; done
printf 'let x = "project A"\n' > "$dir/a/m.gx"
printf 'let x = "project B"\n' > "$dir/b/m.gx"
echo "a cold:       $(run "$dir/a/main.gx")"
echo "b warm:       $(run "$dir/b/main.gx")"
echo "b --no-cache: $(run --no-cache "$dir/b/main.gx")"

echo "=== modpath"
export XDG_CACHE_HOME="$dir/cache-modpath"
mkdir -p "$dir/q" "$dir/libA" "$dir/libB"
printf 'mod lib;\nsys::exit(sys::time::after_idle(duration:100.ms, lib::x ~ 0));\nlib::x\n' \
    > "$dir/q/main.gx"
printf 'let x = "libA"\n' > "$dir/libA/lib.gx"
printf 'let x = "libB"\n' > "$dir/libB/lib.gx"
echo "libA cold:       $(GRAPHIX_MODPATH=file:$dir/libA run "$dir/q/main.gx")"
echo "libB warm:       $(GRAPHIX_MODPATH=file:$dir/libB run "$dir/q/main.gx")"
echo "libB --no-cache: $(GRAPHIX_MODPATH=file:$dir/libB run --no-cache "$dir/q/main.gx")"

echo "=== origin"
export XDG_CACHE_HOME="$dir/cache-origin"
mkdir -p "$dir/first" "$dir/second"
cat > "$dir/first/main.gx" <<'GX'
let out = never();
{
    catch(e) out <- (e.0).ori.source;
    error(`Boom)?
};
sys::exit(sys::time::after_idle(duration:100.ms, out ~ 0));
out
GX
cp "$dir/first/main.gx" "$dir/second/main.gx"
echo "first cold:   $(run "$dir/first/main.gx")"
echo "second warm:  $(run "$dir/second/main.gx")"
