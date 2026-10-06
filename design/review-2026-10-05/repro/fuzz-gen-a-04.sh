#!/usr/bin/env bash
# fuzz-gen-a-04: the generator registers m<i>::csize at three collection
# types, but only the last registration (Array<string>) is ever callable.
#
# graphix-fuzz/src/generate/modules.rs:428 pushes `m<i>::csize` at
# Array<i64>, Map<string, i64> and Array<string> "so call sites dispatch
# across them"; GenCtx::visible_entries (mod.rs:283), which fns_returning,
# vars_of and poly_fns all go through, keeps one entry per name (the last
# pushed), so try_call only ever sees fn(Array<string>) -> i64.
#
# command:
#   GRAPHIX_FUZZ=/path/to/graphix-fuzz bash design/review-2026-10-05/repro/fuzz-gen-a-04.sh
#
# expected: some generated csize call sites pass a Map<string, i64> or an
#   Array<i64>; the hand-written witness below (one call at each of the
#   three registered types) compiles and agrees at [2, 3, 1].
# observed (HEAD c722befe, debug build):
#   seed 7, 2000 programs: 54 csize call sites, 0 at Map, 0 at Array<i64>
#   seed 11, 1500 programs: 40 call sites, 0 at Map, 0 at Array<i64>
#   seed 3, 1500 programs: 24 call sites, 0 at Map, 0 at Array<i64>
#   (every argument is a string array: literals, re::split, m<j>::k<j>,
#   a struct field, `{ let mt: Array<string> = []; mt }`)
#   witness: AGREE, every mode Trace([0:[i64:2, i64:3, i64:1]]); the same
#   calls against the bare-module variant (no m0.gxi), plus two from inside
#   a lambda, also AGREE at [2, 3, 1, 4]
set -u
FUZZ=${GRAPHIX_FUZZ:-$(command -v graphix-fuzz || echo "$HOME/tmp/target/debug/graphix-fuzz")}
N=${N:-2000}
SEED=${SEED:-7}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT

# 1. what the generator emits: every csize call site, bucketed by argument
timeout -s KILL 120 "$FUZZ" gen "$N" "$SEED" > "$dir/gen.txt" || exit 1
grep -o -E '(super::|package::)?m[0-9]+::csize\([^)]{0,40}' "$dir/gen.txt" \
  | sed -E 's/^.*::csize\(//' > "$dir/args.txt"
awk '
  /^\[i64:/ || /Array<i64>/ { a++; next }
  /^\{"/ || /Map</ { m++; next }
  { s++ }
  END { printf "seed %s, %s programs: %d csize call sites: Array<i64> %d, Map %d, other (string arrays) %d\n", seed, n, a + m + s, a, m, s }
' seed="$SEED" n="$N" "$dir/args.txt"
echo "arguments:"
sort "$dir/args.txt" | uniq -c | sort -rn | sed 's/^/  /'

# 2. the shapes the lost registrations stand for are valid programs
cat > "$dir/witness.gx" <<'EOF'
{ let a = m0::csize([i64:1, i64:2]); let b = m0::csize({"a" => i64:1, "b" => i64:2, "c" => i64:3}); let c = m0::csize(["x"]); [a, b, c] }
// file-v1: m0.gxi
val csize: fn(c: Collection) -> i64;
// file-v1: m0.gx
let csize = |c: Collection| Collection::fold(c, i64:0, |acc, x| acc + i64:1);
EOF
echo "witness (calls at all three registered types):"
XDG_CACHE_HOME="$dir/cache" timeout -s KILL 180 "$FUZZ" run "$dir/witness.gx" 2>/dev/null \
  | grep -E 'Trace|Err' | sed 's/^/  /'
XDG_CACHE_HOME="$dir/cache" timeout -s KILL 180 "$FUZZ" check "$dir/witness.gx" 2>/dev/null \
  | sed 's/^/  /'
