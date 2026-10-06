#!/usr/bin/env bash
# t-format-resolver.r2-04: an interface-only `type` (or `trait`) is spliced
# after whatever implementation statement binds the `val` the .gxi lists
# before it, so the implementation can name it only below that statement.
#
# add_interface_modules (graphix-types/src/expr/resolver.rs:418) anchors
# each interface-only item after the interface item before it, a `val`
# included. bind_sig registers the .gxi's types and traits in the outer
# env, but the body compiles under the module's env cloned before it, so
# the body sees only the spliced copy, at its splice point; a written
# annotation is looked up when its statement compiles.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-format-resolver.r2-04.sh
#
# Cases (each m.gx/m.gxi with main.gx `mod m; ...`):
#   A   gxi `val a: i64; type T = i64; val b: T`, gx `let b: T = 1; let a = 2`
#   A2  A with the gx lets swapped (`let a = 2; let b: T = 1`)
#   B   gxi `val f: fn(x: T) -> i64; type T = i64`, gx `let f = |x: T| -> i64 x`
#       (T lands after `let f` whatever the gx's order)
#   C   gxi `val a: i64; trait Show {..}; val b: string`,
#       gx `impl Show for i64 {..}; let b = Show::show(5); let a = 2`
#   C2  C with `let a = 2` first
#   D   gxi `use str::len; use array::map; type T = i64; val f: fn() -> T`,
#       gx `use array::map; let f = || -> T 1; use str::len`, checked before
#       and after `graphix fmt m.gxi` (which sorts the two uses)
# expected: every case checks ok; the interface's types and traits are
#   "automatically available in the implementation"
#   (book/src/modules/interfaces.md), whatever order the .gx binds its vals in
# observed (HEAD c722befe, debug build):
#   A   undefined type T in m  (at m.gx:1:1, let b: T = 1)
#   A2  ok
#   B   undefined type T in m  (at m.gx:1:9, |x: T| -> i64 x)
#   C   no trait `Show` in scope  (at m.gx:1:1, the impl)
#   C2  ok
#   D   before fmt: ok; after fmt: undefined type T in m (at m.gx:2:17)
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

check() { # check <case dir> <label>: ok, or the error's last two lines
    printf '%-14s ' "$2"
    local out
    if out=$(timeout -s KILL 60 "$GRAPHIX" --check "$1/main.gx" 2>&1); then
        echo ok
    else
        printf '%s\n' "$out" | grep -v '^\s*$' | tail -2 \
            | sed -E -e 's#in file [^ ]*/([^/ ]*)#in file \1#g' -e 's/^ *[0-9]*: //' \
            | tr '\n' ' '
        echo
    fi
}

mk() { # mk <case> <m.gxi> <m.gx> <main.gx>
    mkdir -p "$dir/$1"
    printf '%s\n' "$2" > "$dir/$1/m.gxi"
    printf '%s\n' "$3" > "$dir/$1/m.gx"
    printf '%s\n' "$4" > "$dir/$1/main.gx"
}

mk A 'val a: i64;
type T = i64;
val b: T' 'let b: T = 1;
let a = 2' 'mod m;
m::a + m::b'
mk A2 'val a: i64;
type T = i64;
val b: T' 'let a = 2;
let b: T = 1' 'mod m;
m::a + m::b'
mk B 'val f: fn(x: T) -> i64;
type T = i64' 'let f = |x: T| -> i64 x' 'mod m;
m::f(41) + 1'
mk C 'val a: i64;
trait Show { val show: fn(self) -> string };
val b: string' 'impl Show for i64 { let show = |x| "int [x]" };
let b = Show::show(5);
let a = 2' 'mod m;
"[m::a] [m::b]"'
mk C2 'val a: i64;
trait Show { val show: fn(self) -> string };
val b: string' 'let a = 2;
impl Show for i64 { let show = |x| "int [x]" };
let b = Show::show(5)' 'mod m;
"[m::a] [m::b]"'
mk D 'use str::len;
use array::map;
type T = i64;
val f: fn() -> T' 'use array::map;
let f = || -> T 1;
use str::len;' 'mod m;
m::f()'

for c in A A2 B C C2; do check "$dir/$c" "$c"; done
check "$dir/D" "D before fmt"
timeout -s KILL 60 "$GRAPHIX" fmt "$dir/D/m.gxi"
check "$dir/D" "D after fmt"
