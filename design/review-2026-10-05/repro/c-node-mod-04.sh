#!/usr/bin/env bash
# c-node-mod-04: a sibling module sees another's undeclared impl unless the
# two `mod` statements are adjacent.
#
# typecheck0_statements (graphix-compiler/src/node/mod.rs:976) hides a
# module's undeclared impls (Env::hidden_impls) only inside a run of
# adjacent static modules (typecheck0_modules). A module checked alone,
# because any other statement separates it from its siblings, takes the
# serial branch with nothing hidden, and impls are registered at compile
# time, so it sees every sibling's undeclared impl, a LATER sibling's
# included.
#
# The modules are the pin trait_undeclared_impl_hidden_from_siblings
# (stdlib/graphix-tests/src/lang/traits.rs:274); only main.gx varies, plus
# an unrelated module c in the last two:
#   adjacent:  mod t; mod a; mod b;            (the pin)
#   apart:     mod t; mod a; let z = 0; mod b; (one unrelated statement)
#   before:    mod t; mod b; let z = 0; mod a; (b checked before a)
#   c_plain:   mod t; mod a; mod c; mod b;     (c has no interface)
#   c_gxi:     the same, c has a c.gxi
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/c-node-mod-04.sh
#
# expected: all five refused alike ("Siblings reach each other only
# through interfaces", CLAUDE.md, Module system): b uses the impl a adds
# without declaring it in a.gxi.
#
# observed (HEAD c722befe, debug build):
#   adjacent: --check exit 1, "compiling module b" ...
#             "type mismatch 'self: unbound within Show does not contain i64"
#   apart:    --check exit 0, and the run prints "int 1"
#   before:   --check exit 0, and the run prints "int 1"
#   c_plain:  --check exit 0, and the run prints "int 1"
#   c_gxi:    --check exit 1, same error as adjacent
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
declare -A mods=(
    [adjacent]='mod t; mod a; mod b;'
    [apart]='mod t; mod a; let z = 0; mod b;'
    [before]='mod t; mod b; let z = 0; mod a;'
    [c_plain]='mod t; mod a; mod c; mod b;'
    [c_gxi]='mod t; mod a; mod c; mod b;'
)
for v in adjacent apart before c_plain c_gxi; do
    d="$dir/$v"
    mkdir -p "$d"
    printf 'trait Show { val show: fn(self) -> string }\n' > "$d/t.gxi"
    printf 'let unused = 0\n' > "$d/t.gx"
    printf 'val unused: i64\n' > "$d/a.gxi"
    cat > "$d/a.gx" <<'GX'
use super::t::Show;
impl Show for i64 { let show = |x| "int [x]" };
let unused = 0
GX
    printf 'val shown: string\n' > "$d/b.gxi"
    printf 'let shown = super::t::Show::show(1)\n' > "$d/b.gx"
    printf 'let k = 1\n' > "$d/c.gx"
    if [ "$v" = c_gxi ]; then printf 'val k: i64\n' > "$d/c.gxi"; fi
    printf '%s\nprintln(b::shown);\nsys::exit(sys::time::after_idle(duration:100.ms, 0))\n' \
        "${mods[$v]}" | sed 's/; mod/;\nmod/g; s/; let/;\nlet/g' > "$d/main.gx"
    echo "=== $v: ${mods[$v]}"
    out=$(cd "$d" && timeout -s KILL 60 "$GRAPHIX" --check main.gx 2>&1)
    rc=$?
    echo "--check exit: $rc"
    if [ "$rc" -ne 0 ]; then
        echo "$out" | tail -1
    else
        (cd "$d" && timeout -s KILL 60 "$GRAPHIX" --no-cache main.gx 2>&1 | tail -1)
    fi
done
