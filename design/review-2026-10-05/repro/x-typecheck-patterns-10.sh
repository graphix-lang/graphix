#!/usr/bin/env bash
# x-typecheck-patterns-10: a parent interface's `mod sub;` makes an
# interface-less submodule's abstract types public.
#
# export_sig (graphix-compiler/src/node/module.rs:339) publishes the rep of
# every abstract typedef under a re-exported `mod sub;` that carries a body.
# A typedef from a gxi body is already public (bind_sig registers it so), so
# the loop only ever publishes an interface-less descendant's own
# `type T = Abstract<..>`, which TypeDef::compile registered private. Whether
# `outer::sub::T(5)`, `.0` and the pattern `T(p)` compile outside outer::sub
# therefore depends on whether an ancestor has an interface: adding a gxi to
# the parent (a restriction) widens the child.
#
# design/nominal_abstract_types.md (The rule): no gxi + `type T =
# Abstract<u64>` is a "module-private nominal type". book/src/modules/
# interfaces.md:379 says the opposite ("public newtypes as well"), and so
# does the doc on Env::publish_abstract_rep. Every case below uses the same
# sub.gx; only the interfaces around it vary:
#   nested_gxi:   outer/mod.gxi = `mod sub;`, no sub.gxi
#   nested_plain: no outer/mod.gxi, no sub.gxi
#   flat:         top-level `mod m;`, no m.gxi
#   deep:         outer/mod.gxi `mod sub;`, sub/mod.gxi `mod deep;`, no deep.gxi
#   hidden:       nested_gxi plus sub.gxi = `type T; val make: ..` (control)
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-typecheck-patterns-10.sh
#
# expected: the first four alike. Per the design table all four refuse
# ("the definition of ... is not visible here, so it cannot be
# constructed"); per the book all four print "5 6". hidden refuses either way.
#
# observed (HEAD c722befe, debug build):
#   nested_gxi:   --check exit 0; run prints "5 6"
#   nested_plain: --check exit 1; the definition of outer::sub::T is not
#                 visible here, so it cannot be constructed
#   flat:         --check exit 1; the definition of m::T is not visible
#                 here, so it cannot be constructed
#   deep:         --check exit 0; run prints "5 6"
#   hidden:       --check exit 1; the definition of outer::sub::T is not
#                 visible here, so it cannot be constructed
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
sub='type T = Abstract<i64>;
let make = |x: i64| -> T T(x);'
main() {
    printf '%s\n' "mod $1;" "let u = $2(5);" "let v = select u { $2(p) => p + 1 };" \
        'sys::exit(sys::time::after_idle(duration:100.ms, 0));' '"[u.0] [v]"'
}

mkdir -p "$dir/nested_gxi/outer" "$dir/nested_plain/outer" "$dir/flat" \
    "$dir/deep/outer/sub" "$dir/hidden/outer"
for c in nested_gxi nested_plain hidden; do
    main outer outer::sub::T > "$dir/$c/main.gx"
    echo 'mod sub;' > "$dir/$c/outer/mod.gx"
    echo "$sub" > "$dir/$c/outer/sub.gx"
done
echo 'mod sub;' > "$dir/nested_gxi/outer/mod.gxi"
echo 'mod sub;' > "$dir/hidden/outer/mod.gxi"
printf '%s\n' 'type T;' 'val make: fn(x: i64) -> T;' > "$dir/hidden/outer/sub.gxi"
main m m::T > "$dir/flat/main.gx"
echo "$sub" > "$dir/flat/m.gx"
main outer outer::sub::deep::T > "$dir/deep/main.gx"
echo 'mod sub;' > "$dir/deep/outer/mod.gx"
echo 'mod sub;' > "$dir/deep/outer/mod.gxi"
echo 'mod deep;' > "$dir/deep/outer/sub/mod.gx"
echo 'mod deep;' > "$dir/deep/outer/sub/mod.gxi"
echo "$sub" > "$dir/deep/outer/sub/deep.gx"

for c in nested_gxi nested_plain flat deep hidden; do
    cd "$dir/$c" || exit 1
    out=$(timeout -s KILL 60 "$GRAPHIX" --no-cache --check main.gx 2>&1)
    st=$?
    if [ $st -eq 0 ]; then
        run=$(timeout -s KILL 60 "$GRAPHIX" --no-cache main.gx 2>&1 | tail -1)
        echo "$c: --check exit 0; run prints $run"
    else
        echo "$c: --check exit $st; $(echo "$out" | tail -1 | sed 's/^ *[0-9]*: //')"
    fi
done
