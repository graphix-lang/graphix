#!/usr/bin/env bash
# ide-tooling-03: the tree-sitter grammar has no type-constructor
# application, so `self<'a>` and `'c<T>` parse to ERROR nodes (core's
# Collection trait, the book's constructor-trait example). Same defect as
# ide-tooling.r2-07.sh.
#
# The real parser applies `self` (graphix-types/src/expr/parser/typexp.rs
# fnpositional() and typ()) and a type variable (typ()) to one argument;
# ide/tree-sitter-graphix/grammar.js type_variable (472), self_type (475)
# and the fn_type_arg receiver self_param (525) take none.
#
# command (from the repo root):
#   GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/ide-tooling-03.sh
#   (TS=/path/to/tree-sitter overrides the CLI, default the npm one under
#   ide/tree-sitter-graphix/node_modules)
#
# expected: every source the real parser accepts has 0 ERROR nodes.
# observed (HEAD c722befe, tree-sitter 0.24.7), exit 1:
#   core_mod.gxi     real=ok      ts_errors=18
#   book_sqr.gx      real=ok      ts_errors=3
#   control_sqr.gx   real=ok      ts_errors=0
#   lambda body of book_sqr.gx: (builtin_ref [1, 55] - [1, 57])
# All 18 ERROR nodes of core mod.gxi are on the 8 `self<..>` lines of the
# Collection trait. book_sqr.gx is book/src/udt/traits.md:329; error
# recovery turns its lambda body into the builtin reference `'a` and
# `map(c, ..)` into the right side of a binary expression.
set -u
root=$(git -C "$(dirname "$0")" rev-parse --show-toplevel) || exit 2
ts=${TS:-$root/ide/tree-sitter-graphix/node_modules/tree-sitter-cli/tree-sitter}
gx=${GRAPHIX:-graphix}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/parsers/tree-sitter-graphix" "$tmp/src"
cp -r "$root/ide/tree-sitter-graphix/grammar.js" \
    "$root/ide/tree-sitter-graphix/tree-sitter.json" \
    "$root/ide/tree-sitter-graphix/src" "$tmp/parsers/tree-sitter-graphix/"
printf '{"parser-directories": ["%s"]}\n' "$tmp/parsers" >"$tmp/config.json"
cp "$root/stdlib/graphix-package-core/src/graphix/mod.gxi" "$tmp/src/core_mod.gxi"
cat >"$tmp/src/book_sqr.gx" <<'EOF'
use Collection::*;
let sqr = 'c: Collection, 'a: Number |c: 'c<'a>| -> 'c<'a> map(c, |n| n * n);
sqr([1, 2, 3])
EOF
sed "s/'c<'a>/'c/g" "$tmp/src/book_sqr.gx" >"$tmp/src/control_sqr.gx"
tsparse() {
    XDG_CACHE_HOME="$tmp/cache" TREE_SITTER_LIBDIR="$tmp/lib" \
        timeout -s KILL 170 "$ts" parse --config-path "$tmp/config.json" "$1" 2>&1
}
status=0
for f in core_mod.gxi book_sqr.gx control_sqr.gx; do
    if XDG_CACHE_HOME="$tmp/cache" timeout -s KILL 60 "$gx" fmt --stdout "$tmp/src/$f" \
        >/dev/null 2>&1; then
        real=ok
    else
        real=REFUSED
    fi
    tree=$(tsparse "$tmp/src/$f")
    n=$(printf '%s\n' "$tree" | grep -E '^ ' | grep -cE '\((ERROR|MISSING)')
    [ "$n" -gt 0 ] && [ "$real" = ok ] && status=1
    printf '%-16s real=%-7s ts_errors=%s\n' "$f" "$real" "$n"
done
printf 'lambda body of book_sqr.gx: %s\n' \
    "$(tsparse "$tmp/src/book_sqr.gx" | grep -oE '\(builtin_ref \[[0-9, ]+\] - \[[0-9, ]+\]' | head -n 1))"
exit $status
