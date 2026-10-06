#!/usr/bin/env bash
# ide-tooling.r2-07: the tree-sitter grammar has no constructor-applied
# types (`self<'a>`, `'c<i64>`), so core's Collection trait parses as ERROR.
#
# The real parser applies `self` and a type variable to one argument
# (graphix-types/src/expr/parser/typexp.rs: typ() and fnpositional());
# ide/tree-sitter-graphix/grammar.js's type_variable (472), self_type (475)
# and the fn_type_arg receiver (525) take none. This script compiles the
# checkout's grammar with the tree-sitter CLI and parses sources the real
# parser accepts: a trait method over `self<'a>`, a lambda over `'c<i64>`,
# an interface val over `'c<i64>`, stdlib core's mod.gxi, and a control
# trait over a bare `self`.
#
# command: bash design/review-2026-10-05/repro/ide-tooling.r2-07.sh
#   (TS=/path/to/tree-sitter overrides the CLI, default the npm one under
#   ide/tree-sitter-graphix/node_modules; GRAPHIX=/path/to/graphix, default
#   the one on PATH, adds the real parser's verdict via `graphix fmt`)
#
# expected: no source parses to an ERROR node (exit 0)
# observed (HEAD c722befe, tree-sitter 0.24.7), exit 1:
#   trait_self_app.gx   real=ok      ts=(ERROR [1, 21] - [1, 25])
#   lambda_tvar_app.gx  real=ok      ts=(ERROR [0, 29] - [0, 31])
#   sig_tvar_app.gxi    real=ok      ts=(ERROR [0, 30] - [0, 32])
#   core_mod.gxi        real=ok      ts=(ERROR [53, 21] - [53, 25])
#   control_self.gx     real=ok      ts=ok
# core_mod.gxi holds 18 ERROR nodes, all on the 8 `self<..>` lines of the
# Collection trait; the lambda's whole `let` is lost to error recovery.
# A `_type` alternative `prec(1, seq(choice($.type_variable, $.self_type),
# '<', $._type, '>'))` plus `optional(seq('<', $._type, '>'))` after the
# fn_type_arg receiver generates without conflicts and parses all five
# (and every other .gx/.gxi in the tree as before); an optional `<T>` on
# type_variable itself conflicts with builtin_ref (`'name <`).
set -u
root=$(git -C "$(dirname "$0")" rev-parse --show-toplevel) || exit 2
ts=${TS:-$root/ide/tree-sitter-graphix/node_modules/tree-sitter-cli/tree-sitter}
if [ ! -x "$ts" ]; then
    ts=$(command -v tree-sitter) || { echo "no tree-sitter CLI found" >&2; exit 2; }
fi
gx=${GRAPHIX:-$(command -v graphix || true)}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/grammar" "$tmp/src"
cp -r "$root/ide/tree-sitter-graphix/grammar.js" \
    "$root/ide/tree-sitter-graphix/tree-sitter.json" \
    "$root/ide/tree-sitter-graphix/src" "$tmp/grammar/"
printf '{"parser-directories": []}\n' >"$tmp/config.json"
cat >"$tmp/src/trait_self_app.gx" <<'EOF'
trait Sized {
    val size: fn(self<'a>) -> i64
};
impl Sized for Array<'_> {
    let size = |a| array::len(a)
};
Sized::size([1, 2, 3])
EOF
cat >"$tmp/src/lambda_tvar_app.gx" <<'EOF'
let f = 'c: Collection |xs: 'c<i64>| -> 'c<string> Collection::map(xs, |x| "[x]");
f([1, 2, 3])
EOF
cat >"$tmp/src/sig_tvar_app.gxi" <<'EOF'
val f: fn<'c: Collection>(c: 'c<i64>) -> 'c<string>;
EOF
cp "$root/stdlib/graphix-package-core/src/graphix/mod.gxi" "$tmp/src/core_mod.gxi"
cat >"$tmp/src/control_self.gx" <<'EOF'
trait Sized {
    val size: fn(self) -> i64
};
impl Sized for Array<i64> {
    let size = |a| array::len(a)
};
Sized::size([1, 2, 3])
EOF
status=0
for f in trait_self_app.gx lambda_tvar_app.gx sig_tvar_app.gxi core_mod.gxi control_self.gx; do
    real=unknown
    if [ -n "$gx" ]; then
        if XDG_CACHE_HOME="$tmp/cache" timeout -s KILL 60 "$gx" fmt --stdout "$tmp/src/$f" \
            >/dev/null 2>&1; then
            real=ok
        else
            real=REFUSED
        fi
    fi
    out=$(cd "$tmp/grammar" &&
        XDG_CACHE_HOME="$tmp/cache" TREE_SITTER_LIBDIR="$tmp/lib" \
            timeout -s KILL 170 "$ts" parse --config-path "$tmp/config.json" -q \
            "$tmp/src/$f" 2>&1)
    res=$(printf '%s\n' "$out" | grep -o '(\(ERROR\|MISSING\)[^)]*)' | head -n 1)
    if [ -z "$res" ]; then
        res=ok
    elif [ "$real" != REFUSED ]; then
        status=1
    fi
    printf '%-19s real=%-7s ts=%s\n' "$f" "$real" "$res"
done
exit $status
