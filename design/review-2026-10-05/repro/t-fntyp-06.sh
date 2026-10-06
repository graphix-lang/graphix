#!/usr/bin/env bash
# t-fntyp-06: in-place normalization writes a cell another compile task
# owns, so a module with an interface that only READS a parent's settled
# binding is refused with "the check decides a type the code around the
# module left open".
#
# `let v = never(); v <- [`A, `B][0]` binds v's cell (task 0) to
# ['_N: [`A, `B], Error<`ArrayIndexError(string)>], whose normal form is
# [Error<`ArrayIndexError(string)>, `A, `B] (a type error on v prints the
# first before any select over v, the second after). A select over
# v in the module normalizes its scrutinee (Select::typecheck0_with,
# check_dead_arms); Type::normalize_int's TVar arm calls
# TVar::normalize_int (graphix-types/src/typ/tvar.rs:989-999), which
# re-binds the cell to its flattened form through TVar::bind. Under the
# module's task that bind is a foreign decision (tvar::decided), so the
# write is not made and Module::typecheck0 refuses the module.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-fntyp-06.sh
#
# expected (design/parallel_compile.md: only a binding whose first use is
# in a child module must be annotated; here the parent's writer decided
# v before `mod inner`): every case checks and prints 5.
# observed (HEAD c722befe, debug build):
#   1. with inner.gxi: refused, run and --check alike (exit 1),
#      "compiling module inner / the check decides a type the code
#      around the module left open: annotate the binding it decides";
#      under GRAPHIX_TASK_AUDIT=1 the module's only foreign writes are
#      two, both TVar::bind <- TVar::normalize_int <- Type::normalize <-
#      Select::typecheck0_with / Select::check_dead_arms (with --no-cache
#      the stdlib interfaces' own frozen-variable writes print as well)
#   2. the same module without inner.gxi: 5
#   3. the same code inline: 5
#   4. case 1 plus a `select v` in the parent before `mod inner` (which
#      normalizes the cell in task 0 first): 5
#   (case 1 with v annotated, normal form or not, also checks)
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
mkdir -p "$dir/gxi" "$dir/nogxi" "$dir/inline" "$dir/pre"

inner='use super::v;
let f = |x: i64| -> i64 select v { _ => x }'
parent='let v = never();
v <- [`A, `B][0];
mod inner;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
inner::f(5)'

printf '%s\n' "$inner" > "$dir/gxi/inner.gx"
printf 'val f: fn(x: i64) -> i64\n' > "$dir/gxi/inner.gxi"
printf '%s\n' "$parent" > "$dir/gxi/main.gx"

printf '%s\n' "$inner" > "$dir/nogxi/inner.gx"
printf '%s\n' "$parent" > "$dir/nogxi/main.gx"

cat > "$dir/inline/main.gx" <<'EOF'
let v = never();
v <- [`A, `B][0];
let f = |x: i64| -> i64 select v { _ => x };
sys::exit(sys::time::after_idle(duration:100.ms, 0));
f(5)
EOF

cp "$dir/gxi/inner.gx" "$dir/gxi/inner.gxi" "$dir/pre/"
cat > "$dir/pre/main.gx" <<'EOF'
let v = never();
v <- [`A, `B][0];
let z = select v { _ => 0 };
mod inner;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
inner::f(5) + z
EOF

run() { timeout -s KILL 30 "$GRAPHIX" --no-cache "$1" 2>&1 | tail -3; }
echo "1. module with interface:"; run "$dir/gxi/main.gx"
echo "2. module without interface:"; run "$dir/nogxi/main.gx"
echo "3. inline:"; run "$dir/inline/main.gx"
echo "4. with interface, parent selects on v first:"; run "$dir/pre/main.gx"
