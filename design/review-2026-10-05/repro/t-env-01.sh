#!/usr/bin/env bash
# t-env-01: a type and a trait of one name do not shadow each other; which
# one a name means depends on where it is written.
#
# Env::trait_of_ref (graphix-types/src/env.rs:908-913) asks lookup_trait,
# which searches the trait tables alone (lexical chain, then the core
# prelude); TypeRef::resolve_pure (graphix-types/src/typ/mod.rs:604-616)
# searches the typedef tables alone. Parameter types (node/lambda.rs:1379),
# return types and typedef bodies (rewrite_trait_args, typ/mod.rs:2143 and
# 2157) and contains (typ/contains.rs:493-502) ask trait_of_ref first, so
# any visible trait, the core prelude's Eq/Ord/Display/Collection included,
# beats a closer typedef or an explicit import. A let annotation
# (node/bind.rs:159) and never<T> (node/mod.rs:1896) call
# rewrite_trait_args BEFORE scope_refs, so their trait test runs from `/`
# (the parser's scope for every ref, expr/parser/typexp.rs:390): it sees
# the core prelude but not a trait declared in the script's #do block.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-env-01.sh
#
# expected (design/module_system.md: the core prelude is "shadowable by
# declarations and imports"; precedence "own declaration -> explicit import
# -> glob -> package prelude -> core prelude"; design/traits.md: a trait
# "resolves like any other" name; types and traits share one namespace,
# pin types.rs `type_after_trait_of_one_name`):
#   ctl `Desc   a `Desc   b `Asc   c "full"   d 5
#   e refused: trait T used as a type (the block's trait shadows type T)
#   f one verdict from --check and from the run: trait T used as a type
#
# observed (HEAD c722befe, debug build):
#   ctl: `Desc
#   a: trait Ord used as a type: a trait is a bound ...
#   b: trait Ord used as a type: a trait is a bound ...
#   c: missing match cases type mismatch [`Compact, `Full] does not contain
#      '#d: unbound within Display
#   d: trait T used as a type: a trait is a bound ...
#   e: 6
#   f: check: trait T used as a type ...; run: undefined type T in #do<id>
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
exit_line='sys::exit(sys::time::after_idle(duration:100.ms, 0));'
flip='let flip = |o: Ord| -> Ord select o { `Asc => `Desc, `Desc => `Asc };'

mkdir -p "$dir/ctl" "$dir/a" "$dir/b" "$dir/c" "$dir/d" "$dir/e" "$dir/f"
# control: the same program with the type named Order
printf '%s\n' 'type Order = [`Asc, `Desc];' \
  'let flip = |o: Order| -> Order select o { `Asc => `Desc, `Desc => `Asc };' \
  "$exit_line" 'flip(`Asc)' > "$dir/ctl/main.gx"
# A: a type named like a core trait, as a parameter and return type
printf '%s\n' 'type Ord = [`Asc, `Desc];' "$flip" "$exit_line" 'flip(`Asc)' \
  > "$dir/a/main.gx"
# B: the same type in a let annotation
printf '%s\n' 'type Ord = [`Asc, `Desc];' 'let o: Ord = `Asc;' "$exit_line" 'o' \
  > "$dir/b/main.gx"
# C: an explicitly imported type named Display, as a parameter type
printf '%s\n' 'type Display = [`Full, `Compact]' > "$dir/c/m.gx"
printf '%s\n' 'mod m;' 'use m::Display;' \
  'let render = |d: Display| -> string select d { `Full => "full", `Compact => "compact" };' \
  "$exit_line" 'render(`Full)' > "$dir/c/main.gx"
# D: a block's type T under an outer trait T, in a typedef body
printf '%s\n' 'trait T { val show: fn(self) -> string };' \
  'let r = { type T = i64; type U = [T, string]; let u: U = 5; u };' \
  "$exit_line" 'r' > "$dir/d/main.gx"
# E: a block's trait T under an outer type T, in a let annotation
printf '%s\n' 'type T = i64;' \
  'let r = { trait T { val show: fn(self) -> string }; let v: T = 5; v + 1 };' \
  "$exit_line" 'r' > "$dir/e/main.gx"
# F: a script's own trait in a let annotation
printf '%s\n' 'trait T { val show: fn(self) -> string };' 'let v: T = 5;' \
  "$exit_line" 'v' > "$dir/f/main.gx"

for c in ctl a b c d e; do
  out=$(timeout -s KILL 60 "$GRAPHIX" --no-cache "$dir/$c/main.gx" 2>&1)
  echo "$c: $(printf '%s' "$out" | tail -n 1 | sed 's/^ *//')"
done
check=$(timeout -s KILL 60 "$GRAPHIX" --check "$dir/f/main.gx" 2>&1)
run=$(timeout -s KILL 60 "$GRAPHIX" --no-cache "$dir/f/main.gx" 2>&1)
echo "f: check: $(printf '%s' "$check" | tail -n 1 | sed 's/^ *//')"
echo "f: run:   $(printf '%s' "$run" | tail -n 1 | sed 's/^ *//')"
