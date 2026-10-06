#!/usr/bin/env bash
# t-env-07: `use super::*` in a module declared inside a block or a
# lambda body reports a shadowed name as ambiguous.
#
# compile_use_item (graphix-compiler/src/node/module.rs:56-63) registers
# one `super::*` whose anchor is a block level as one glob source per
# chain level, and Env::lookup_at (graphix-types/src/env.rs:713-725)
# treats glob sources as rivals. A name declared at an inner level and
# again at an outer one (ordinary shadowing) is found by two "globs" of
# the one `use`, while `use super::x` walks the chain innermost-first
# and takes the inner name. The outer name is not nameable from the
# module at all (`super::super` climbs above the root), so the advice
# "import one explicitly" has nothing to choose between.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-env-07.sh
#
# expected: each case prints the same value for `glob` and `explicit`:
#   1. block shadows a file-level let:   2
#   2. lambda parameter shadows a let:   5
#   3. block shadows a file-level type:  "hello"
# observed (HEAD c722befe, debug build):
#   1. glob: "`x` is ambiguous: both `#do..::#do..` and `#do..` provide it;
#      import one explicitly" (under --check the second scope prints as
#      ``, the root)                      explicit: 2
#   2. glob: "`x` is ambiguous: both `#do..::#fn..` and `#do..` provide
#      it; import one explicitly"         explicit: 5
#   3. glob: "undefined type T in #do..::#do..::inner" (the ambiguity is
#      only in the warn log)              explicit: "hello"
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

# case <name> <main.gx> <inner.gx after its use>: run with `use super::*`
# and with `use super::<import>`
case_() {
    local name=$1 main=$2 import=$3 body=$4
    for v in glob explicit; do
        local d="$dir/$name/$v"
        mkdir -p "$d"
        printf '%s\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nr\n' \
            "$main" > "$d/main.gx"
        if [ "$v" = glob ]; then use='use super::*;'; else use="use super::$import;"; fi
        printf '%s\n%s\n' "$use" "$body" > "$d/inner.gx"
        printf '%-24s %-9s ' "$name" "$v"
        (cd "$d" && timeout -s KILL 30 "$GRAPHIX" --no-cache main.gx 2>&1) \
            | grep -v '^\s*$' | tail -n 1 | sed 's/^ *[0-9]*: *//'
    done
}

case_ 1-block-let \
'let x = 1;
let r = {
  let x = 2;
  mod inner;
  inner::y
};' \
x 'let y = x'

case_ 2-lambda-param \
'let x = 1;
let f = |x| {
  mod inner;
  inner::y
};
let r = f(5);' \
x 'let y = x'

case_ 3-block-type \
'type T = i64;
let r = {
  type T = string;
  mod inner;
  inner::y
};' \
T 'let y: T = "hello"'
