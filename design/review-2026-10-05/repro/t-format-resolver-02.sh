#!/usr/bin/env bash
# t-format-resolver-02: fmt's use merge reorders a glob and a same-root
# item, changing what a path resolves to.
#
# merge_uses' depends (graphix-types/src/expr/format.rs:129-141) calls a
# pair independent whenever either item is a glob and both share a root,
# and reads() never sees a keyword-rooted item's second segment.
# compile_use_item (graphix-compiler/src/node/module.rs:51) resolves each
# item's prefix through the imports and globs made before it, so the sort
# inside the merged statement (a glob after a name at the same depth,
# then segment order) changes what a prefix names. format_source's
# reparse guard compares the merged input with the merged output, so it
# cannot see the change: the file is written and fmt exits 0.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-format-resolver-02.sh
#
# expected: every case prints the same value before and after
# `graphix fmt` (the statements stay apart, or fmt refuses).
# observed (HEAD c722befe, debug build):
#   1-glob-then-rename-to-root  before: 1  after: 2
#       { use a::*; use a::x as a; v }  ->  { use a::{x as a, *}; v }
#   2-glob-binds-the-root       before: 2  after: 1
#       { use a::*; use a::w; w }  ->  { use a::{w, *}; w }
#   3-rename-then-later-glob    before: 2  after: 1
#       { use a::z as a; use a::b::*; v }  ->  { use a::{b::*, z as a}; v }
#   4-reader-then-later-glob    before: 1  after: 2
#       { use a::z; use a::b::*; z }  ->  { use a::{b::*, z}; z }
#   5-self-glob-then-self-path  before: 7  after: use: no module `self::m` in scope
#       use self::r::*; use self::m::x;  ->  use self::{m::x, r::*};
#   6-self-rename-then-self-path (no glob)  before: 7  after: the same error
#       use self::q as m; use self::m::x;  ->  use self::{m::x, q as m};
#   7-package-rename-then-path (no glob, a script's top level)  before: 7
#       after: use: no module `package::m` in scope
#       use package::q as m; use package::m::x;  ->  use package::{m::x, q as m};
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

# mk <case> <file> <text>: write one file of a case's module tree
mk() {
    mkdir -p "$(dirname "$dir/$1/$2")"
    printf '%s\n' "$3" > "$dir/$1/$2"
}

# main <case> <expr>: a main.gx that prints <expr> and exits
main() {
    mk "$1" main.gx "mod a;
let r = $2;
println(r);
sys::exit(sys::time::after_idle(duration:100.ms, 0))"
}

run() {
    (cd "$dir/$1" && timeout -s KILL 30 "$GRAPHIX" --no-cache main.gx 2>&1) \
        | grep -v '^\s*$' | tail -1 | sed 's/^ *[0-9]*: //'
}

# check <case> <file>: run, format <file> in place, run again
check() {
    local before after
    before=$(run "$1")
    (cd "$dir/$1" && timeout -s KILL 30 "$GRAPHIX" fmt "$2") || echo "fmt refused"
    after=$(run "$1")
    printf '%-30s before: %-4s after: %s\n' "$1" "$before" "$after"
    grep 'use ' "$dir/$1/$2" | sed 's/^/    formatted: /'
}

mk 1-glob-then-rename-to-root a.gx 'mod x;
let v = 1'
mk 1-glob-then-rename-to-root a/x.gx 'let v = 2'
main 1-glob-then-rename-to-root '{ use a::*; use a::x as a; v }'
check 1-glob-then-rename-to-root main.gx

mk 2-glob-binds-the-root a.gx 'mod a;
let w = 1'
mk 2-glob-binds-the-root a/a.gx 'let w = 2'
main 2-glob-binds-the-root '{ use a::*; use a::w; w }'
check 2-glob-binds-the-root main.gx

mk 3-rename-then-later-glob a.gx 'mod z;
mod b;
let u = 0'
mk 3-rename-then-later-glob a/z.gx 'mod b;
let u = 0'
mk 3-rename-then-later-glob a/z/b.gx 'let v = 2'
mk 3-rename-then-later-glob a/b.gx 'let v = 1'
main 3-rename-then-later-glob '{ use a::z as a; use a::b::*; v }'
check 3-rename-then-later-glob main.gx

mk 4-reader-then-later-glob a.gx 'mod b;
let z = 1'
mk 4-reader-then-later-glob a/b.gx 'mod a;
let q = 0'
mk 4-reader-then-later-glob a/b/a.gx 'let z = 2'
main 4-reader-then-later-glob '{ use a::z; use a::b::*; z }'
check 4-reader-then-later-glob main.gx

mk 5-self-glob-then-self-path a.gx 'mod r;
use self::r::*;
use self::m::x;
let u = x'
mk 5-self-glob-then-self-path a/r.gx 'mod m;
let z = 0'
mk 5-self-glob-then-self-path a/r/m.gx 'let x = 7'
main 5-self-glob-then-self-path 'a::u'
check 5-self-glob-then-self-path a.gx

mk 6-self-rename-then-self-path a.gx 'mod q;
use self::q as m;
use self::m::x;
let u = x'
mk 6-self-rename-then-self-path a/q.gx 'let x = 7'
main 6-self-rename-then-self-path 'a::u'
check 6-self-rename-then-self-path a.gx

mk 7-package-rename-then-path q.gx 'let x = 7'
mk 7-package-rename-then-path main.gx 'mod q;
use package::q as m;
use package::m::x;
println(x);
sys::exit(sys::time::after_idle(duration:100.ms, 0))'
check 7-package-rename-then-path main.gx
