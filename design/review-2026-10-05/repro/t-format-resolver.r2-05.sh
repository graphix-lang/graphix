#!/usr/bin/env bash
# t-format-resolver.r2-05: the items of one `use` statement see each other
# in the parser's sorted order, so the names' alphabetical order decides
# whether a sibling sees a rename onto the statement's root.
#
# compile_use_items (graphix-compiler/src/node/module.rs:131) installs each
# item, in UseItem::sorted order (print::cmp_use_items), before
# Env::use_anchor resolves the next item's prefix through the scope's
# imports. `a::b` sorts before `a::c as a`, so `use a::{b, c as a}` takes b
# from `a`; `a::z` sorts after it, so `use a::{z, c as a}` takes z from
# `a::c`. A .gxi `use` is compiled twice into the same scope (bind_sig, then
# spliced into the body by add_interface_modules), and the second compile
# resolves the prefix through the first one's import (case 3).
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-format-resolver.r2-05.sh
#
# expected:
#   1. the two groups agree: (1, 1) if a group resolves against the scope
#      before it, (2, 2) under Rust's order-independent imports (rustc
#      1.98 prints 2 for `{ use a::{B, c as a}; B }` and for
#      `{ use a::{Z, c as a}; Z }` over the same module tree)
#   2. both groups compile, or both fail the same way
#   3. the .gxi `use` is accepted, as the same line is in the .gx
# observed (HEAD c722befe, debug build, the same with --no-fusion):
#   1. (1, 2)
#   2. fs: ok    time: `time::now not defined`
#   3. gxi: "`sys` is already imported here (from `sys`); rename one
#      (`use ... as ...`)"    gx: ok
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

# last <case> <args..>: the last non-blank line graphix prints
last() {
    local c=$1; shift
    (cd "$dir/$c" && timeout -s KILL 30 "$GRAPHIX" "$@" main.gx 2>&1) \
        | grep -v '^\s*$' | tail -1 | sed 's/^ *[0-9]*: //'
}

# 1. a sibling sorting before the rename keeps `a`, one sorting after
#    follows the rename into `a::c`
mk 1 a.gx 'mod c;
let b = 1;
let z = 1'
mk 1 a/c.gx 'let b = 2;
let z = 2'
mk 1 main.gx 'mod a;
let b = { use a::{b, c as a}; b };
let z = { use a::{z, c as a}; z };
let v = (b, z);
sys::exit(sys::time::after_idle(duration:100.ms, v ~ 0));
v'
echo "1. (b, z): $(last 1 --no-cache)"

# 2. the same with stdlib modules in one file: fs < net < time
mk 2fs main.gx 'let v = { use sys::{fs, net as sys}; fs::read_all("x") };
v'
mk 2time main.gx 'let v = { use sys::{time, net as sys}; time::now(1) };
v'
r_fs=$(last 2fs --check); r_time=$(last 2time --check)
echo "2. fs: ${r_fs:-ok}    time: ${r_time:-ok}"

# 3. a lone rename onto a package root, in the .gxi and in the .gx
mk 3gxi main.gx 'mod m;
m::v'
mk 3gxi m.gxi 'use sys::net as sys;
val v: i64;'
mk 3gxi m.gx 'let v = 1'
mk 3gx main.gx 'mod m;
m::v'
mk 3gx m.gxi 'val v: i64;'
mk 3gx m.gx 'use sys::net as sys;
let v = 1'
r_gxi=$(last 3gxi --check); r_gx=$(last 3gx --check)
echo "3. gxi: ${r_gxi:-ok}    gx: ${r_gx:-ok}"
