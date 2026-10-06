#!/usr/bin/env bash
# t-typ-mod-01: the contractiveness check matches the written name, so a
# renamed import of the type itself bypasses it (unsound accept, checker hang).
#
# Type::reaches_unguarded (graphix-types/src/typ/mod.rs:1905-1910) decides that
# a ref returns to the definition being registered by comparing
# (canonical scope, basename of the name AS WRITTEN) with (scope, name). After
# `use self::T as V`, V resolves to T's own definition but its written name is
# `V`, so the check never matches, expands T's body until UNGUARDED_DEPTH and
# answers "guarded". RefHist::ref_id keys refs by definition identity
# (def_key), so contains' coinductive memo then accepts anything against T.
# A rename anywhere in a cycle does it: `use self::U as W; type T = [i64, W];
# type U = [string, T]` is accepted, while the same without the rename is
# refused at U.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-typ-mod-01.sh
#
# expected (CLAUDE.md: "A typedef must be contractive ... `type T = [i64, T]`
#   is refused at Env::deftype"): cases 2-5 refused at `type T` with
#   "recursive type T refers back to itself through unions and aliases alone",
#   exactly as the control (case 1) is.
# observed (HEAD c722befe, debug build):
#   1. control `type T = [i64, T]`: refused (exit 1)
#   2. `use self::T as V; type T = [i64, V]` + `let n: i64 = s` with
#      s: T = "hello": --check exit 0
#   3. the same program run: the runtime panics at fusion/kernel.rs:243
#      "kernel param `n`: runtime String("hello") does not match the compiled
#      Scalar(I64) slot", then "Error: runtime did not respond" (exit 1)
#   4. the same with --no-fusion: x + 1 runs on x = "hello" (an arith error
#      at a site typed i64)
#   5. `use self::T as V; type T = V; let y: T = 1`: --check never returns
#      (killed by the 10 s timeout); the run hangs the same way. A backtrace
#      shows StructPatternNode::compile_int_inner looping on lookup_ref
#      (graphix-compiler/src/node/pattern.rs:436).
# The probes are modules because a script's top-level `use self::T as V` is
# refused by the run path ("use: no `T` in ``") though --check accepts it.
set -u
G=${GRAPHIX:-graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
export XDG_CACHE_HOME="$D/cache"

mkproj() { # dir, a.gx body
    mkdir -p "$D/$1"
    printf 'mod a;\na::r\n' > "$D/$1/main.gx"
    printf '%s\n' "$2" > "$D/$1/a.gx"
}

mkproj ctl 'type T = [i64, T];
let s: T = "hello";
let n: i64 = s;
let inc = |x: i64| x + 1;
let r = inc(n)'

mkproj ren 'use self::T as V;
type T = [i64, V];
let s: T = "hello";
let n: i64 = s;
let inc = |x: i64| x + 1;
let r = inc(n)'

mkproj hang 'use self::T as V;
type T = V;
let r: T = 1'

echo "== 1. control, --check"
timeout -s KILL 30 "$G" --check "$D/ctl/main.gx" 2>&1 | tail -1
echo "== 2. renamed self-import, --check"
timeout -s KILL 30 "$G" --check "$D/ren/main.gx"; echo "exit=$?"
echo "== 3. renamed self-import, run"
timeout -s KILL 10 "$G" --no-cache "$D/ren/main.gx"; echo "exit=$?"
echo "== 4. renamed self-import, run --no-fusion (killed after 5 s)"
timeout -s KILL 5 "$G" --no-cache --no-fusion "$D/ren/main.gx" 2>&1 | head -2
echo "== 5. renamed self-import as the whole body, --check (killed after 10 s)"
timeout -s KILL 10 "$G" --check "$D/hang/main.gx"; echo "exit=$? (137 = killed by the timeout)"
