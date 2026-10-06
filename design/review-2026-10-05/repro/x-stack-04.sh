#!/usr/bin/env bash
# x-stack-04: module resolution's poll recursion is unguarded. Each nested
# `mod` (and each expression level above it) adds four or five poll frames
# (Expr::resolve_modules_int -> resolve_children -> TryJoinAll ->
# TryMaybeDone -> resolve_modules_int, graphix-types/src/expr/resolver.rs)
# on the tokio worker's 2 MiB stack, with no crate::stack::ensure_sufficient.
# Every file is parsed on its own, so the parser's nesting limit does not
# bound the depth across files: a chain of nested module files aborts
# the process.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-stack-04.sh [N] [B]
#   N  nested module files (default 600): top.gx has `mod m;`, m.gx has
#      `let x = 1; mod m`, m/m.gx the same, ..., the last is `let x = 1`
#   B  when given, every file but the last instead nests its `mod m; m::x`
#      in B blocks `{ let a = 1; .. }` (330 is about the parser's limit)
#
# expected: "exit 0" (the chain checks; m::x is i64), never an abort.
# observed (HEAD c722befe):
#   debug (dev profile) build: N=400 exit 0; N=500 exit 134,
#     "thread 'tokio-rt-worker' has overflowed its stack /
#      fatal runtime error: stack overflow, aborting"
#   quick (opt-level 3) build of 10-04 23:03 (resolver.rs as at HEAD):
#     N=500 exit 0; N=550 exit 134, same message
#   N=2 B=330: exit 0; N=3 B=330 (three files nest 330 blocks each, every
#     one within the parser's limit): exit 134 on both builds
#   gdb: 1900-3260 frames of resolve_modules_int / TryJoinAll::poll /
#     TryMaybeDone::poll under GX::process_input_batch on tokio-rt-worker
#   a run (no --check) aborts the same way, and so does `graphix lsp` when
#   an editor opens top.gx of the N=500 tree (the server exits 134).
set -u
GRAPHIX=${GRAPHIX:-graphix}
N=${1:-600}
B=${2:-}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
nest() {
    local s='{ mod m; m::x }' i
    for ((i = 0; i < B; i++)); do s="{ let a = 1; $s }"; done
    printf '%s' "$s"
}
gate='sys::exit(sys::time::after_idle(duration:100.ms, 0));'
if [ -z "$B" ]; then
    printf 'mod m;\n%s\nm::x\n' "$gate" > "$D/top.gx"
else
    printf 'let r = %s;\n%s\nr\n' "$(nest)" "$gate" > "$D/top.gx"
fi
d=$D
for ((i = 1; i <= N; i++)); do
    if ((i == N)); then
        printf 'let x = 1\n' > "$d/m.gx"
    elif [ -z "$B" ]; then
        printf 'let x = 1;\nmod m\n' > "$d/m.gx"
    else
        printf 'let x = %s\n' "$(nest)" > "$d/m.gx"
    fi
    mkdir -p "$d/m"
    d=$d/m
done
cd "$D" || exit 1
"$GRAPHIX" --check top.gx > out.txt 2>&1
rc=$?
tail -n 3 out.txt
echo "exit $rc"
