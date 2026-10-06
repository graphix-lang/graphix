#!/usr/bin/env bash
# x-image-02: lazily restored instances register no reads, so wake
# catch-up loses fires on warm starts.
#
# CallSite::image_decode (graphix-compiler/src/node/callsite.rs:1886)
# keeps a program image's statically bound instance as Callee::Imaged and
# registers none of the body's reads with the runtime; they are
# registered only when materialize decodes the body at the site's first
# dispatch. Cold, the instance exists before the first cycle and its
# reads are registered (CLAUDE.md: "Registration covers every node a
# statement holds, selected or not ... which wake catch-up relies on").
# So a variable that only the body of a call in a sleeping select arm
# (or an unentered seq step) reads, written alone in its cycle, schedules
# nothing warm: TrackedFires::observe never sees the fire and the woken
# arm gets no catch-up. GRAPHIX_DBG_VARS=1 shows `REF_VAR <g> by <root>`
# before the first cycle cold, and only after `SET_VAR <sel> = 1` warm.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-image-02.sh
#
# Each program runs with --no-cache, then cold and warm over a fresh
# XDG_CACHE_HOME. The design requires a hit to be indistinguishable from
# a cold compile (design/program_image.md, Correctness).
#
# expected: every mode alike, the cold output:
#   select:  "A" then "1 1"   (g fired while the `_` arm slept: one catch-up)
#   seq:     "got: pong"
#   control: "A" then "1 1"
# observed (HEAD c722befe, debug build; same with --no-fusion and with
# GRAPHIX_PAR=off or force):
#   select:  no-cache "A" "1 1", cold "A" "1 1", warm "A" only
#   seq:     no-cache "got: pong", cold "got: pong", warm nothing (the
#            seq never completes)
#   control: (select plus `let other = g;`, a second reader the root
#            registers) "A" "1 1" in every mode
# graphix-fuzz check reports AGREE on the select program: its trace of a
# timer program ends at cycle 0.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT

cat > "$dir/select.gx" <<'GX'
let to_b = sys::time::timer(duration:300.ms, false);
let sel = 0;
sel <- to_b ~ 1;
let gt = sys::time::timer(duration:150.ms, false);
let g: i64 = never();
g <- gt ~ 42;
let f = |x: i64| -> string "[x] [count(g)]";
let r = select sel { 0 => "A", _ => f(sel) };
sys::exit(sys::time::timer(duration:700.ms, false) ~ 0);
r
GX

cat > "$dir/seq.gx" <<'GX'
let go = sys::time::timer(duration:50.ms, false);
let ready = false;
ready <- sys::time::timer(duration:300.ms, false) ~ true;
let reply: string = never();
reply <- sys::time::timer(duration:150.ms, false) ~ "pong";
let await_reply = |tag: string| -> string "[tag]: [reply ~ reply]";
let r = seq go {
  until ready;
  await_reply("got")
};
sys::exit(sys::time::timer(duration:700.ms, false) ~ 0);
r
GX

sed 's/^let f = /let other = g;\nlet f = /' "$dir/select.gx" > "$dir/control.gx"

for p in select seq control; do
    echo "=== $p"
    export XDG_CACHE_HOME="$dir/cache_$p"
    mkdir -p "$XDG_CACHE_HOME"
    for mode in no-cache cold warm; do
        flag=()
        [ "$mode" = no-cache ] && flag=(--no-cache)
        out=$(timeout -s KILL 30 "$GRAPHIX" "${flag[@]}" "$dir/$p.gx" 2>/dev/null | tr '\n' ' ')
        echo "$mode: ${out:-<nothing>}"
    done
done
