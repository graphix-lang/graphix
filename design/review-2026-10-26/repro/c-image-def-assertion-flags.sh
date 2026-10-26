#!/bin/sh
# Title: a restored #[pure] assertion loses its compile flags and its `warned` bit
# (registration.rs restore_tables decodes DefAssertion with flags empty, warned false).
# Run: sh c-image-def-assertion-flags.sh   (runs each program cold then warm)
# Expected: the warm run reports exactly what the cold run does.
# Observed:
#  (a) pure4.gx, no flags: cold prints the `#[pure] ... dbg` WARNING once; warm prints it
#      twice (once from ProgramRoot::warnings, again at the runtime bind of `h`).
#  (b) pure3.gx with `-W error`: cold reports the runtime bind's warning as an ERROR
#      ("at: line: 2 ... #[pure]: ..."); warm prints it as a plain WARNING.
G=/home/eric/tmp/review-2026-10-26/bin/graphix
D=$(mktemp -d)
cat > $D/pure4.gx <<'GX'
#[pure]
let f = |x: i64| dbg(x) + 1;
let g = |x: i64| x + 2;
let h = g;
h <- sys::time::after_idle(duration:10.ms, f);
sys::exit(sys::time::after_idle(duration:200.ms, 0));
(f(5), h(1))
GX
cat > $D/pure3.gx <<'GX'
#[pure]
let f = |x: i64| dbg(x) + 1;
let g = |x: i64| x + 2;
let h = g;
h <- sys::time::after_idle(duration:10.ms, f);
sys::exit(sys::time::after_idle(duration:200.ms, 0));
h(1)
GX
for i in cold warm; do echo "== (a) $i"; XDG_CACHE_HOME=$D/c4 timeout -s KILL 30 $G $D/pure4.gx 2>&1 >/dev/null | grep -c WARNING; done
for i in cold warm; do echo "== (b) $i"; XDG_CACHE_HOME=$D/c3 timeout -s KILL 30 $G -W error $D/pure3.gx 2>&1 >/dev/null | grep -E "^(WARNING|at:)" | cut -c1-40; done
