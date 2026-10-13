#!/bin/sh
# collections-str-02: str::sprintf panics (runtime dies) when a %.Ns
# precision splits a multibyte char.
# Run: sh design/review-2026-10-12/repro/collections-str-02.sh <graphix> <graphix-fuzz>
# Expected: top.gx prints "💖|" (precision cut back to a char boundary) or a FormatError;
#   the program exits 0.
# Observed (HEAD 7324207f, debug build):
#   node-walk and default: "panicked at fish-printf-0.2.1/src/printf_impl.rs:520:19:
#   end byte index 3 is not a char boundary", then "Error: runtime did not
#   respond", exit 1.
#   graphix-fuzz run on the lambda form: same panic (raised in the node-walk
#   mode, CallSite -> CachedArgs<SprintfEv> -> fc_sprintf -> fish_printf),
#   then "Aborted (core dumped)", exit 134.
# (collections-str-02.gx in this directory is an unrelated array::group repro.)
G=${1:-graphix}; F=${2:-graphix-fuzz}
D=$(mktemp -d)
cat > "$D/top.gx" <<'GX'
sys::exit(sys::time::after_idle(duration:100.ms, 0));
str::sprintf("%.3s|", "💖💖💖💖")
GX
cat > "$D/fused.gx" <<'GX'
let f = |s: string| -> string str::sprintf("%.1s|", s)$;
f("é")
GX
timeout -s KILL 30 "$G" --no-cache --no-fusion "$D/top.gx"; echo "node-walk exit $?"
timeout -s KILL 30 "$G" --no-cache "$D/top.gx"; echo "default exit $?"
timeout -s KILL 120 "$F" run "$D/fused.gx"; echo "fuzz run exit $?"
rm -rf "$D"
