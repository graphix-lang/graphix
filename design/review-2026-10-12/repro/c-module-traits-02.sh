#!/bin/sh
# c-module-traits-02: inside write to an exported val is lost when an outside
# write lands the same cycle.
#
# m.gx writes its own `x <- t ~ 1`; main writes `m::x <- m::t ~ 100`. Both fire
# in the cycle `t` lands, so the inner and the interface binding of `x` each get
# a write in the same later cycle. Module::update copies the outer event over the
# inner one (the Proxy loop before the body runs), so the inner write of 1 is
# discarded. flat.gx is the same program without a module.
#
# usage: sh c-module-traits-02.sh [path/to/graphix]
# expected: main: 0 1 100 (or 0 100 1)  -- one variable, the second write queues
#           flat: 0 1 100
# observed: main: 0 100                 -- the write of 1 never appears
#           flat: 0 1 100
G=${1:-graphix}
D=$(mktemp -d)
cat > $D/m.gx <<'GX'
let t = never();
let x = 0;
x <- t ~ 1
GX
cat > $D/m.gxi <<'GX'
val x: i64;
val t: i64;
GX
cat > $D/main.gx <<'GX'
mod m;
let clock = sys::time::timer(duration:100.ms, false);
m::t <- clock ~ 5;
m::x <- m::t ~ 100;
sys::exit(sys::time::timer(duration:400.ms, false) ~ 0);
m::x
GX
cat > $D/flat.gx <<'GX'
let t = never();
let x = 0;
x <- t ~ 1;
let clock = sys::time::timer(duration:100.ms, false);
t <- clock ~ 5;
x <- t ~ 100;
sys::exit(sys::time::timer(duration:400.ms, false) ~ 0);
x
GX
echo "== main (module)"; (cd $D && timeout -s KILL 30 $G --no-cache main.gx)
echo "== flat";          (cd $D && timeout -s KILL 30 $G --no-cache flat.gx)
rm -rf $D
