#!/bin/sh
# x-builtin-effects-03: aborting read_exact / write_exact when their select
# arm sleeps loses (read) or duplicates (write) stream bytes.
#
# Command: GRAPHIX=/path/to/graphix sh design/review-2026-10-12/repro/x-builtin-effects-03.sh
# Expected:
#   read_exact: abcdef
#   bytes sent: 192000 child counted: 192000
# Observed (HEAD 7324207f, both engines, --no-fusion too):
#   read_exact: defghi                              ("abc" was consumed by the aborted read and dropped)
#   bytes sent: 192000 child counted: 257536        (the 64 KiB the aborted write_all pushed, then all 192000 again)
# Controls: with `on` always true the same programs print abcdef and 192000.
GRAPHIX=${GRAPHIX:-graphix}
D=$(mktemp -d)
cat > $D/read.gx <<'GX'
use sys::process::{options, spawn, stdio};
use sys::io::Read;
let child = spawn(options(
  #args: ["-c", "printf abc; sleep 0.4; printf def; sleep 0.4; printf ghi"],
  #stdio: stdio(#stdout: `Pipe),
  "sh"
))$;
let out = child.stdout$;
let clk = sys::time::timer(duration:100.ms, true);
let n = 0;
n <- clk ~ n + 1;
let on = uniq(n < 2 || n > 6);
let got = select on { true => Read::read_exact(out, u64:6), false => never() };
println("read_exact: [buffer::to_string(got$)]");
sys::exit(got ~ 0)
GX
cat > $D/write.gx <<'GX'
use sys::process::{options, spawn, stdio};
use sys::io::{Read, Write, Close};
let child = spawn(options(
  #args: ["-c", "sleep 0.6; wc -c"],
  #stdio: stdio(#stdin: `Pipe, #stdout: `Pipe),
  "sh"
))$;
let inp = child.stdin$;
let out = child.stdout$;
let data = buffer::from_string(str::concat(array::init(2000, |_| "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")));
let clk = sys::time::timer(duration:100.ms, true);
let n = 0;
n <- clk ~ n + 1;
let on = uniq(n < 2 || n > 3);
let wrote = select on { true => Write::write_exact(inp, data), false => never() };
let closed = Close::close(wrote ~ inp);
let count = Read::read_all(closed ~ out)$;
println("bytes sent: [buffer::len(data)] child counted: [buffer::to_string(count)]");
sys::exit(count ~ 0)
GX
timeout -s KILL 30 $GRAPHIX --no-cache $D/read.gx
timeout -s KILL 30 $GRAPHIX --no-cache $D/write.gx
rm -rf $D
