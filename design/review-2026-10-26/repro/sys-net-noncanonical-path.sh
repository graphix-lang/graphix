#!/bin/sh
# Title: sys::net subscribe/write/publish compare a canonical stored Path with the raw
# new path string, so a non-canonical path ("/local/x/") that re-fires unchanged tears
# the subscription/target/publication down and re-creates it at every fire.
# Command: sh design/review-2026-10-26/repro/sys-net-noncanonical-path.sh
# Expected: every count is 1 (a static value, an unchanged path re-firing).
# Observed (HEAD e6ffc6eed, debug build):
#   subscribe /local/x -> 1     subscribe /local/x/ -> 11 (the value re-delivered per fire)
#   write     /local/w -> 1     write     /local/w/ -> 11 (the standing value written again per fire)
#   publish   /local/q -> (1,7) publish   /local/q/ -> (19,7) (unpublish + republish per fire)
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
for P in "/local/x" "/local/x/"; do cat > $D/s.gx <<EOT
sys::net::publish("/local/x", 7);
let t = sys::time::timer(duration:50.ms, true);
let p = t ~ "$P";
let n = 0;
let v: i64 = sys::net::subscribe(p)\$;
n <- v ~ n + 1;
sys::exit(sys::time::timer(duration:600.ms, false) ~ 0);
n
EOT
echo "subscribe $P -> $(timeout -s KILL 30 $G --no-cache $D/s.gx | tail -1)"; done
for P in "/local/w" "/local/w/"; do cat > $D/w.gx <<EOT
let writes = 0;
sys::net::publish(#on_write: |v: i64| writes <- v ~ writes + 1, "/local/w", 0);
let t = sys::time::timer(duration:50.ms, true);
sys::net::write(t ~ "$P", 5);
sys::exit(sys::time::timer(duration:600.ms, false) ~ 0);
writes
EOT
echo "write $P -> $(timeout -s KILL 30 $G --no-cache $D/w.gx | tail -1)"; done
for P in "/local/q" "/local/q/"; do cat > $D/p.gx <<EOT
let t = sys::time::timer(duration:50.ms, true);
sys::net::publish(t ~ "$P", 7);
let n = 0;
let v: [i64, Error<[\`SubscribeError(string), \`InvalidCast(string)]>] = sys::net::subscribe("/local/q");
n <- v ~ n + 1;
sys::exit(sys::time::timer(duration:600.ms, false) ~ 0);
(n, v)
EOT
echo "publish $P -> $(timeout -s KILL 30 $G --no-cache $D/p.gx | tail -1)"; done
rm -rf $D
