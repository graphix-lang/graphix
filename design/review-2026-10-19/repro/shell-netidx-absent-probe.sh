#!/bin/sh
# Title: a program entry is written although module resolution consulted
#   netidx and found the module absent there (NetidxResolver records no
#   Probe on TryNextMethod), so a later netidx publication is not seen by
#   a warm start.
# Command: sh shell-netidx-absent-probe.sh
# Expected: no program entry (as for a module read from netidx: nothing on
#   disk vouches that netidx still lacks /mods/m.gx on the next start), or a
#   Netidx probe in the record.
# Observed: "Program image written ..." and the entry's GXDEPS record lists
#   only file probes (proj/m.gx absent, data/graphix/m.gx read); nothing
#   records that /mods/m.gx on netidx was consulted. A next start, with
#   /mods/m.gx now published, restores the data-dir m while a cold run
#   would compile netidx's.
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
mkdir -p $D/proj $D/data/graphix
printf 'let x = 1\n' > $D/data/graphix/m.gx
printf 'mod m;\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\nm::x\n' > $D/proj/main.gx
XDG_CACHE_HOME=$D/cache XDG_DATA_HOME=$D/data GRAPHIX_MODPATH='netidx:/mods' RUST_LOG=info \
  timeout -s KILL 60 $G --no-netidx --log-dir $D/log $D/proj/main.gx
grep "Program image written" $D/log/*.log
for f in $D/cache/graphix/registration/*/*.img; do head -c 2000 $f | strings | grep -E '^[+-]/' ; done
