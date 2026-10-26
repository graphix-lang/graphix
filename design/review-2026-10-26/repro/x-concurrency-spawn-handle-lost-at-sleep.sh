#!/bin/sh
# Title: a spawn whose reply has not landed when its arm sleeps is lost:
#        the child runs, the program never gets its handle
# Command: sh design/review-2026-10-26/repro/x-concurrency-spawn-handle-lost-at-sleep.sh
# Expected: the program's count of Child replies equals the number of
#           children spawned (each spawn's reply answers at the wake, as a
#           not-cancel-safe eval's does), "got 201 / spawned 201".
# Observed (HEAD e6ffc6eed, debug build): "got 1 / spawned 201": 200 children
#           ran with no handle reaching the program (orphans with the default
#           kill_on_drop false; killed unseen with kill_on_drop true).
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > "$D/p.gx" <<GXEOF
let n = 0;
n <- select n { k if k < 400 => k + 1, _ => never() };
let got = 0;
select n % 2 {
  0 => {
    let c = sys::process::spawn(n ~ sys::process::options(
      #args: ["-c", "echo x >> $D/spawn.log"],
      "sh"
    ));
    got <- c ~ got + 1
  },
  _ => never()
};
sys::exit(sys::time::after_idle(duration:1000.ms, select n { 400 => 0, _ => never() }));
got
GXEOF
got=$(timeout -s KILL 60 "$GX" --no-cache "$D/p.gx" 2>/dev/null | tail -1)
sleep 1
echo "got $got / spawned $(wc -l < "$D/spawn.log")"
rm -rf "$D"
