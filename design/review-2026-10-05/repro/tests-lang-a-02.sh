#!/usr/bin/env bash
# tests-lang-a-02: three fixtures sequence their events with wall-clock
# one-shot timers; a 30-100 ms stall of the runtime merges two timers
# into one cycle and flips each result.
#   tail_rebind_carries_bottom  (stdlib/graphix-tests/src/lang/functions.rs:1961)
#   select_sibling_binds_spent  (stdlib/graphix-tests/src/lang/select.rs:1885)
#   let_sibling_binds_spent     (stdlib/graphix-tests/src/lang/select.rs:2002)
# Each fixture below is the test's program unchanged, plus a first
# `println("ARMED")` (printed in the init cycle, the one that arms the
# timers) and a RESULT print and exit. The script runs each one twice:
# once untouched, once frozen with SIGSTOP/SIGCONT across the gap between
# its timers t1 and t2, as a descheduled or blocked runtime thread would be.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/tests-lang-a-02.sh
#
# expected (what the tests assert): tail (null, 7); sel (1, 1, 0); let (1, 1, 0)
# observed (HEAD c722befe, debug build):
#   tail  no freeze: (null, 7)    35 ms freeze from ~25 ms: (null, null)  [3/3]
#   sel   no freeze: (1, 1, 0)   120 ms freeze from ~40 ms: (1, 0, 1)     [2/2]
#   let   no freeze: (1, 1, 0)   120 ms freeze from ~40 ms: (1, 0, 1)     [2/2]
#   a 10 ms freeze outside the window leaves tail at (null, 7).
# Both answers are the language's: `<-` lands next cycle, so a `t2 ~ obs`
# in t1's cycle reads the old value. The fixtures only pass while t1 and t2
# land in different idle passes of the runtime (gx.rs:1029 drains every
# completed timer task into one cycle). A stall that freezes the runtime
# thread across 30 ms (tail) or 100 ms (sel, let) makes them fail.
set -u
GRAPHIX=${GRAPHIX:-graphix}
DIR=$(mktemp -d)
export XDG_CACHE_HOME=$DIR/cache

cat > "$DIR/tail.gx" <<'EOF'
println("ARMED");
let result = {
  let rec f = |n: i64, x: i64, k: i64| -> i64 select n {
    0 => x,
    _ => f(n - 1, select n { m if m == k => null$, _ => x }, k)
  };
  let k = 2;
  let t1 = sys::time::timer(duration:0.03s, false);
  k <- t1 ~ 9;
  let r = f(3, 7, k);
  let obs: [i64, null] = null;
  obs <- r;
  let t0 = sys::time::timer(duration:0.015s, false);
  let t2 = sys::time::timer(duration:0.06s, false);
  (t0 ~ obs, t2 ~ obs)
};
println("RESULT [result]");
sys::exit(sys::time::timer(duration:0.4s, false) ~ 0)
EOF

cat > "$DIR/sel.gx" <<'EOF'
println("ARMED");
let result = {
  type Ev = [`Key([`Enter, `Other]), `Mouse];
  let screen = 0;
  let fired = 0;
  let seen = 0;
  let keys = |k: [`Enter, `Other]| -> null select k {
    kk@ `Enter => { fired <- (kk ~ fired) + 1; null },
    `Other => null
  };
  let landing = |e: Ev| -> null select e {
    `Key(k) => select k {
      kk@ `Enter => { seen <- (kk ~ seen) + 1; null },
      `Other => null
    },
    `Mouse => null
  };
  let handle = |e: Ev| -> null select e {
    ev@ `Key(k) => select screen {
      0 => landing(ev),
      _ => keys(k)
    },
    `Mouse => null
  };
  let e: Ev = never();
  let out = handle(e);
  let t1 = sys::time::timer(duration:0.05s, false);
  e <- t1 ~ `Key(`Enter);
  let t2 = sys::time::timer(duration:0.15s, false);
  screen <- t2 ~ 1;
  let t3 = sys::time::timer(duration:0.3s, false);
  t3 ~ (screen, seen, fired)
};
println("RESULT [result]");
sys::exit(sys::time::timer(duration:0.6s, false) ~ 0)
EOF

cat > "$DIR/let.gx" <<'EOF'
println("ARMED");
let result = {
  let screen = 0;
  let seen = 0;
  let fired = 0;
  let pair: (i64, i64) = never();
  let (a, b) = pair;
  select screen {
    0 => seen <- a ~ (seen + 1),
    _ => fired <- b ~ (fired + 1)
  };
  let t1 = sys::time::timer(duration:0.05s, false);
  pair <- t1 ~ (1, 2);
  let t2 = sys::time::timer(duration:0.15s, false);
  screen <- t2 ~ 1;
  let t3 = sys::time::timer(duration:0.3s, false);
  t3 ~ (screen, seen, fired)
};
println("RESULT [result]");
sys::exit(sys::time::timer(duration:0.6s, false) ~ 0)
EOF

# run PROG DELAY STALL: DELAY s after ARMED, SIGSTOP the graphix process
# for STALL s (STALL=0: no freeze).
run() {
  local prog=$1 delay=$2 stall=$3 pid="" p line
  coproc G { exec timeout -s KILL 60 "$GRAPHIX" --no-cache "$prog" 2>&1; }
  for _ in $(seq 1 300); do
    for p in $(pgrep -f -- "$prog"); do
      [[ "$(basename "$(readlink "/proc/$p/exe" 2>/dev/null)")" == graphix ]] && pid=$p
    done
    [ -n "$pid" ] && break
    sleep 0.02
  done
  while IFS= read -r line <&"${G[0]}"; do
    case "$line" in
      ARMED*)
        if [ "$stall" != 0 ] && [ -n "$pid" ]; then
          sleep "$delay"; kill -STOP "$pid"; sleep "$stall"; kill -CONT "$pid"
        fi ;;
      RESULT*) echo "  $(basename "$prog" .gx) delay=$delay freeze=$stall: $line" ;;
    esac
  done
  wait
}

run "$DIR/tail.gx" 0 0
run "$DIR/tail.gx" 0.025 0.035
run "$DIR/tail.gx" 0 0.01
run "$DIR/sel.gx" 0 0
run "$DIR/sel.gx" 0.04 0.12
run "$DIR/let.gx" 0 0
run "$DIR/let.gx" 0.04 0.12
rm -rf "$DIR"
