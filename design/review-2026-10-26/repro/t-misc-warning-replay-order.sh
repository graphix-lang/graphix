#!/bin/sh
# Title: a warm image start replays a run's warnings in task-completion order,
#        not the program order a cold run printed them in.
# Command: sh design/review-2026-10-26/repro/t-misc-warning-replay-order.sh
# Expected: both runs print the warnings in program order: a a b b
# Observed (HEAD e6ffc6eed, debug build):
#   cold: warn/a.gx warn/a.gx warn/b.gx warn/b.gx
#   warm: warn/b.gx warn/b.gx warn/a.gx warn/a.gx
# Cause: Env::publish_warning pushes into Env::recorded when the warning is
# issued (inside a parallel module-check task), while IdeMode::Warnings holds
# the printed copy until the task joins in program order.
GX=${GX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
mkdir -p $D/warn
cd $D/warn
cat > test.gx <<'G'
mod a;
mod b;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
a::v + b::v
G
echo 'val v: i64;' > a.gxi
echo 'val v: i64;' > b.gxi
python3 - <<'P'
lines = []
for i in range(400):
    lines.append(f"let h{i} = |x: i64| -> i64 {{ let q = x + {i}; select q {{ 0 => q * 2, n => n + 1 }} }};")
lines.append("let f = |x: i64| -> i64 { let y = 0; select x { 0 => y <- 1, _ => y <- 2 }; y };")
lines.append("let v = f(1) + h1(1)")
open("a.gx", "w").write("\n".join(lines) + "\n")
P
echo 'let g = |x: i64| -> i64 { let z = 0; select x { 0 => z <- 3, _ => z <- 4 }; z };
let v = g(1)' > b.gx
export XDG_CACHE_HOME=$D/cache
for run in cold warm; do
  printf '%s: ' $run
  timeout -s KILL 90 $GX test.gx 2>&1 >/dev/null | grep -o 'warn/[ab].gx' | tr '\n' ' '
  echo
done
rm -rf $D
