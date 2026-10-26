#!/bin/sh
# t-misc: a warm image start replays a parallel module run's warnings in
# task completion order, not the program order a cold run prints them in.
# Command: sh t-misc-warning-order-image.sh  (uses $GRAPHIX, default the review binary)
# Expected: the warm run prints the a.gx warnings, then the b.gx ones, as the cold run does.
# Observed: cold prints a.gx (line 603) then b.gx (line 3); warm prints b.gx first.
# Cause: Env::publish_warning appends to Env::recorded when the task warns,
# while IdeMode::Warnings holds the printed copy until the task's join.
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d); cd "$d" || exit 1
printf 'mod a;\nmod b;\nsys::exit(sys::time::after_idle(duration:100.ms, 0));\na::v + b::v\n' > main.gx
tail='let f = |t: i64| {
  let x = 0;
  select t { 1 => x <- 5, _ => x <- 6 };
  x
};
let v = f(1)'
python3 -c "
for i in range(300):
    print(f'let g{i} = |a: i64, b: i64| select a > b {{ true => [a, b, a + b], false => [b, a, a * b] }};')
    print(f'let h{i} = g{i}(1, 2);')
" > a.gx
echo "$tail" >> a.gx
echo "$tail" > b.gx
printf 'val v: i64;\n' > a.gxi; cp a.gxi b.gxi
export XDG_CACHE_HOME=$d/cache
echo "== cold"; timeout -s KILL 120 "$GRAPHIX" main.gx 2>&1 | grep -o 'line: [0-9]*, column: [0-9]* in file [^ ]*/[ab].gx'
echo "== warm"; timeout -s KILL 120 "$GRAPHIX" main.gx 2>&1 | grep -o 'line: [0-9]*, column: [0-9]* in file [^ ]*/[ab].gx'
rm -rf "$d"
