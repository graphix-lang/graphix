#!/bin/sh
# lang::attributes::tail_recursive_raise reads the first update of
# `select count(caught) { 4 => 4, _ => never() }`. `count(caught)` counts
# caught's birth fire too, and a first update can only be 4: the fixture
# passes for ANY raise count of three or more, not the four it claims.
#
# Command: sh design/review-2026-11-16/repro/tests-lang-rest-c-raise-count-unpinned.sh
# Expected (for the fixture to pin "one raise a pass"): only N=3 (four
#   activations, four raises) prints 4.
# Observed: N=2 (three raises), N=3, N=5 (six) and N=9 (ten) all print 4,
#   fused and node-walk alike. The exact form below (caught <- x ~ caught + 1,
#   read eight cycles in) prints the raise count itself: 3, 4, 6, 10.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
for n in 2 3 5 9; do
  cat > "$d/p.gx" <<EOF
let result = {
  let e: Result<i64, \`E> = error(\`E);
  let caught = 0;
  catch(x) caught <- x ~ 1;
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 {
    let k = e?;
    select n { 0 => acc + k, _ => f(n - 1, acc + n) }
  };
  let r = f($n, 0);
  select count(caught) { 4 => 4, _ => never() }
};
sys::exit(sys::time::after_idle(duration:200.ms, 0));
result
EOF
  cat > "$d/q.gx" <<EOF
let result = {
  let e: Result<i64, \`E> = error(\`E);
  let caught = 0;
  catch(x) caught <- x ~ caught + 1;
  #[tail_recursive]
  let rec f = |n: i64, acc: i64| -> i64 {
    let k = e?;
    select n { 0 => acc + k, _ => f(n - 1, acc + n) }
  };
  let r = f($n, 0);
  let k = 0;
  k <- select k { 8 => never(), _ => k + 1 };
  select k { 8 => caught, _ => never() }
};
sys::exit(sys::time::after_idle(duration:200.ms, 0));
result
EOF
  for flag in "" --no-fusion; do
    printf 'N=%s %s: fixture %s exact %s\n' "$n" "${flag:-fused}" \
      "$(timeout -s KILL 60 "$G" --no-cache $flag "$d/p.gx" 2>/dev/null | tail -1)" \
      "$(timeout -s KILL 60 "$G" --no-cache $flag "$d/q.gx" 2>/dev/null | tail -1)"
  done
done
rm -rf "$d"
