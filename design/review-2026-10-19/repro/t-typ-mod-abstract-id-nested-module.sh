#!/bin/sh
# Title: one module file's abstract type has one identity under two function
#        bodies but another at the top level (AbstractId::of vs ::declared spell
#        the module path differently).
# Command: sh design/review-2026-10-19/repro/t-typ-mod-abstract-id-nested-module.sh
# Expected: both programs agree: (3) and (4, 4), or (-1) and (-1, 4), by one rule
#           ("a module's type is its path without the generated components").
# Observed: two_bodies prints 3 (one type), top_and_body prints (-1, 4) (two types).
GX=${GX:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d)
cat > "$d/m.gx" <<'X'
type T = Abstract<i64>;
let mk = |x: i64| -> T T(x);
let get = |t: T| -> i64 t.0
X
cat > "$d/two_bodies.gx" <<'X'
let f = |y: i64| -> Any {
  mod m;
  m::mk(y)
};
let g = |v: Any| -> i64 {
  mod m;
  select v {
    m::T as t => m::get(t),
    _ => -1
  }
};
g(f(3))
X
cat > "$d/top_and_body.gx" <<'X'
mod m;
let g = |v: Any| -> i64 {
  mod m;
  select v {
    m::T as t => m::get(t),
    _ => -1
  }
};
let v: Any = m::mk(4);
(g(v), select v { m::T as t => m::get(t), _ => -1 })
X
timeout -s KILL 10 "$GX" --no-cache "$d/two_bodies.gx"
timeout -s KILL 10 "$GX" --no-cache "$d/top_and_body.gx"
rm -rf "$d"
