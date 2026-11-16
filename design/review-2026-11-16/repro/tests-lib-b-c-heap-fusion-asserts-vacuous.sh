#!/bin/sh
# Title: forked_cycle_heap's "partly fused" check `failed > 0` holds for a wholly fused program
#
# forked_heap_settles asserts `regions > 0 && failed > 0` for the "partly
# fused" case over the whole context's FusionStats. Every program its
# `program()` builds records failures of its own whatever fuses: the
# counter's `let n = 0`, its `n <- n + 1` and the sample `l ~ n`. The
# "fused" program (FLAT_MAP, leaf `n`), which fuses its recursion whole,
# records the same three, so `failed > 0` cannot tell "partly fused" from
# "fused". `#[native]` on the root call does: it accepts the fused program
# and refuses the partly fused one.
#
# Command: sh design/review-2026-11-16/repro/tests-lib-b-c-heap-fusion-asserts-vacuous.sh
# Expected (for `failed > 0` to mean "partly fused"): the fused program
#   records no failure.
# Observed: fused: fused=1 with 3 program failures ("a binding something
#   writes keeps its value at a wake", "a region of type _ has no kernel
#   return", "sample keeps cross-cycle state"; the two "module statement"
#   lines are graphix-fuzz's own wrapper); partly: fused=2 and the same three
#   plus "lambda `t` has no kernel: .. `uniq` ..". With `#[native]` on
#   `array::len(t(..))` the fused program builds and the partly fused one is
#   refused ("#[native] expression did not fully fuse to native code").

B=/home/eric/tmp/review-2026-11-16/bin
w=$(mktemp -d)
cat > "$w/fused.gx" <<'EOF'
{
  let n = 0;
  n <- n + 1;
  let rec t = |a: Array<i64>| -> Array<i64> select a {
    [] => [n],
    [h, rest..] => array::push(array::flat_map([rest, rest], t), h)
  };
  let l = array::len(t([1, 2, 3, 4, 5]));
  l ~ n
}
EOF
cat > "$w/partly.gx" <<'EOF'
{
  let n = 0;
  n <- n + 1;
  let rec t = |a: Array<i64>| -> Array<i64> select a {
    [] => [uniq(n)],
    [h, rest..] => {
      let join = |xs: Array<Array<i64>>, h: i64| array::push(array::flatten(xs), h);
      join(array::map([rest, rest], t), h)
    }
  };
  let l = array::len(t([1, 2, 3, 4, 5]));
  l ~ n
}
EOF
for f in fused partly; do
    echo "== $f: fusion stats"
    timeout -s KILL 170 "$B/graphix-fuzz" run "$w/$f.gx" 2>&1 | grep -E "fusion:|failed" | sort -u
    sed 's/let l = array::len/let l = #[native] array::len/' "$w/$f.gx" > "$w/${f}_native.gx"
    echo "== $f with #[native] on the root call (no output = accepted)"
    timeout -s KILL 60 "$B/graphix" --no-cache --expand "$w/${f}_native.gx" 2>&1 | tail -3
done
rm -rf "$w"
