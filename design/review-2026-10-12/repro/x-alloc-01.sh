#!/bin/sh
# x-alloc-01: every cycle walks every slot of every live collection, even
# when nothing the collection reads fired. An unrelated counter's 300
# cycles cost O(slots) each: compare SZ=2000 with SZ=20000 (fusion on;
# the callback holds `once`, so it does not fuse whole).
GX=${GX:-graphix}
for SZ in 2000 20000; do
  f=$(mktemp --suffix=.gx)
  cat > "$f" <<GX
let big = array::map(array::init($SZ, |i| i), |i| once(i) + 1);
let n = 0;
n <- select n < 300 { true => n + 1, false => never() };
sys::exit(select n { x if x == 300 => 0, _ => never() });
n
GX
  s=$(date +%s.%N); timeout -s KILL 60 $GX --no-cache "$f" >/dev/null 2>&1; e=$(date +%s.%N)
  echo "slots=$SZ 300 unrelated cycles: $(echo "$e - $s" | bc) s"
  rm -f "$f"
done
