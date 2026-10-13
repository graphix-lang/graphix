#!/bin/sh
# Parse + check time of a nested fn type doubles per level (FnType::for_each_part
# re-walks the subtree through sig_tvars at every nested fn).
G=${G:-graphix}
for n in 16 18 20 22; do
  f=$(mktemp --suffix=.gx)
  python3 -I -c "n=$n; print('type T = ' + 'fn() -> '*n + 'i64;'); print('0')" > "$f"
  s=$(date +%s%N); timeout -s KILL 120 "$G" --check --no-cache "$f" >/dev/null 2>&1; e=$(date +%s%N)
  echo "depth $n: $(( (e - s) / 1000000 )) ms"
  rm -f "$f"
done
