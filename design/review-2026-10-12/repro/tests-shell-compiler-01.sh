#!/bin/bash
# deep_nesting.rs: six of its shapes are ill-typed at every depth, so their
# check/run/nofusion cases stop at the first type error (exit 1 here) and
# never elaborate, fuse or run. Shapes copied from program() in
# graphix-shell/tests/deep_nesting.rs at depth accepted() = 1000/8 = 125.
G=${GRAPHIX:-graphix}
D=${1:-125}
dir=$(mktemp -d)
python3 - "$dir" "$D" <<'PY'
import sys
d, n = sys.argv[1], int(sys.argv[2])
shapes = {
  "slicepat": "let v = [1];\nlet x = select v { " + "["*n + "x" + "]"*n + " => 1, _ => 0 }",
  "tuplepat": "let v = (1, 1);\nlet x = select v { " + "(1, "*n + "x" + ")"*n + " => 1, _ => 0 }",
  "field": "let s = {a: 1};\nlet x = s" + ".a"*n,
  "index": "let a = [1];\nlet x = a" + "[0]"*n,
  "deref": "let v = 1;\nlet r = &v;\nlet x = " + "*"*n + "r",
  "qop": "let a = [1];\nlet x = a[0]" + "$"*n,
}
for k, v in shapes.items():
    open(f"{d}/{k}.gx", "w").write(v)
PY
for s in slicepat tuplepat field index deref qop; do
  echo "== $s: $(timeout -s KILL 60 $G --no-cache --check $dir/$s.gx 2>&1 | tail -1 | cut -c1-120)"
done
rm -rf "$dir"
