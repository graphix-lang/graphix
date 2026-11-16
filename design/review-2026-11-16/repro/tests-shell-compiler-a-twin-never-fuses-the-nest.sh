#!/bin/bash
# deep_nesting's "twin" mode never puts the deep construct through the JIT.
#
# deep_nesting.rs::source wraps a shape's program as
#   let twin = |z| { <program>; z }; twin(1)
# and the header says this runs the program "as a called lambda's body
# (which fuses)". Every `let x = <deep>` shape leaves `x` unused, and the
# fused region drops it: the emitted CLIF is the same size at depth 10 and
# at depth 200. With the body's value `x` (or a leaf that is `z`) the CLIF
# grows with the depth, so it is the unused let, not the shape, that keeps
# the nest out of the emitter.
#
# Run:  bash design/review-2026-11-16/repro/tests-shell-compiler-a-twin-never-fuses-the-nest.sh
# Expected (if twin covered fusion): CLIF lines grow with depth for "as the test writes it".
# Observed (debug binary at e1be2fbf2):
#   array as the test writes it @10: 248 CLIF lines   @200: 248
#   array body value x          @10: 397              @200: 2487
#   parens as the test writes it @10: 248             @200: 248
#   parens body value x          @10: 465             @200: 4455
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
T=$(mktemp -d)
export XDG_CACHE_HOME=$T/cache
nest() { # shape depth
  case $1 in
    array) printf 'let x = %s1%s' "$(printf '[%.0s' $(seq $2))" "$(printf ']%.0s' $(seq $2))" ;;
    parens) printf 'let x = %s1%s' "$(printf '(1 + %.0s' $(seq $2))" "$(printf ')%.0s' $(seq $2))" ;;
  esac
}
for shape in array parens; do
  for ret in z x; do
    for d in 10 200; do
      printf 'let twin = |z: i64| {\n%s;\n%s\n};\nprintln(twin(1));\nsys::exit(0)\n' "$(nest $shape $d)" $ret > $T/p.gx
      n=$(GRAPHIX_DUMP_CLIF=1 timeout -s KILL 120 "$GX" --no-cache --no-netidx $T/p.gx 2>&1 | wc -l)
      label=$([ $ret = z ] && echo "as the test writes it" || echo "body value x")
      echo "$shape $label @$d: $n CLIF lines"
    done
  done
done
rm -rf "$T"
