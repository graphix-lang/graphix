#!/bin/sh
# x-fix-types-07: a constructed abstract value's parameters are decided by a
# wider annotation only at a top-level let initializer or a call argument;
# a return annotation, an array/tuple/struct element, select arms and `<-`
# refuse.
# run: sh design/review-2026-10-12/repro/x-fix-types-07.sh [path/to/graphix]
# expected: every case checks (exit 0), as `let b: Box<[i64, string]> = Box(1)` does
# observed (HEAD 7324207f): let, call, call-tuple check; ret, arr, tuple,
#   struct, sel, con are refused ("Box<[i64, string]> does not contain
#   Box<i64>"; con: "declare b: [Box<[i64, string]>, Box<i64>]")
G=${1:-graphix}
D=$(mktemp -d)
hdr='type Box<'"'"'a> = Abstract<'"'"'a>;'
case_() {
  printf '%s\n%s\n' "$hdr" "$2" > "$D/$1.gx"
  if out=$(timeout -s KILL 60 "$G" --check "$D/$1.gx" 2>&1); then
    echo "$1: ok"
  else
    echo "$1: REFUSED: $(echo "$out" | tail -1)"
  fi
}
case_ let   'let b: Box<[i64, string]> = Box(1); b'
case_ call  'let f = |b: Box<[i64, string]>| b; f(Box(1))'
case_ calltup 'let f = |b: (Box<[i64, string]>, i64)| b; f((Box(1), 2))'
case_ ret   'let g = || -> Box<[i64, string]> Box(1); g()'
case_ arr   'let b: Array<Box<[i64, string]>> = [Box(1)]; b'
case_ tuple 'let b: (Box<[i64, string]>, i64) = (Box(1), 2); b'
case_ struct 'let b: {a: Box<[i64, string]>} = {a: Box(1)}; b'
case_ sel   'let b: Box<[i64, string]> = select true { true => Box(1), false => Box("s") }; b'
case_ con   'let b: Box<[i64, string]> = never(); b <- Box(1); b'
rm -rf "$D"
