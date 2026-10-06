#!/bin/bash
# x-node-contract-06: delete leaves store/env entries behind: ByRef cell,
# Catch bind, generated binds, builtin delivery ids.
#
# Every program below rebuilds one array::map slot on odd cycles and
# deletes it on even ones (`n <- n + 1` runs a cycle after the other), and
# prints its own VmRSS every 2 s, then exits at 7 s. They differ only in
# what the slot's callback holds:
#   control  a 4096-element array `big`, computed from the element
#   byref    the same, read through `let r = &big; *r`   (ByRef::delete keeps
#            its cell's store entry, so each deleted slot pins its `big`)
#   iter     `array::iter([big])`  (Iter::delete unrefs its delivery id but
#            keeps the store entry holding the last value it delivered)
#   catch    20 `catch(e) k;` and no array  (Catch::delete never unbinds its
#            error binding from env.by_id / env.binds)
#   queuefn  10 `queuefn(#trigger: x, |v: i64| v + k)(x)`  (WrapperApply
#            keeps its genn::bind args, QueueFn keeps fid's entry: f's def)
#   sleep    no slot: a select arm holding `array::iter([big])` sleeps every
#            other cycle; Iter::sleep mints a fresh id, the old entry stays
#
# command: timeout -s KILL 170 bash design/review-2026-10-05/repro/x-node-contract-06.sh <graphix>
#          (runs `<graphix> --no-cache --no-fusion` on each program)
#
# expected: every program's VmRSS stays flat like control's.
# observed (HEAD c722befe, debug build; VmRSS MB at 2 / 4 / 6 s):
#   control   75   81   82      byref   253  435  616     iter  245  413  581
#   catch    154  236  318      queuefn 193  321  445     sleep 712 1388 2068
#   Without --no-fusion the same: control 70 71 71 (over 42K slot rebuilds),
#   byref 260 435 609, iter 257 449 628, catch 144 209 281, queuefn 188 312
#   447, sleep 785 1506 2261. With a 16384-element array the byref and iter
#   programs reach a 6 GB memory cap in about 24 s.
G=${1:-graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT

TAIL='let tick = sys::time::timer(duration:2.s, true);
let rss = re::find(#pat: r"VmRSS:\s+\d+ kB", sys::fs::read_all(tick ~ "/proc/self/status")$)$;
sys::exit(sys::time::timer(duration:7.s, false) ~ 0);
"[rss ~ n] cycles: [rss]"'

BIG='  let a0 = [x, x, x, x, x, x, x, x];
  let a1 = array::concat(a0, a0, a0, a0, a0, a0, a0, a0);
  let a2 = array::concat(a1, a1, a1, a1, a1, a1, a1, a1);
  let big = array::concat(a2, a2, a2, a2, a2, a2, a2, a2);'

slot() {
  printf 'let n = 0;\nn <- n + 1;\nlet a = select n %% 2 { 0 => [], _ => [n] };\n'
  printf 'let lens = array::map(a, |x| {\n%s\n});\n%s\n' "$1" "$TAIL"
}

slot "$BIG
  array::len(big)" > "$D/control.gx"
slot "$BIG
  let r = &big;
  array::len(*r)" > "$D/byref.gx"
slot "$BIG
  array::len(array::iter([big]))" > "$D/iter.gx"
slot "$(for i in $(seq 0 19); do echo "  catch(e$i) $i;"; done)
  x" > "$D/catch.gx"
slot "$(for i in $(seq 0 9); do echo "  let q$i = queuefn(#trigger: x, |v: i64| v + $i);"; echo "  let r$i = q$i(x);"; done)
  r0 + r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9" > "$D/queuefn.gx"
{
  printf 'let n = 0;\nn <- n + 1;\nlet x = n;\nlet lens = select n %% 2 {\n  0 => {\n%s\n    array::len(array::iter([big]))\n  },\n  _ => 0\n};\n%s\n' "$BIG" "$TAIL"
} > "$D/sleep.gx"

for p in control byref iter catch queuefn sleep; do
  echo "== $p"
  timeout -s KILL 25 "$G" --no-cache --no-fusion "$D/$p.gx"
done
