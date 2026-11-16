#!/bin/sh
# Title: a default whose label is written at a bound holding `self`, ahead
#   of the receiver, is refused by a bare mismatch that prints the declared
#   'b with a `V` bound the trait never wrote and names no remedy.
# Command: sh design/review-2026-11-16/repro/tests-traits-b-label-before-self-message.sh
# Expected: a refusal saying the default's written `self` is the lambda's own
#   variable, which the label decides as 'b before the receiver, and naming
#   the remedies lang::traits' comment gives (write the label as the declared
#   variable, or leave it unwritten); the declaration printed as declared,
#   fn<'b: [self, null]>(#k: 'b, self) -> i64.
# Observed: `written` is refused "type mismatch fn<'b: V + [null, 'self:
#   unbound within V]>(..) does not contain fn(#k: [null, 'self: ..], s:
#   'self: 'b: ..)"; `body_names_self` (the receiver annotated 'q so the
#   label passes) is refused "'self: unbound does not contain 'q: unbound";
#   both with the label unwritten check.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > $D/written.gx <<'G1'
trait V {
    val v: fn<'b: [self, null]>(#k: 'b, self) -> i64 = |#k: [self, null], s|
        select k { null as _ => 0, _ => 1 }
};
impl V for i64 {};
V::v(#k: 2, 1)
G1
sed 's/|#k: \[self, null\], s|/|#k, s|/' $D/written.gx > $D/unwritten.gx
cat > $D/body_names_self.gx <<'G2'
trait V {
    val v: fn<'b: [self, null]>(#k: 'b, self) -> i64 = |#k: [self, null], s: 'q| { let r: self = s; 0 }
};
impl V for i64 {};
V::v(#k: 2, 1)
G2
sed 's/|#k: \[self, null\], s: /|#k, s: /' $D/body_names_self.gx > $D/body_names_self_unwritten.gx
for f in written unwritten body_names_self body_names_self_unwritten; do
  echo "== $f"
  timeout -s KILL 30 $G --check --no-cache $D/$f.gx 2>&1 | grep -v '^ *[0-9]*: at' | tail -1
done
rm -rf $D
