#!/bin/sh
# Title: a literal pattern is typed `Primitive(Typ::get(v))`, not by the rule
# expression literals use (compiler.rs constant_type): `error:1` types as the
# primitive `error` and an `abstract:..` literal is accepted.
# Command: sh design/review-2026-10-19/repro/t-parser-b-literal-pattern-type.sh
# Expected: 1.gx refused like `1.5` over [i64, Error<string>] ("pattern
# Error<i64> will never match ..., unused match cases"); 2.gx refused like the
# expression `abstract:..` ("an abstract value literal has no type").
# Observed: both check and run, printing 2 (the `error:1` arm is dead, the
# abstract arm matches nothing a program can build).
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
cat > $D/1.gx <<'GX'
let r: [i64, Error<string>] = error("x");
sys::exit(sys::time::after_idle(duration:100.ms, 0));
select r { error:1 => 1, _ => 2 }
GX
cat > $D/2.gx <<'GX'
let x: Any = 1;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
select x { abstract:EgABAgMEBQYHCAkKCwwNDg8BAg== => 1, _ => 2 }
GX
for f in 1 2; do echo "== $f"; timeout -s KILL 30 $G --no-cache $D/$f.gx 2>&1 | tail -2; done
rm -rf $D
