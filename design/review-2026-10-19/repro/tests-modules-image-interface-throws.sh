#!/bin/bash
# Title: an error raised through an interface `val .. throws` is never raised
#   by any fixture; and the throws mismatch prints internal forms
# Command: bash design/review-2026-10-19/repro/tests-modules-image-interface-throws.sh
# Expected:
#   raise:     -1 (the caller's catch hears the ArrayIndexError get_value raises)
#   undeclared/other: refused, naming the declared and the implemented throws
#     types as written (`Error<ErrChain<`A>>`, or "throws nothing")
# Observed (HEAD 9445a1d21):
#   raise:     -1 (works; interfaces.rs abstract_type_with_throws_clause and
#              abstract_type_in_throws never raise, so nothing pins this)
#   undeclared: "in throws clause: type mismatch: signature has _, implementation
#              has Error<ErrChain<`Zero>>"
#   other:     "in throws clause: type mismatch: signature has [null, ErrChain<`A>],
#              implementation has [null, ErrChain<`Zero>]"
G=${G:-/home/eric/tmp/review-2026-10-19/bin/graphix}
d=$(mktemp -d); cd "$d"
printf 'type T;\nval make: fn(x: i64) -> T;\nval get_value: fn(t: T) -> i64 throws Error<ErrChain<`ArrayIndexError(string)>>\n' > inner.gxi
printf 'type T = Abstract<{ value: i64 }>;\nlet make = |x: i64| -> T T({ value: x });\nlet get_value = |t: T| -> i64 {\n    let a = [t.0.value + 41];\n    a[t.0.value]?\n}\n' > inner.gx
cat > raise.gx <<'EOF'
mod inner;
let caught = 0;
let r = {
    catch(e) caught <- select e.0.error { `ArrayIndexError(_) => -1 };
    inner::get_value(inner::make(1))
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
caught
EOF
echo "== raise"; timeout -s KILL 30 "$G" --no-cache raise.gx 2>&1 | tail -1
printf 'val f: fn(x: i64) -> i64\n' > m.gxi
printf 'let f = |x: i64| -> i64 select x { 0 => error(`Zero)?, n => n }\n' > m.gx
printf 'mod m;\nm::f(1)\n' > main.gx
echo "== undeclared"; timeout -s KILL 30 "$G" --check main.gx 2>&1 | tail -1
printf 'val f: fn(x: i64) -> i64 throws Error<ErrChain<`A>>\n' > m.gxi
echo "== other"; timeout -s KILL 30 "$G" --check main.gx 2>&1 | tail -1
rm -rf "$d"
