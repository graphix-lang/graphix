#!/bin/bash
# Title: SKILL.md "Annotations" says a lambda's `-> T` is needed where a
# .gxi declares the function ("an inferred return does not cross modules")
# and where a function with a type variable has a field of its result read;
# neither is asked for by the checker.
# Command: bash lang-skill-return-annotations-unneeded.sh
# Expected (per SKILL.md): both programs refused until `-> T` is written.
# Observed: both run and print 3 and 1. A third program shows that for a
# never() let with writers of two types the checker asks for the union
# ("declare go: [i64, string]"), not the `Any` SKILL.md recommends.
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
d=$(mktemp -d)
trap 'rm -rf "$d"' EXIT
printf 'mod a;\na::f(1) + a::g(1).v\n' > "$d/main.gx"
printf "val f: fn(x: i64) -> i64;\nval g: fn(x: 'a) -> {v: 'a};\n" > "$d/a.gxi"
printf "let f = |x| x + 1;\nlet g = |x| {v: x}\n" > "$d/a.gx"
timeout -s KILL 10 "$G" --no-cache "$d/main.gx"
printf "let h = |x: 'a| {v: x};\nh(1).v\n" > "$d/top.gx"
timeout -s KILL 10 "$G" --no-cache "$d/top.gx"
printf 'let go = never();\ngo <- 1;\ngo <- "a";\ngo\n' > "$d/any.gx"
timeout -s KILL 10 "$G" --check "$d/any.gx" 2>&1 | tail -1
exit 0
