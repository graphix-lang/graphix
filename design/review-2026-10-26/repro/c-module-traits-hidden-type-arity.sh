#!/bin/sh
# A hidden type's implementation may drop a parameter its interface declares
# check_hidden_typedef pairs the implementation's parameters with the interface's by
# name and only one way, so `type Box<'a, 'b>;` in the .gxi is "implemented" by
# `type Box<'a> = Abstract<'a>`, and code outside writes m::Box<i64, string>.
# Run: sh c-module-traits-hidden-type-arity.sh
# Expected: the module is refused (the implementation lacks 'b)
# Observed: --check exits 0
G=${G:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
printf "type Box<'a, 'b>;\nval n: i64\n" > $D/m.gxi
printf "type Box<'a> = Abstract<'a>;\nlet n = 1\n" > $D/m.gx
printf "mod m;\nlet b: m::Box<i64, string> = never();\nm::n\n" > $D/main.gx
timeout -s KILL 30 $G --no-cache --check $D/main.gx; echo "check exit $?"
rm -rf $D
