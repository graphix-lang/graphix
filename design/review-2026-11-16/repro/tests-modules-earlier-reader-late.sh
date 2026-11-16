#!/bin/sh
# Title: the earlier-reader interface pins read r a cycle after it changes
# a_write_through_an_interface_reaches_an_earlier_reader_of_the_body (and
# a_body_write_reaches_an_earlier_reader_through_its_interface) claim the
# write "lands on both as the cycle starts, so a reader ... before the module
# sees it". The write lands at ep 3 and `r` is already 11 there, but the
# fixtures read `r` at ep 4, where a reader served a cycle late reads 11 too.
# Command: sh tests-modules-earlier-reader-late.sh  (G= the debug graphix)
# Observed: "11 | 1:1 2:1 3:11" (ep:r), the same over plain lets.
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d)
cat > "$D/main.gx" <<'GX'
let ep = 0;
ep <- select ep { n if n < 4 => n + 1, _ => never() };
let h: fn(k: i64) -> i64 = never();
let r = h(1);
mod a;
h <- a::g;
a::x <- select ep { 2 => 10, _ => never() };
let seen = "";
seen <- ep ~ "[seen] [ep]:[r]";
sys::exit(sys::time::after_idle(duration:100.ms, 0));
select ep { 4 => "[r] |[seen]", _ => never() }
GX
echo 'val x: i64; val g: fn(k: i64) -> i64' > "$D/a.gxi"
echo 'let x = 0; let g = |k: i64| -> i64 x + k' > "$D/a.gx"
timeout -s KILL 30 "$G" --no-cache "$D/main.gx" | head -1
rm -rf "$D"
