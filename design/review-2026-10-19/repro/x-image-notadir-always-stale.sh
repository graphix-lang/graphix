#!/bin/bash
# x-image: a program entry is stale forever when a module name is also a plain file
#
# Run: bash x-image-notadir-always-stale.sh [path-to-graphix]
# Setup: proj/main.gx does `mod m;`, proj/m.gx defines it, and proj/m is a regular file
#   (an executable or data file named like the module). Resolution probes proj/m/mod.gx,
#   read_optional maps NotADirectory to "absent", and the entry records `-proj/m/mod.gx`.
# Expected: the second and third runs restore the program entry.
# Observed (HEAD 9445a1d21): every run logs "Program image ... is stale", compiles cold and
#   rewrites the entry: sources_unchanged accepts only NotFound for an absent path, and
#   symlink_metadata of proj/m/mod.gx fails with NotADirectory. Removing proj/m makes
#   the entry restore. A dangling symlink at a probed path (proj/m.gxi -> /nonexistent)
#   is the same miss the other way: read_optional reads NotFound, symlink_metadata finds
#   the link (second case below).
G=${1:-/home/eric/tmp/review-2026-10-19/bin/graphix}
D=$(mktemp -d)
mkdir -p $D/proj
printf 'mod m;\nsys::exit(sys::time::after_idle(duration:50.ms, 0));\nm::x\n' > $D/proj/main.gx
printf 'let x = 1\n' > $D/proj/m.gx
printf 'not a module\n' > $D/proj/m
for i in 1 2 3; do
  rm -rf $D/log
  RUST_LOG=info XDG_CACHE_HOME=$D/cache timeout -s KILL 20 $G --log-dir $D/log $D/proj/main.gx > /dev/null
  echo "run $i: $(grep -ho 'is stale\|Program image written' $D/log/* | tr '\n' ' ')"
done
rm -f $D/proj/m; rm -rf $D/cache
ln -s /nonexistent/target $D/proj/m.gxi
for i in 1 2; do
  rm -rf $D/log
  RUST_LOG=info XDG_CACHE_HOME=$D/cache timeout -s KILL 20 $G --log-dir $D/log $D/proj/main.gx > /dev/null
  echo "dangling m.gxi, run $i: $(grep -ho 'is stale\|Program image written' $D/log/* | tr '\n' ' ')"
done
rm -rf $D
