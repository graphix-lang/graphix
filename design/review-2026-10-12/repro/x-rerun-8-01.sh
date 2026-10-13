#!/usr/bin/env bash
# x-rerun-8-01 (variant of x-image-01): a program entry made while `m.gx`
# resolved `mod m` does not record `m/mod.gx` as absent, so a warm start
# masks the "both define module m" refusal a cold compile gives.
# sources_record (graphix-shell/src/cache.rs:205-245) records `m.gx` beside
# a read `m/mod.gx`, never `m/mod.gx` beside a read `m.gx`; the root_module_site
# loop breaks at once for a module in the root's own directory.
# command: GRAPHIX=/path/to/graphix bash x-rerun-8-01.sh
# expected: warm == --no-cache (refused: both define module m)
# observed: warm prints "file", --no-cache refuses
set -u
G=${GRAPHIX:-graphix}
d=$(mktemp -d); trap 'rm -rf "$d"' EXIT
export XDG_CACHE_HOME=$d/cache
mkdir -p "$d/p"; cd "$d/p"
printf 'mod m;\nsys::exit(sys::time::after_idle(duration:100.ms, m::x ~ 0));\nm::x\n' > main.gx
printf 'let x = "file"\n' > m.gx
echo "cold:       $(timeout -s KILL 60 "$G" main.gx 2>&1 | tail -1)"
mkdir m; printf 'let x = "dir"\n' > m/mod.gx
echo "warm:       $(timeout -s KILL 60 "$G" main.gx 2>&1 | tail -1)"
echo "--no-cache: $(timeout -s KILL 60 "$G" --no-cache main.gx 2>&1 | tail -1)"
