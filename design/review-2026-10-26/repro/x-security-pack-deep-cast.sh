#!/bin/sh
# Title: pack::read of 1.2 MB of crafted bytes into a recursive type exhausts
# memory: the cast recurses once per nesting level and each level costs
# ~10 KB of (stacker-grown) stack, with no depth limit on pack data.
# Command: sh design/review-2026-10-26/repro/x-security-pack-deep-cast.sh
# Expected: an error (a PackErr or InvalidCast naming a depth limit, as
#   json::read's recursion limit and toml's give), in well under 100 MB.
# Observed (debug build, 6 GB cap): depth 300000 reads "ok" in 0.5 s; depth
#   600000 (1.2 MB of input) is OOM-killed past 6 GB within 0.5 s. The same
#   bytes read as Array<Any> or i64 finish at once: the cost is the cast to
#   the recursive type, not the decode.
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
python3 -I -c "import sys; open(sys.argv[1], 'wb').write(bytes([19, 1]) * 600000 + bytes([19, 0]))" $D/deep.pack
cat > $D/p.gx <<GX
sys::exit(sys::time::after_idle(duration:300.ms, 0));
type N = Array<N>;
let b = sys::fs::read_all_bin("$D/deep.pack");
let r: Result<N, [\`PackErr(string), \`InvalidCast(string)]> = pack::read(b\$);
println(select r { error as e => "error", _ => "ok" })
GX
timeout -s KILL 60 $G --no-cache $D/p.gx
echo "exit $?"
rm -rf $D
