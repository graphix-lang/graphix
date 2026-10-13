#!/bin/sh
# usage: sh x-security-02-pack-deep-overflow.sh /path/to/graphix
# 1 KB of pack bytes (500 nested one-element arrays around null) read as a
# recursive type aborts the process: netidx Value::cast recurses through
# first elements without a stack guard (graphix-types/src/typ/cast.rs convert).
d=$(mktemp -d)
python3 -I -c "open('$d/deep.pack','wb').write(bytes([19,1])*500+bytes([16]))"
cat > $d/p.gx <<GX
type T = [null, Array<T>];
sys::exit(sys::time::after_idle(duration:500.ms, 0));
let b = sys::fs::read_all_bin("$d/deep.pack")\$;
let v: [T, Error<[\`PackErr(string), \`InvalidCast(string)]>] = pack::read(b);
select v { error as e => "err", _ => "ok" }
GX
"$1" --no-cache $d/p.gx   # thread 'tokio-rt-worker' has overflowed its stack; abort
