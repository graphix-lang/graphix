#!/bin/sh
# The cache key holds no ISA: two hosts with one build id and different
# CPU features that share a cache directory (an NFS home) overwrite each
# other's entries, and every start on either is cold. Simulated here by
# rewriting one ISA flag of this host's entries (`has_avx512f=1` to `=0`,
# checksum resealed), as another host's run would have left them.
#
# Run: sh x-image-isa-not-in-key.sh /path/to/graphix
# Expected: the other host's entries sit beside this host's; this run
#   restores its own (or writes its own beside them).
# Observed: both entries are refused ("the image was written for another
#   isa", the registration's too, though it holds no native code) and then
#   overwritten with this host's, so the other host's next start is cold
#   again; the last line prints True for both files (this host's flags).
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/p.gx" <<'EOF'
let f = |x| x + 1;
sys::exit(sys::time::after_idle(duration:10.ms, 0));
f(1)
EOF
XDG_CACHE_HOME=$D/cache "$G" "$D/p.gx" > /dev/null
python3 - "$D" <<'EOF'
import glob, struct, sys, zlib
d = sys.argv[1]
for f in glob.glob(d + '/cache/graphix/registration/*/*.img'):
    b = bytearray(open(f, 'rb').read())
    s = 0
    if b[:8] == b"GXDEPS\0\0":
        n = struct.unpack("<Q", b[8:16])[0]
        s = (16 + n + 63) // 64 * 64
    j = b.find(b'has_avx512f=1', s)
    if j < 0:
        j = b.find(b'has_avx2=1', s)
    k = b.find(b'=1', j)
    b[k + 1:k + 2] = b'0'
    b[s + 5:s + 9] = struct.pack(">I", zlib.crc32(bytes(b[s + 9:])) & 0xffffffff)
    open(f, 'wb').write(bytes(b))
EOF
mkdir -p "$D/logs"
XDG_CACHE_HOME=$D/cache RUST_LOG=info "$G" --log-dir "$D/logs" "$D/p.gx" > /dev/null
grep -h "another isa\|written to" "$D"/logs/*.log | cut -c1-120
python3 - "$D" <<'EOF'
import glob, sys
for f in glob.glob(sys.argv[1] + '/cache/graphix/registration/*/*.img'):
    b = open(f, 'rb').read()
    print(f.rsplit('/', 1)[1], b'has_avx512f=1' in b or b'has_avx2=1' in b)
EOF
