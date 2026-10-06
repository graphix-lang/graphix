#!/bin/bash
# x-image-09: macOS build id falls back to the crate version: rebuilt binaries load stale images
#
# graphix-shell/src/cache.rs:48 build_id() reads an ELF64 GNU build-id note or a
# PE link stamp; any other executable, every Mach-O one included (magic
# cf fa ed fe / ca fe ba be: neither "\x7fELF" at :95 nor "MZ" at :68), keys
# the cache "v<graphix-shell version>". The registration key is the image
# format plus the package names (`mod core; mod array; ..`), the program key
# adds the flags and the script's text; the image header checks magic,
# format and ISA. So two different builds at one version share entries.
#
# This script shows that on Linux by putting a binary in the Mach-O state:
#   A = graphix with its NT_GNU_BUILD_ID note's owner renamed (no build id)
#   B = A with the stdlib's math::pi changed to 3.0 (source + packed AST),
#       i.e. "edit stdlib, rebuild at the same version"
#   C = B with a build id of its own (what a relink gives on Linux): control
#
# Command (from the repo root):
#   WORK=<scratch dir> RUNNER="<optional memory-cap wrapper>" \
#     bash design/review-2026-10-05/repro/x-image-09.sh [path/to/graphix]
# Expected: B prints "pi = 3" with or without the cache.
# Observed (HEAD c722befe, debug build):
#   A, cache (cold, writes registration/v0.9.0/*.img)   pi = 3.141592653589793
#   B, --no-cache                                       pi = 3
#   B, cache (A's program entry)                        pi = 3.141592653589793   <- stale
#   B, cache, new program (A's registration entry)      2pi = 6.283185307179586  <- stale
#   B, --no-cache, new program                          2pi = 6
#   C, cache (own build id: misses, compiles cold)      pi = 3
set -u
GX=${1:-$HOME/tmp/target/debug/graphix}
WORK=${WORK:-$(mktemp -d)}
RUNNER=${RUNNER:-}
mkdir -p "$WORK"
trap 'rm -f "$WORK"/graphix-A "$WORK"/graphix-B "$WORK"/graphix-C' EXIT
cp "$GX" "$WORK/graphix-A"
cp "$GX" "$WORK/graphix-B"
cp "$GX" "$WORK/graphix-C"
python3 - "$WORK" <<'EOF'
import struct, sys
w = sys.argv[1]

def build_id_note(data):
    # the walk cache.rs::elf_build_id does: PT_NOTE segments, NT_GNU_BUILD_ID
    phoff, = struct.unpack_from('<Q', data, 32)
    phentsize, phnum = struct.unpack_from('<HH', data, 54)
    for i in range(phnum):
        ph = phoff + i * phentsize
        if struct.unpack_from('<I', data, ph)[0] != 4:
            continue
        off, = struct.unpack_from('<Q', data, ph + 8)
        size, = struct.unpack_from('<Q', data, ph + 32)
        at = off
        while at + 12 <= off + size:
            namesz, descsz, ntype = struct.unpack_from('<III', data, at)
            name_end = at + 12 + namesz
            desc_start = (name_end + 3) & ~3
            if ntype == 3 and data[at + 12:name_end] == b'GNU\0':
                return at + 12, desc_start, descsz
            at = (desc_start + descsz + 3) & ~3
    raise SystemExit('no GNU build id note')

def patch(path, at, old, new):
    with open(path, 'r+b') as f:
        f.seek(at)
        assert f.read(len(old)) == old, (path, at)
        f.seek(at)
        f.write(new)

data = open(f'{w}/graphix-A', 'rb').read()
name_at, desc_at, descsz = build_id_note(data)
src = b'let pi: f64 = f64:3.141592653589793'
src_at = data.find(src)
pi = struct.pack('>d', 3.141592653589793)
pi_at = data.find(pi, src_at)
assert src_at >= 0 and 0 < pi_at - src_at < 65536 and data.find(pi, pi_at + 1) < 0
for b in ('A', 'B'):
    patch(f'{w}/graphix-{b}', name_at, b'GNU\0', b'XNU\0')
for b in ('B', 'C'):
    patch(f'{w}/graphix-{b}', src_at, src, b'let pi: f64 = f64:3.000000000000000')
    patch(f'{w}/graphix-{b}', pi_at, pi, struct.pack('>d', 3.0))
last = desc_at + descsz - 1
patch(f'{w}/graphix-C', last, data[last:last + 1], bytes([data[last] ^ 0xff]))
EOF
cat > "$WORK/prog.gx" <<'EOF'
println("pi = [math::pi]");
sys::exit(sys::time::after_idle(duration:100.ms, 0))
EOF
cat > "$WORK/prog2.gx" <<'EOF'
println("2pi = [math::pi * 2.0]");
sys::exit(sys::time::after_idle(duration:100.ms, 0))
EOF
rm -rf "$WORK/cache"
run() {
    local label=$1
    shift
    printf '%-52s ' "$label"
    XDG_CACHE_HOME="$WORK/cache" timeout -s KILL 90 $RUNNER "$@" 2>&1 | grep 'pi =' | head -1
}
run "A, cache (cold, writes the images)" "$WORK/graphix-A" "$WORK/prog.gx"
ls "$WORK/cache/graphix/registration/"
run "B, --no-cache" "$WORK/graphix-B" --no-cache "$WORK/prog.gx"
run "B, cache (A's program entry)" "$WORK/graphix-B" "$WORK/prog.gx"
run "B, cache, new program (A's registration entry)" "$WORK/graphix-B" "$WORK/prog2.gx"
run "B, --no-cache, new program" "$WORK/graphix-B" --no-cache "$WORK/prog2.gx"
run "C, cache (own build id)" "$WORK/graphix-C" "$WORK/prog.gx"
ls "$WORK/cache/graphix/registration/"
