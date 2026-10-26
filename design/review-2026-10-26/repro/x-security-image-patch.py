# Usage: python3 -I x-security-image-patch.py <XDG_CACHE_HOME> <old> <new> (same length)
# Rewrites a string literal in a graphix program-entry image and reseals its crc32.
import sys, zlib, struct, glob
old, new = sys.argv[2].encode(), sys.argv[3].encode()
assert len(old) == len(new)
for path in glob.glob(sys.argv[1] + "/graphix/registration/*/*.img"):
    b = bytearray(open(path, "rb").read())
    if b[:8] != b"GXDEPS\0\0":
        continue
    n = struct.unpack("<Q", b[8:16])[0]
    start = -(-(16 + n) // 64) * 64
    img = b[start:]
    hits = img.count(old)
    img = img.replace(old, new)
    assert img[:4] == b"GXIM"
    img[5:9] = struct.pack(">I", zlib.crc32(bytes(img[9:])) & 0xffffffff)
    b[start:] = img
    open(path, "wb").write(b)
    print(f"patched {hits} occurrence(s) in {path}")
