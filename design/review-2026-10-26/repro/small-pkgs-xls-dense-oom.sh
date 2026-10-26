#!/bin/sh
# small-pkgs: a 5.6 KB .xls (BIFF8) OOM-kills the runtime through xls::sheets
# Run: sh design/review-2026-10-26/repro/small-pkgs-xls-dense-oom.sh
# Expected: an `XlsErr` value (the sheet spans more than MAX_CELLS), as the ods
#   guard check_ods gives for the same shape of ods file.
# Observed: "[probe] OOM-KILLED: this run exceeded its 6G memory cap" (exit 251).
#   calamine's Xls reader builds every sheet densely at open
#   (Range::from_sparse over the cells' bounding box): one cell at A1 and one at
#   row 65535, column 65535 ask for 2^32 cells; huge.xls does that.
set -e
D=${TMPDIR:-/tmp}/small-pkgs-xls-oom; mkdir -p "$D"
cat > "$D/mkxls.py" <<'PY'
# mkxls.py OUT NSHEETS ROW COL : a BIFF8 .xls (CFB, 512-byte sectors) whose
# NSHEETS BoundSheet records all name one sheet substream holding two NUMBER
# cells, at A1 and at (ROW, COL).
import struct, sys
out, nsheets, row, col = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4])
def rec(t, d): return struct.pack('<HH', t, len(d)) + d
bof_glob = rec(0x0809, struct.pack('<HH', 0x0600, 0x0005) + b'\0' * 12)
def boundsheet(pos, name):
    n = name.encode('latin1')
    return rec(0x0085, struct.pack('<IBB', pos, 0, 0) + bytes([len(n), 0]) + n)
names = ['S%d' % i for i in range(nsheets)]
glob_len = len(bof_glob) + sum(len(boundsheet(0, n)) for n in names) + 4
sheet = (rec(0x0809, struct.pack('<HH', 0x0600, 0x0010) + b'\0' * 12)
         + rec(0x0203, struct.pack('<HHHd', 0, 0, 0, 1.0))
         + rec(0x0203, struct.pack('<HHHd', row, col, 0, 2.0))
         + rec(0x000A, b''))
wb = bof_glob + b''.join(boundsheet(glob_len, n) for n in names) + rec(0x000A, b'') + sheet
wb = wb.ljust(max(4096, (len(wb) + 511) // 512 * 512), b'\0')
nsec = len(wb) // 512
hdr = bytearray(512)
hdr[0:8] = bytes.fromhex('D0CF11E0A1B11AE1')
struct.pack_into('<HHHHH', hdr, 24, 0x3E, 3, 0xFFFE, 9, 6)
struct.pack_into('<I', hdr, 44, 1)            # FAT sectors
struct.pack_into('<I', hdr, 48, 1)            # directory start
struct.pack_into('<I', hdr, 56, 4096)
struct.pack_into('<IIII', hdr, 60, 0xFFFFFFFE, 0, 0xFFFFFFFE, 0)
for i in range(109): struct.pack_into('<I', hdr, 76 + 4 * i, 0xFFFFFFFF)
struct.pack_into('<I', hdr, 76, 0)
fat = [0xFFFFFFFD, 0xFFFFFFFE] + [3 + i for i in range(nsec - 1)] + [0xFFFFFFFE]
fat += [0xFFFFFFFF] * (128 - len(fat))
fat = struct.pack('<128I', *fat)
def dirent(name, typ, start, size):
    e = bytearray(128)
    n = name.encode('utf-16-le') + b'\0\0'
    e[0:len(n)] = n
    struct.pack_into('<HBB', e, 64, len(n), typ, 1)
    struct.pack_into('<III', e, 68, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF)
    struct.pack_into('<II', e, 116, start, size)
    return bytes(e)
d = dirent('Root Entry', 5, 0xFFFFFFFE, 0) + dirent('Workbook', 2, 2, len(wb))
d = d.ljust(512, b'\0')
open(out, 'wb').write(bytes(hdr) + fat + d + wb)
PY
python3 -I "$D/mkxls.py" "$D/huge.xls" 1 65535 65535
cat > "$D/x.gx" <<GX
let data = sys::fs::read_all_bin("$D/huge.xls")\$;
sys::exit(sys::time::after_idle(duration:500.ms, 0));
xls::sheets(data)
GX
timeout -s KILL 120 /home/eric/tmp/review-2026-10-26/bin/graphix --no-cache "$D/x.gx"
