#!/bin/sh
# Title: a value stored in a db that decodes as an error reaches the program
# as the call's error, outside its declared `DbErr` (CachedArgsAsync skips
# read_back for every Value::Error a DATA_RESULT eval returns).
# Command: sh design/review-2026-10-26/repro/x-security-db-stored-error.sh
# Expected: db::get refuses the stored error as data its tree cannot hold:
#   DbErr: ...  (or the import refuses the file)
# Observed: select e.0 { `DbErr(m) => .. }: no arm matches: a coverage hole in
#   the check   (exit 134 under GRAPHIX_ABORT_ON_NO_MATCH=1)
# The file is an ordinary db::export of a Tree<string, i64> with one value's
# 9 bytes (I64 0x0102030405060708) replaced by an equally long error value.
G=${GRAPHIX:-/home/eric/tmp/review-2026-10-26/bin/graphix}
D=$(mktemp -d)
cat > $D/a.gx <<GX
let r = seq {
    let db = db::open("$D/dbA")\$;
    let t: db::Tree<string, i64> = db::tree(db, "t")\$;
    let _ = db::insert(t, "k", 72623859790382856)\$;
    let _ = db::flush(db)\$;
    let _ = db::export(db, "$D/export.bin")\$;
    "exported"
};
sys::exit(sys::time::after_idle(duration:200.ms, r ~ 0));
r
GX
timeout -s KILL 30 $G --no-cache $D/a.gx
python3 -I -c "
import sys
b = bytearray(open(sys.argv[1], 'rb').read())
i = b.find(bytes([6, 1, 2, 3, 4, 5, 6, 7, 8]))
b[i:i + 9] = bytes([18, 7]) + b'EVIL!!!'
open(sys.argv[1], 'wb').write(b)" $D/export.bin
cat > $D/b.gx <<GX
let r = seq {
    let db = db::open("$D/dbB")\$;
    let _ = db::import(db, "$D/export.bin")\$;
    let t: db::Tree<string, i64> = db::tree(db, "t")\$;
    db::get(t, "k")
};
sys::exit(sys::time::after_idle(duration:200.ms, r ~ 0));
select r {
    error as e => select e.0 { \`DbErr(m) => "DbErr: [m]" },
    null as _ => "absent",
    i64 as n => "value [n]"
}
GX
GRAPHIX_ABORT_ON_NO_MATCH=1 timeout -s KILL 30 $G --no-cache $D/b.gx
echo "exit $?"
rm -rf $D
