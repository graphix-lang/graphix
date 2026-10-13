#!/bin/sh
# tests-types-02: a cast to a nominal abstract type checks id and params
# only, never the payload against the representation.
#
# Run: sh design/review-2026-10-12/repro/tests-types-02.sh [path/to/graphix]
#
# Part 1 (type confusion): bytes packed from `type Box = Abstract<string>`
# holding Box("hello") are pack::read as `type Box = Abstract<i64>`.
#   expected: `Err(InvalidCast ..) in both engines (payload is not an i64)
#   observed: JIT prints `Ok(1)` ("hello" read as 0); node-walk
#             (--no-fusion) logs an arith parse error and prints nothing.
# Part 2 (forged reference): bytes for `R(&mut <bind id 6>)` with
# `type R = Abstract<&mut i64>`, never derived from `secret`, are read
# from data and written through.
#   expected: the read is refused (a reference can't be read from data;
#             the 'b: Concrete target holds a reference)
#   observed: prints 5 then 42: `secret`, which the program never took a
#             reference to, is overwritten.
# The constant `minted` value registers the GxAbstract decoder.
G=${1:-graphix}
D=$(mktemp -d)
cat > "$D/confuse.gx" <<'EOF'
type Box = Abstract<i64>;
let minted = Box(1);
let bs = minted ~ bytes:GyVaDDF+kktOYbgtbxPJpH0IgVt4u3Lm5Q8DQm94AAwFaGVsbG8=;
let r: Result<Box, [`InvalidCast(string), `PackErr(string)]> = pack::read(bs);
let f = |b: Box| b.0 + 1;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
select r { error as e => `Err(e), b => `Ok(f(b)) }
EOF
cat > "$D/forge.gx" <<'EOF'
type R = Abstract<&mut i64>;
let secret = 5;
let mine = 6;
let minted = R(&mut mine);
let forged: Result<R, [`InvalidCast(string), `PackErr(string)]> = pack::read(minted ~ bytes:GyVaDDF+kktOYbgtbxPJpH0Ik4nT+o7RULgBUgAEQAAAAAAAEAY=);
let p = select forged { error as e => never(), R(p) => p };
*p <- p ~ 42;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
secret
EOF
echo "== confuse, JIT";       timeout -s KILL 30 "$G" --no-cache "$D/confuse.gx"
echo "== confuse, node-walk"; timeout -s KILL 30 "$G" --no-cache --no-fusion "$D/confuse.gx"
echo "== forge";              timeout -s KILL 30 "$G" --no-cache "$D/forge.gx"
rm -rf "$D"
