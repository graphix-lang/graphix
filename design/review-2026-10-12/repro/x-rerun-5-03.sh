#!/usr/bin/env bash
# x-rerun-5-03 (variant of small-pkgs-16, fixed in re/str only): sys::net
# formats anyhow errors with {e:?} into program-visible error values
# (stdlib/graphix-package-sys/src/netstate.rs:544, 570, 581, 632, 654, 664,
# 688), so under RUST_BACKTRACE=1 an RpcError's text carries a Rust stack
# backtrace and its length depends on the environment.
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-12/repro/x-rerun-5-03.sh
# expected: the same "rpc-bad-arg" line with and without RUST_BACKTRACE
# observed (HEAD 7324207f, debug build): with RUST_BACKTRACE=1 the RpcError
#   string is "no such argument vall\n\nStack backtrace:\n   0: <anyhow::Error>::msg ..."
G=${GRAPHIX:-graphix}
D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
cat > "$D/p.gx" <<'GX'
sys::net::rpc(#path: "/local/setter", #doc: "d", #spec: {val: {default: i64:0, doc: "v"}}, #f: |a: {val: i64}| a ~ null);
let r2: Result<null, [`RpcError(string), `InvalidCast(string)]> = sys::net::call("/local/setter", {vall: 42});
println(select r2 { error as e => "rpc-bad-arg: len [str::len("[e]")]: [e]", v => "rpc-bad-arg: value [v]" });
sys::exit(sys::time::after_idle(duration:1.s, 0))
GX
echo "== plain";            timeout -s KILL 30 "$G" --no-cache --no-netidx "$D/p.gx" 2>&1 | cut -c1-200
echo "== RUST_BACKTRACE=1"; RUST_BACKTRACE=1 timeout -s KILL 30 "$G" --no-cache --no-netidx "$D/p.gx" 2>&1 | cut -c1-200
