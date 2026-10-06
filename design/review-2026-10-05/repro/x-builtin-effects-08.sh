#!/usr/bin/env bash
# x-builtin-effects-08: hbs::render, sys::net::call and sys::net::rpc refuse
# in typecheck1 what --check and the LSP accept.
#
# The check (--check, the LSP: GXRt::check under CFlag::CheckOnly) runs
# typecheck0 and the call-site settles and stops (graphix-compiler/src/
# lib.rs:1958); it never runs typecheck1. These builtins' typecheck1 hooks
# refuse argument shapes their signatures ('a, 'b, 'spec, 'args) admit:
#   stdlib/graphix-package-hbs/src/lib.rs:125  #partials struct/map/null, data struct/map
#   stdlib/graphix-package-sys/src/net.rs:479  sys::net::call args struct/null/Any
#   stdlib/graphix-package-sys/src/net.rs:890  publish_rpc validate_spec (#spec, #f shapes)
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-builtin-effects-08.sh
#   graphix-fuzz check on the file of case 1, 2 or 3 prints
#   "DIVERGENCE — the check accepted what the build refused (elaboration
#   refused a program the definition and call-site checks passed: a
#   type-system bug)".
#
# expected (CLAUDE.md: elaboration refusing what the check accepted is a
# type-system bug): --check and the build agree on every case.
# observed (HEAD c722befe, debug build):
#   1 hbs data i64:        check rc=0; build: expected struct or map not i64
#   2 net::call args i64:  check rc=0; build: sys::net::call args must be a struct or null
#   3 rpc #spec i64:       check rc=0; build: rpc #spec must be a struct or null
#   4 rpc #f lacks field:  check rc=0; build: rpc #f argument missing field 'x'
#   5 case 1 through a dynamic call: check rc=0; the run prints "v=42" and
#     logs ERROR "a run-time bind at f(42) did not elaborate: ...
#     expected struct or map not i64" (the static refusal is only logged,
#     and the builtin renders scalar data fine).
#   6 rpc #spec {x: 5} through a dynamic call: check rc=0; the run logs
#     "did not elaborate: ... expected RpcArg {{default: 'a, doc: string}}
#     not i64", then the runtime panics at
#     stdlib/graphix-package-sys/src/net.rs:1149 "internal error: entered
#     unreachable code" and the shell exits "Error: runtime did not respond":
#     PublishRpc::update takes validate_spec's refusal as an invariant.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
export XDG_CACHE_HOME="$dir/cache"

cat > "$dir/1_hbs_data.gx" <<'EOF'
hbs::render("hello {{this}}", 42)
EOF
cat > "$dir/2_net_call.gx" <<'EOF'
let r: Result<i64, [`RpcError(string), `InvalidCast(string)]> = sys::net::call("/local/rpc", 42);
r
EOF
cat > "$dir/3_rpc_spec.gx" <<'EOF'
sys::net::rpc(#path: "/local/foo", #doc: "x", #spec: 42, #f: |a: i64| a + 1)
EOF
cat > "$dir/4_rpc_fields.gx" <<'EOF'
sys::net::rpc(#path: "/local/foo", #doc: "x", #spec: {x: {default: 1, doc: "the x"}}, #f: |a: {y: i64}| a.y + 1)
EOF
cat > "$dir/5_hbs_dynamic.gx" <<'EOF'
let b = true;
let f = select b {
  true => |x| hbs::render("v={{this}}", x),
  false => |x| hbs::render("w={{this}}", x)
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
f(42)
EOF
cat > "$dir/6_rpc_dynamic.gx" <<'EOF'
let b = true;
let f = select b {
  true => |s| sys::net::rpc(#path: "/local/foo", #doc: "x", #spec: s, #f: |a: {x: i64}| a.x + 1),
  false => |s| sys::net::rpc(#path: "/local/bar", #doc: "x", #spec: s, #f: |a: {x: i64}| a.x + 1)
};
sys::exit(sys::time::after_idle(duration:100.ms, 0));
f({x: 5})
EOF

for f in 1_hbs_data 2_net_call 3_rpc_spec 4_rpc_fields 5_hbs_dynamic 6_rpc_dynamic; do
  echo "== $f"
  timeout -s KILL 60 "$GRAPHIX" --check "$dir/$f.gx" >/dev/null 2>&1
  echo "   check rc=$?"
  mkdir -p "$dir/log_$f"
  out=$(RUST_LOG=warn timeout -s KILL 20 "$GRAPHIX" --no-cache --log-dir "$dir/log_$f" "$dir/$f.gx" 2>&1)
  rc=$?
  echo "   build rc=$rc, last line: $(printf '%s\n' "$out" | tail -n 1)"
  printf '%s\n' "$out" | grep -A1 'panicked at' | sed 's/^/   /'
  if [ -s "$dir/log_$f/graphix.log" ]; then
    grep -o 'did not elaborate.*' "$dir/log_$f/graphix.log" | sed 's/^/   log: /'
  fi
done
echo "(files in $dir)"
