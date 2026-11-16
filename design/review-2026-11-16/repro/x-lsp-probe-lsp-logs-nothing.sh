#!/bin/bash
# `graphix lsp` installs no logger, so every log::* the server writes is
# dropped: the malformed-frame warning, "unhandled request", "a check
# panicked", "no URI for ..": an editor's server log stays empty.
#
# Run: bash design/review-2026-11-16/repro/x-lsp-probe-lsp-logs-nothing.sh
#      (GRAPHIX=/path/to/graphix to pick the binary)
# Expected: with RUST_LOG=trace, stderr shows at least the warning
#   "ignored a frame that is no JSON-RPC message" and "unhandled request: foo/bar".
# Observed (HEAD e1be2fbf2): stderr is empty; the server answers and exits 0.
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
frame() { printf 'Content-Length: %d\r\n\r\n%s' "${#1}" "$1"; }
err=$(mktemp)
{
  frame '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"capabilities":{}}}'
  frame '{"jsonrpc":"2.0","method":"initialized","params":{}}'
  frame 'not json at all'
  frame '{"jsonrpc":"2.0","id":2,"method":"foo/bar","params":{}}'
  frame '{"jsonrpc":"2.0","id":3,"method":"shutdown","params":null}'
  frame '{"jsonrpc":"2.0","method":"exit","params":null}'
} | RUST_LOG=trace timeout -s KILL 60 "$GRAPHIX" lsp 2>"$err" | tr -d '\r' | grep -o '"id":[0-9]*,"error":{[^}]*}'
echo "exit: ${PIPESTATUS[1]}"
echo "stderr bytes: $(wc -c <"$err")"
cat "$err"
rm -f "$err"
