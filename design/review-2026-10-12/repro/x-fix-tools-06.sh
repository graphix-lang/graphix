#!/bin/sh
# x-fix-tools-06: no panic containment around a check: one compiler panic during an
# LSP check kills the runtime task, and every later check publishes "runtime is dead".
# Same root and driver as lsp-02 (repro/lsp-02.py); the panic used is the genn.rs
# assert_eq reached by binding 'array_group with a zero-argument callback.
# command: timeout -s KILL 90 sh design/review-2026-10-12/repro/x-fix-tools-06.sh /path/to/graphix
# expected: bad.gx gets an error diagnostic; good.gx gets none; the hover on good.gx answers.
# observed (debug build at 7324207f):
#   DIAG bad.gx  [(0:0, 'runtime did not respond')]
#   DIAG good.gx [(0:0, 'runtime is dead')]
#   RESP 2 None   (hover on good.gx)
#   stderr: panicked at graphix-compiler/src/node/genn.rs:105:5: a generated call supplies every formal
exec python3 -I "$(dirname "$0")/lsp-02.py" "$1"
