#!/bin/bash
# Title: a widget tree that fails to build leaves a blank TUI and no message (logged off FAILURE_TARGET)
# Command: bash design/review-2026-11-16/repro/tui-core-hidden-widget-error.sh
# The handler's #[native] body cannot fuse; the check and the build accept the program (the
# attribute waits for an instance), and the instance is built when the display compiles the
# input_handler's callable, which fails. display() logs it with error!("error handling update ..")
# under the crate's own target, which the shell keeps off unless --log-dir and RUST_LOG are given.
# Expected: the failure reaches stderr (as the GUI's widget failures do, under FAILURE_TARGET),
#   or the display ends with it.
# Observed: the screen stays blank ("presses 0" is never drawn), Ctrl-C exits 130, nothing is
#   printed. With `--log-dir d` and RUST_LOG=error, d/graphix.log holds:
#   ERROR [graphix_package_tui] error handling update invalid widget specification
#     ... #[native] expression did not fully fuse to native code: - connect is an effect ...
#   The same program without #[native] draws "presses 0", then "presses 1" after a key.
here=$(dirname "$0")
G=${G:-/home/eric/tmp/review-2026-11-16/bin/graphix}
d=$(mktemp -d)
cat > "$d/native_handler.gx" <<'GX'
use tui::{input_handler::input_handler, text::text};
let n = 0;
let handle = |e: tui::input_handler::Event| #[native] { n <- e ~ n + 1; e ~ `Continue };
input_handler(#handle: &handle, &text(&"presses [n]"))
GX
"$G" --check "$d/native_handler.gx" && echo "check: ok"
TAIL=600 timeout -s KILL 30 python3 "$here/tui-core-ptyrun.py" --wait 6 --keys '2:a' --keys '4:\x03' \
  -- "$G" --no-cache "$d/native_handler.gx"
