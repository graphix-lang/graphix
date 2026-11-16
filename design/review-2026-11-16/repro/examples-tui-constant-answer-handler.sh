#!/bin/bash
# Title: a handler with a constant answer: harmless for `Stop, loses every
# key for `Continue (the skill's ui.md says the opposite of the first)
#
# ui.md: "a modal one that lets nothing through is |e: Event| { on_press(e,
# keys); e ~ `Stop }. The `e ~` is not optional: a handler whose result is
# a constant answers the first event and then never again ..., and the keys
# after it are lost." Run, the modal with a bare `Stop behaves exactly as
# with `e ~ `Stop`: its keys run (Esc closes it, twice), and an unanswered
# event goes nowhere, as a stopped one does. Where the constant does lose
# keys is a pass-through handler: a bare `Continue passes no event to the
# child, the first one included.
#
# Command: bash design/review-2026-11-16/repro/examples-tui-constant-answer-handler.sh
#   (needs tmux; ~25 s)
# Observed: modal: the layer shows after `o` and is gone after Esc, both
#   times. pass-through: after b, c the inner handler took nothing
#   ("nothing yet"); `a` (taken by the outer callback) still shows.
set -u
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
TMP=$(mktemp -d)
SOCK=repro_const_$$
trap 'tmux -L $SOCK kill-server 2>/dev/null; rm -rf "$TMP"' EXIT
export XDG_CACHE_HOME=$TMP/cache
python3 - "$ROOT" "$TMP" <<'EOF'
import sys
root, tmp = sys.argv[1], sys.argv[2]
def patch(src, old, new, out):
    s = open(f"{root}/book/src/examples/tui/{src}").read()
    assert old in s, src
    open(f"{tmp}/{out}", "w").write(s.replace(old, new))
patch("overlay_modal.gx",
"""let handle_modal = |e: Event| on_press(e, |k| select k.code {
  c@ `Esc => {
    open <- c ~ false;
    `Stop
  },
  _ => `Continue
});""",
"""let handle_modal = |e: Event| {
  on_press(e, |k| select k.code {
    c@ `Esc => { open <- c ~ false; `Stop },
    _ => `Continue
  });
  `Stop
};""", "modal.gx")
patch("input_propagation.gx",
"""let outer = |e: Event| on_press(e, |k| select k.code {
  a@ `Char("a") => {
    last <- a ~ "outer took a";
    `Stop
  },
  _ => `Continue
});""",
"""let outer = |e: Event| {
  on_press(e, |k| select k.code {
    a@ `Char("a") => { last <- a ~ "outer took a"; `Stop },
    _ => `Continue
  });
  `Continue
};""", "passthrough.gx")
EOF
start() {
  tmux -L $SOCK new-session -d -s s -x 80 -y 24 \
    "XDG_CACHE_HOME=$XDG_CACHE_HOME timeout --foreground -s KILL 30 $GX --no-cache $1; sleep 30"
  sleep 5
}
key() { tmux -L $SOCK send-keys -t s "$@"; sleep 0.5; }
shows() { tmux -L $SOCK capture-pane -p -t s | grep -c "$1"; }
echo "== modal with a constant \`Stop"
start "$TMP/modal.gx"
key -l o; echo "after o, layer shown: $(shows 'modal, Esc closes')"
key -l x; key Escape; echo "after x, Esc, layer shown: $(shows 'modal, Esc closes')"
key -l o; echo "after o, layer shown: $(shows 'modal, Esc closes')"
key Escape; echo "after Esc, layer shown: $(shows 'modal, Esc closes')"
tmux -L $SOCK kill-server; sleep 0.5
echo "== pass-through with a constant \`Continue"
start "$TMP/passthrough.gx"
key -l b; key -l c; echo "after b, c: $(tmux -L $SOCK capture-pane -p -t s | head -1)"
key -l a; echo "after a: $(tmux -L $SOCK capture-pane -p -t s | head -1)"
