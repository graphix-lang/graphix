#!/bin/sh
# book-ui-b: ide/skills/graphix-lang/ui.md says a handler whose result is a constant
# "answers the first event and then never again"; it answers no event at all
# (book/src/ui/tui/input.md has it right: "it never fires, so the handler would answer no event").
# command:  sh book-ui-b-const-handler-answers-nothing.sh /path/to/graphix
# expected (ui.md): the first key 'a' is answered `Continue and reaches the inner handler: "a 1"
# observed: the outer handler runs for every key (n counts 1, 2, 3) but the inner one never
#           sees a key: "none 1", "none 2", "none 3"
GX=${1:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(dirname "$0"); T=$(mktemp -d)
cat > $T/c.gx <<'EOF'
use tui::{input_handler::{Event, input_handler, on_press}, paragraph::paragraph};
let last = "none";
let n = 0;
let outer = |e: Event| { n <- e ~ n + 1; `Continue };
let inner = |e: Event| on_press(e, |k| select k.code {
  c@ `Char(s) => { last <- c ~ s; `Stop },
  _ => `Continue
});
input_handler(#handle: &outer, &input_handler(#handle: &inner, &paragraph(&"[last] [n]")))
EOF
timeout -s KILL 60 python3 $D/book-ui-b-tui-drive.py $GX $T/c.gx 40 2 5 'a|b|c|\x03'
rm -rf $T
