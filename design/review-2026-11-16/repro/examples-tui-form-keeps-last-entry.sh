#!/bin/bash
# Title: input_form's form reopens holding the last submission, focus on
# the last field
#
# The example builds the form once over constant fields and never delivers
# them again, so the form's documented re-seed ("A new `fields` delivery
# re-seeds the editors") never runs: a second `f` shows "Ada"/"Lovelace"
# and typing edits the last field. Delivering the fields on the open
# (`filling ~ [..]` as the positional argument) gives a fresh form each time.
#
# Command: bash design/review-2026-11-16/repro/examples-tui-form-keeps-last-entry.sh
#   (needs tmux; ~25 s)
# Expected: the reopened form is empty with the focus on "first"; X then
#   Enter greets "hello X ".
# Observed (book example): the reopened form shows first: Ada, last:
#   Lovelace, and X then Enter greets "hello Ada LovelaceX". The re-seeding
#   variant shows the expected.
set -u
GX=${GX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
TMP=$(mktemp -d)
SOCK=repro_form_$$
trap 'tmux -L $SOCK kill-server 2>/dev/null; rm -rf "$TMP"' EXIT
export XDG_CACHE_HOME=$TMP/cache
EX=$ROOT/book/src/examples/tui/input_form.gx
sed 's/^  \[{ label: "first"/  filling ~ [{ label: "first"/' "$EX" > "$TMP/reseed.gx"
diff "$EX" "$TMP/reseed.gx"
drive() {
  tmux -L $SOCK new-session -d -s s -x 80 -y 24 \
    "XDG_CACHE_HOME=$XDG_CACHE_HOME timeout --foreground -s KILL 30 $GX --no-cache $1; sleep 30"
  sleep 5
  for k in f Ada; do tmux -L $SOCK send-keys -t s -l $k; sleep 0.4; done
  tmux -L $SOCK send-keys -t s Tab; sleep 0.4
  tmux -L $SOCK send-keys -t s -l Lovelace; sleep 0.4
  tmux -L $SOCK send-keys -t s Enter; sleep 0.4
  tmux -L $SOCK send-keys -t s -l f; sleep 0.6
  echo "-- reopened:"; tmux -L $SOCK capture-pane -p -t s | grep 'first:\|last:'
  tmux -L $SOCK send-keys -t s -l X; sleep 0.4
  tmux -L $SOCK send-keys -t s Enter; sleep 0.6
  echo "-- after X, Enter:"; tmux -L $SOCK capture-pane -p -t s | grep hello
  tmux -L $SOCK kill-server; sleep 0.5
}
echo "== book example"; drive "$EX"
echo "== re-seeding variant"; drive "$TMP/reseed.gx"
