#!/usr/bin/env bash
# c-lib-05: the REPL with no controlling terminal busy-loops forever
# printing `error: No such device or address (os error 6)`.
#
# Shell::run (graphix-shell/src/lib.rs:490) prints an Err from
# InputReader::read_line and goes round again when not in script mode.
# With no controlling terminal and stdin not a tty, reedline's read_line
# fails at once on every call (crossterm's enable_raw_mode opens /dev/tty:
# ENXIO), so the loop never blocks, never sees EOF and never exits. This
# is how `graphix` with no file runs under ssh without -t, CI, cron, a
# systemd unit or `docker run` without -t. With a controlling terminal
# reedline reads keys from /dev/tty and redirected stdin is ignored, so
# the script detaches from the terminal (setsid -w) when it has one.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/c-lib-05.sh
#
# expected: each case ends on its own at stdin's EOF (or refuses at once
# with one clear message): exit status 0 or 1, a handful of lines.
#
# observed (HEAD c722befe, debug build, no controlling terminal):
#   stdin /dev/null: exit 137 (killed by the 3 s timeout), 187609 lines:
#     the two welcome lines, then 187607 x
#     `error: No such device or address (os error 6)`
#   stdin a pipe ('1 + 1'): exit 137, 186453 lines, the same error;
#     the piped line is never compiled (no `-:` line)
#   the process runs at ~143% CPU the whole time (ps, sampled at 5 s).
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
detach=()
if (: < /dev/tty) 2>/dev/null; then detach=(setsid -w); fi
report() {
    echo "$1: exit $2, $(wc -l < "$dir/out") lines"
    sort "$dir/out" | uniq -c | sort -rn | head -3
}
"${detach[@]}" timeout -s KILL 3 "$GRAPHIX" --no-netidx --no-cache \
    < /dev/null > "$dir/out" 2>&1
report "stdin /dev/null" $?
printf '1 + 1\n' | "${detach[@]}" timeout -s KILL 3 "$GRAPHIX" --no-netidx --no-cache \
    > "$dir/out" 2>&1
report "stdin a pipe ('1 + 1')" "${PIPESTATUS[1]}"
