#!/usr/bin/env python3
# tui-core-03: swapping the input handler while a call is in flight wedges
# the input handler permanently.
#
# InputHandlerW::set_handle (stdlib/graphix-package-tui/src/input_handler.rs:411)
# calls GXHandle::update_callable, which compiles a Callable for the new
# lambda and drops the old one (DeleteCallable); `pending` stays true. The
# reply to the in-flight event comes back, if at all, under the old
# callable's expr, which handle_update (448-449) no longer matches, so
# `pending` is never cleared, maybe_send_queued (399) never sends again,
# and every later event is queued forever.
#
# The program: `h = select mode { `A => fa, `B => fb }`; both handlers flip
# `mode` on 'm' and count other keys (+1 under fa, +10 under fb). The
# script runs it in a private tmux server, types, and reads the screen:
#   spaced  'm', 'a', 'x', 'x', one key per second
#   burst   'ma' in one write (fast typing, key repeat or a paste),
#           then 'x', 'x' one per second
#
# command: timeout -s KILL 120 python3 design/review-2026-10-05/repro/tui-core-03.py <graphix>
#
# expected: in the burst run the two later 'x' keys reach fb, +10 each
#   (count 21, or 30 if the in-flight 'a' is redelivered to fb).
# observed (HEAD c722befe, debug build; same with --no-fusion and GRAPHIX_PAR=off):
#   spaced  m: count: 0 mode: B   a: 10   x: 20   x: 30
#   burst   ma: count: 1 mode: B  x: count: 1 mode: B  x: count: 1 mode: B
#   ("a" went to fa before the swap landed; nothing after it is handled).
#   With --log-dir and RUST_LOG=graphix_package_tui=debug the burst run logs
#   "sending event" for 'm' and 'a' only. Controls: the same burst with one
#   constant handler that reads `mode` inside handles every key, and a
#   burst with no 'm' ("aaaa") through the swapping program counts 4.
import os, subprocess, sys, tempfile, time

PROG = r"""
use tui::{paragraph::paragraph, input_handler::{Event, input_handler, on_press}};
let n = 0;
let mode: [`A, `B] = `A;
let fa = |e: Event| on_press(e, |k| select k.code {
  c@ `Char("m") => { mode <- c ~ `B; `Stop },
  c@ `Char(_) => { n <- c ~ n + 1; `Stop },
  _ => `Continue
});
let fb = |e: Event| on_press(e, |k| select k.code {
  c@ `Char("m") => { mode <- c ~ `A; `Stop },
  c@ `Char(_) => { n <- c ~ n + 10; `Stop },
  _ => `Continue
});
let h = select mode { `A => fa, `B => fb };
input_handler(#handle: &h, &paragraph(&"count: [n] mode: [mode]"))
"""


def run(gx, d, name, steps):
    sock = os.path.join(d, "tmux-" + name)
    prog = os.path.join(d, "swap.gx")

    def tmux(*a):
        return subprocess.run(["tmux", "-S", sock, *a], capture_output=True, text=True)

    def line():
        for l in tmux("capture-pane", "-p").stdout.splitlines():
            if "count" in l:
                return l.strip()
        return "<not drawn>"

    try:
        tmux("new-session", "-d", "-x", "100", "-y", "12",
             "env XDG_CACHE_HOME=%s timeout -s KILL 40 %s --no-cache %s 2>/dev/null"
             % (d, gx, prog))
        t0 = time.time()
        while line() == "<not drawn>" and time.time() - t0 < 30:
            time.sleep(0.2)
        time.sleep(0.5)
        print("%-7s start: %s" % (name, line()))
        for keys in steps:
            tmux("send-keys", "-l", keys)
            time.sleep(1.0)
            print("%-7s %-5s  %s" % (name, keys, line()))
        return line()
    finally:
        tmux("kill-server")


def main():
    gx = os.path.abspath(sys.argv[1]) if len(sys.argv) > 1 else "graphix"
    with tempfile.TemporaryDirectory() as d:
        with open(os.path.join(d, "swap.gx"), "w") as f:
            f.write(PROG)
        run(gx, d, "spaced", ["m", "a", "x", "x"])
        last = run(gx, d, "burst", ["ma", "x", "x"])
    wedged = "count: 1 " in last
    print("WEDGED: the handler took no key after the swap" if wedged else "ok")
    sys.exit(1 if wedged else 0)


main()
