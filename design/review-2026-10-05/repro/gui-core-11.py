#!/usr/bin/env python3
# gui-core-11: a second GUI in one process fails: a new winit EventLoop is
# built per session.
#
# event_loop::run (stdlib/graphix-package-gui/src/event_loop.rs:437) builds
# a fresh EventLoop for every GUI session. winit sets a process-wide flag
# in EventLoopBuilder::build before it touches the platform and never
# clears it (winit-0.30.13 src/event_loop.rs:118-120), so every build after
# the first returns EventLoopError::RecreationAttempt, whether the first
# session ran or failed. This script drives the REPL in a pty and
# evaluates the same GUI expression twice.
#
# command (headless, what the review ran; no window can open):
#   env -u WAYLAND_DISPLAY -u DISPLAY -u WAYLAND_SOCKET \
#     python3 design/review-2026-10-05/repro/gui-core-11.py ~/tmp/target/debug/graphix
#
# expected: each evaluation starts its own display, so both fail alike
#   with the no-display error.
# observed (HEAD c722befe, debug build):
#   first:  error: initializing custom display / 0: creating the event loop
#           / 1: os error at .../winit-0.30.13/src/platform_impl/linux/mod.rs:765:
#           neither WAYLAND_DISPLAY nor WAYLAND_SOCKET nor DISPLAY is set.
#   second: error: initializing custom display / 0: creating the event loop
#           / 1: EventLoop can't be recreated
#
# With a display (not run in the review) the first evaluation opens a
# window; close it and the second evaluation prints the same
# "EventLoop can't be recreated" error instead of opening a window.
import os, pty, re, select, sys, time

EXPR = b'[&gui::window(&gui::text::text(&"a"))]'
GRAPHIX = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser(
    "~/tmp/target/debug/graphix")
argv = [GRAPHIX, "--no-netidx", "--no-init", "--no-cache"]
PROMPT = "〉".encode()

pid, fd = pty.fork()
if pid == 0:
    os.execv(argv[0], argv)

buf = bytearray()


def pump(timeout):
    end = time.time() + timeout
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], 0.05)
        if not r:
            continue
        try:
            data = os.read(fd, 65536)
        except OSError:
            return False
        if not data:
            return False
        if b"\x1b[6n" in data:
            os.write(fd, b"\x1b[1;1R")
        buf.extend(data)
    return True


def wait_for(pat, start, timeout):
    end = time.time() + timeout
    while time.time() < end:
        if re.search(pat, bytes(buf[start:]), re.S):
            return True
        if not pump(0.2):
            return False
    return False


wait_for(rb"Welcome to the graphix shell.*" + PROMPT, 0, 60)
for _ in (1, 2):
    start = len(buf)
    os.write(fd, EXPR + b"\r")
    # the next prompt appears once the display has stopped or failed
    wait_for(rb"\n-: .*" + PROMPT, start, 300)
    pump(0.5)
os.write(fd, b"\x04")
status = None
end = time.time() + 15
while time.time() < end:
    pump(0.2)
    wp, st = os.waitpid(pid, os.WNOHANG)
    if wp == pid:
        status = st
        break
if status is None:
    try:
        os.killpg(pid, 9)
    except ProcessLookupError:
        pass
    _, status = os.waitpid(pid, 0)
    buf.extend(b"\n[repro] the repl did not exit on ctrl-d; killed\n")
text = re.sub(rb"\x1b(\[[0-9;?]*[A-Za-z]|[78])", b"", bytes(buf)).replace(b"\r", b"")
sys.stdout.write(text.decode("utf-8", "replace"))
sys.stdout.write("\n[repro] repl exit status %d\n" % os.waitstatus_to_exitcode(status))
