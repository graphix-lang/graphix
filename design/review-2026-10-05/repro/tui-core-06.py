#!/usr/bin/env python3
# tui-core-06: SIGTERM leaves the terminal in raw mode, on the alternate
# screen, cursor hidden (and mouse capture on).
#
# The shell installs a handler for SIGINT only (graphix-shell/src/lib.rs:449-456),
# and the TUI display's select (stdlib/graphix-package-tui/src/lib.rs:795)
# restores the terminal only on the exits it sees there. SIGTERM and SIGHUP take
# their default action, so no Rust code runs and nothing is restored.
#
# The script runs each case in a 24x80 pseudo-terminal:
#   A  a TUI program (book/src/examples/tui/block_basic.gx plus the mouse turned
#      on), `kill <pid>` (SIGTERM) 3 s after it entered the alternate screen
#   B  the same, SIGHUP
#   C  control for A: the Ctrl-C key instead
#   D  a TUI program that suspends itself (tui::suspend), runs a graphix TUI
#      child with inherited stdio as the tui::suspend doc says, and stops it at
#      9 s with sys::process::kill(#grace: 2.s, ..), which is SIGTERM first; the
#      parent resumes and is ended by the Ctrl-C key at 16 s
#   E  control for D: the child ends itself with tui::exit at 4 s
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/tui-core-06.py <graphix>
#   (<graphix> is the binary itself, e.g. ~/tmp/target/debug/graphix, not a
#   wrapper script: the signals must reach graphix, and D's child is <graphix>)
#
# expected: every case ends with ICANON and ECHO on, the alternate screen left
#   and the cursor shown; A and B also turn the mouse capture off.
# observed (HEAD c722befe, debug build; "after the alternate screen" counts the
# sequences written after the first ESC[?1049h):
#   A SIGTERM: exit -15; ICANON=False ECHO=False OPOST=False; after the alternate screen: alt-off 0, cursor-show 0, mouse-on 1, mouse-off 0
#   B SIGHUP:  exit -1; ICANON=False ECHO=False OPOST=False; after the alternate screen: alt-off 0, cursor-show 0, mouse-on 1, mouse-off 0
#   C Ctrl-C:  exit 0; ICANON=True ECHO=True OPOST=True; after the alternate screen: alt-off 1, cursor-show 1, mouse-on 1, mouse-off 1
#   D kill:    exit 0; ICANON=False ECHO=False OPOST=False; after the alternate screen: alt-off 2, cursor-show 2, mouse-on 1, mouse-off 0
#   E self:    exit 0; ICANON=True ECHO=True OPOST=True; after the alternate screen: alt-off 3, cursor-show 3, mouse-on 0, mouse-off 0
# In D the child died at 9 s without restoring anything; the parent's resume
# (ratatui::try_init) took the child's raw mode as the mode to restore, so the
# parent's own clean exit leaves the terminal raw and the child's mouse capture
# on. An interactive bash adopts the tty state a job leaves on a normal exit.
# Under an interactive bash 5.3 or fish 4.9 that sees the signal death (case A),
# the shell resets termios (fish also shows the cursor), but neither leaves the
# alternate screen or turns the mouse capture off: the prompt comes back on the
# alternate screen and every mouse move types an escape sequence into it.
import fcntl, os, re, select, shutil, signal, struct, sys, tempfile, termios, time

ALT_ON, ALT_OFF = b'\x1b[?1049h', b'\x1b[?1049l'
CUR_SHOW = b'\x1b[?25h'
MOUSE_ON, MOUSE_OFF = b'\x1b[?1003h', b'\x1b[?1003l'

TUI = '''use tui::{line, block::block, paragraph::paragraph};
tui::mouse <- sys::time::timer(duration:200.ms, false) ~ true;
block(#border: &`All, #title: &line("My Block"), &paragraph(&"Hello, World!"))
'''

CHILD_EXITS = '''use tui::{line, block::block, paragraph::paragraph};
tui::exit(sys::time::timer(duration:4.s, false));
block(#border: &`All, #title: &line("My Block"), &paragraph(&"Hello, World!"))
'''

PARENT = '''use tui::paragraph::paragraph;
let suspended = false;
suspended <- sys::time::timer(duration:1.s, false) ~ true;
let s = {
  catch(e) { println("could not: [e]"); suspended <- e ~ false };
  let released = tui::suspend(suspended)?;
  let go = select released { true => released, false => never() };
  let child = sys::process::spawn(sys::process::options(#args: ["--no-cache", "CHILD"], go ~ "GRAPHIX"))?;
  sys::process::kill(#grace: duration:2.s, sys::time::timer(duration:9.s, false) ~ child.proc);
  let status = sys::process::wait(child.proc)?;
  suspended <- status ~ false;
  released
};
paragraph(&"parent: released=[s]")
'''


def descendants(pid):
    out, todo = [], [pid]
    while todo:
        p = todo.pop()
        try:
            for t in os.listdir(f'/proc/{p}/task'):
                with open(f'/proc/{p}/task/{t}/children') as f:
                    for c in f.read().split():
                        out.append(int(c))
                        todo.append(int(c))
        except OSError:
            pass
    return out


def run(graphix, prog, action, at):
    """Run graphix in a pty; `action` ('SIGTERM', 'SIGHUP' or 'CTRLC') fires
    `at` seconds after the alternate screen first appears."""
    master, slave = os.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 80, 0, 0))
    pid = os.fork()
    if pid == 0:
        os.setsid()
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
        for fd in (0, 1, 2):
            os.dup2(slave, fd)
        os.close(master)
        os.close(slave)
        os.execv(graphix, [graphix, '--no-cache', prog])
    out, status, alt_at, done = bytearray(), None, None, False
    start = time.time()
    while time.time() - start < 40:
        r, _, _ = select.select([master], [], [], 0.05)
        if r:
            try:
                out += os.read(master, 65536)
            except OSError:
                pass
        if alt_at is None and ALT_ON in out:
            alt_at = time.time()
        if not done and alt_at is not None and time.time() - alt_at >= at:
            done = True
            if action == 'CTRLC':
                os.write(master, b'\x03')
            else:
                os.kill(pid, getattr(signal, action))
        p, st = os.waitpid(pid, os.WNOHANG)
        if p == pid:
            status = st
            break
    attr = termios.tcgetattr(slave)
    for p in descendants(pid) + [pid]:
        try:
            os.kill(p, signal.SIGKILL)
        except OSError:
            pass
    if status is None:
        _, status = os.waitpid(pid, 0)
        code = 'none, still running at 40 s'
    else:
        code = os.waitstatus_to_exitcode(status)
    os.close(master)
    os.close(slave)
    tail = bytes(out[out.find(ALT_ON):]) if ALT_ON in out else b''
    lflag, oflag = attr[3], attr[1]
    return (f'exit {code}; ICANON={bool(lflag & termios.ICANON)} ECHO={bool(lflag & termios.ECHO)} '
            f'OPOST={bool(oflag & termios.OPOST)}; after the alternate screen: '
            f'alt-off {tail.count(ALT_OFF)}, cursor-show {tail.count(CUR_SHOW)}, '
            f'mouse-on {tail.count(MOUSE_ON)}, mouse-off {tail.count(MOUSE_OFF)}')


def main():
    graphix = shutil.which(sys.argv[1]) or sys.argv[1]
    graphix = os.path.abspath(graphix)
    d = tempfile.mkdtemp(prefix='tui-core-06-')
    try:
        def write(name, text):
            path = os.path.join(d, name)
            with open(path, 'w') as f:
                f.write(text)
            return path

        tui = write('tui.gx', TUI)
        killed = write('parent_kill.gx', PARENT.replace('CHILD', write('child.gx', TUI))
                       .replace('GRAPHIX', graphix))
        exits = write('parent_self.gx', PARENT.replace('CHILD', write('child_exits.gx', CHILD_EXITS))
                      .replace('GRAPHIX', graphix))
        print('A SIGTERM:', run(graphix, tui, 'SIGTERM', 3))
        print('B SIGHUP: ', run(graphix, tui, 'SIGHUP', 3))
        print('C Ctrl-C: ', run(graphix, tui, 'CTRLC', 3))
        print('D kill:   ', run(graphix, killed, 'CTRLC', 16))
        print('E self:   ', run(graphix, exits, 'CTRLC', 16))
    finally:
        shutil.rmtree(d, ignore_errors=True)


main()
