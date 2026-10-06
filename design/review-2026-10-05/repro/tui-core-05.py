#!/usr/bin/env python3
# tui-core-05: tui::suspend hands the child a terminal with the cursor hidden.
#
# Every draw hides the cursor (ratatui-core 0.1.0 terminal.rs:470: no widget
# sets a cursor position). The suspend branch (stdlib/graphix-package-tui/
# src/lib.rs:816-828) calls ratatui::restore(), which only disables raw mode
# and leaves the alternate screen, and keeps the Terminal, whose Drop is what
# shows the cursor. Cursor visibility (DECTCEM, ?25) is a terminal-wide mode
# that ?1049 does not save or restore, so the child runs with it hidden. The
# show arrives on resume, when the old Terminal drops inside the new
# alternate screen. The program below suspends after 1 s, runs
# `sh -c 'echo CHILD-RUNNING; sleep 1'` on the released terminal, resumes on
# its exit and quits at 4 s. The script runs it in a 24x80 pseudo-terminal
# and prints the order of the cursor and alternate-screen escapes around the
# child's output.
#
# command: timeout -s KILL 60 python3 design/review-2026-10-05/repro/tui-core-05.py <graphix>
#   (<graphix> is a built binary, e.g. ~/tmp/target/debug/graphix)
#
# expected: a show (?25h) between leaving the alternate screen and the
#   child's output; "when the child prints: screen=main cursor=visible".
# observed (HEAD c722befe, debug build):
#   event order: alt+, hide x3, alt-, CHILD, alt+, show, hide x3, alt-, show
#     (the hide counts are the draw counts and vary run to run)
#   when the child prints: screen=main cursor=HIDDEN
#   at exit: screen=main cursor=visible
# The same program run in a tmux pane with `printf "Password: "; sleep 3` as
# the child: tmux reports alternate_on=0 cursor_flag=0 for the child's whole
# run. nano, vim and micro re-show the cursor themselves; sh, less and a
# sudo or su password prompt do not.
import fcntl, os, re, select, shlex, shutil, signal, struct, sys, tempfile, termios, time

PROG = '''use tui::paragraph::paragraph;
let suspended = false;
suspended <- sys::time::timer(duration:1.s, false) ~ true;
tui::exit(sys::time::timer(duration:4.s, false));
let s = {
  catch(e) suspended <- e ~ false;
  let released = tui::suspend(suspended)?;
  let go = select released { true => released, false => never() };
  let child = sys::process::spawn(sys::process::options(#args: ["-c", "echo CHILD-RUNNING; sleep 1"], go ~ "sh"))?;
  suspended <- sys::process::wait(child.proc)? ~ false;
  released
};
paragraph(&"suspended: [s]")
'''
EVENTS = {
    b'\x1b[?25l': 'hide',
    b'\x1b[?25h': 'show',
    b'\x1b[?1049h': 'alt+',
    b'\x1b[?1049l': 'alt-',
    b'CHILD-RUNNING': 'CHILD',
}


def run(argv, env, secs=12.0):
    master, slave = os.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 24, 80, 0, 0))
    pid = os.fork()
    if pid == 0:
        os.setsid()
        fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
        for fd in (0, 1, 2):
            os.dup2(slave, fd)
        os.close(master)
        os.execvpe(argv[0], argv, env)
    os.close(slave)
    out = bytearray()
    deadline = time.time() + secs
    while time.time() < deadline:
        r, _, _ = select.select([master], [], [], 0.1)
        if r:
            try:
                b = os.read(master, 65536)
            except OSError:
                b = b''
            if not b:
                break
            out += b
    try:
        os.kill(pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    os.waitpid(pid, 0)
    os.close(master)
    return bytes(out)


tmp = tempfile.mkdtemp()
path = os.path.join(tmp, 'suspend.gx')
with open(path, 'w') as f:
    f.write(PROG)
env = dict(os.environ, TERM='xterm-256color', XDG_CACHE_HOME=tmp)
out = run(shlex.split(sys.argv[1]) + ['--no-cache', path], env)
shutil.rmtree(tmp)
pat = re.compile(b'|'.join(re.escape(k) for k in EVENTS))
evs = [EVENTS[m.group(0)] for m in pat.finditer(out)]
runs = []
for e in evs:
    if runs and runs[-1][0] == e:
        runs[-1][1] += 1
    else:
        runs.append([e, 1])
print('event order:', ', '.join(e if n == 1 else '%s x%d' % (e, n) for e, n in runs))
visible, alt = True, False
for e in evs:
    if e in ('hide', 'show'):
        visible = e == 'show'
    elif e in ('alt+', 'alt-'):
        alt = e == 'alt+'
    else:
        print('when the child prints: screen=%s cursor=%s'
              % ('alternate' if alt else 'main', 'visible' if visible else 'HIDDEN'))
print('at exit: screen=%s cursor=%s'
      % ('alternate' if alt else 'main', 'visible' if visible else 'HIDDEN'))
