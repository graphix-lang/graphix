#!/usr/bin/env python3
# x-errors-01: sys::exit calls process::exit inside a cycle, so a TUI is left
# in the alternate screen with the cursor hidden and the terminal in raw mode;
# a kill_on_drop child also outlives the program, as it does after Ctrl-C and
# tui::exit.
#
# Exit::update (stdlib/graphix-package-sys/src/lib.rs:588-596) flushes
# stdout/stderr and calls std::process::exit from inside the node's update.
# The shell's orderly end (graphix-shell/src/lib.rs:542-547: interrupt,
# output.clear(), abort) never runs, so the TUI display's ratatui::restore
# (stdlib/graphix-package-tui/src/lib.rs:872-877) is skipped. tui::exit
# (stdlib/graphix-package-tui/src/lib.rs:621-626) ends through the shell and
# restores, but it takes no exit code.
#
# A kill_on_drop child is killed only by its own_child task
# (stdlib/graphix-package-sys/src/process.rs:66-104) once every Proc handle is
# gone. process::exit drops nothing. The orderly end does not reach the kill
# either: the task does not get to run when the runtime is torn down, and the
# tokio Command (process.rs:239, its task spawned at 262) is built without
# kill_on_drop, so dropping the task orphans the child. On unix nothing else ties the child to graphix
# (the Job is a no-op there). A terminal's Ctrl-C hides this, because it
# signals the whole foreground process group, the child included. The children
# here ignore SIGHUP so the pty's hangup does not kill them either.
#
# command: timeout -s KILL 300 python3 design/review-2026-10-05/repro/x-errors-01.py <graphix>
#   (<graphix> defaults to `graphix` on PATH)
#
# expected:
#   tui + sys::exit: leaves the alternate screen (ESC[?1049l), shows the
#     cursor (ESC[?25h), and the pty is back in cooked mode (icanon echo)
#   every child case: the kill_on_drop child is gone once graphix has exited
#
# observed (HEAD c722befe, debug build):
#   tui + sys::exit   rc=0 enter-alt=1 leave-alt=0 show-cursor=0 tty after: -opost -isig -icanon -echo
#   tui + tui::exit   rc=0 enter-alt=1 leave-alt=1 show-cursor=1 tty after: opost isig icanon echo
#   child + sys::exit       rc=0 kill_on_drop child alive after graphix exited: True
#   child + tui::exit       rc=0 kill_on_drop child alive after graphix exited: True
#   child + SIGINT (Ctrl-C) rc=0 kill_on_drop child alive after graphix exited: True
import fcntl, os, pty, select, signal, struct, subprocess, sys, tempfile, termios, time

GX = sys.argv[1] if len(sys.argv) > 1 else "graphix"

TUI = """use tui::paragraph::paragraph;
EXIT(sys::time::after_idle(duration:1500.ms, 0));
paragraph(&"hello")
"""

CHILD = """use sys::process::{options, spawn, stdio};
USE
let c = spawn(options(
  #kill_on_drop: true,
  #stdio: stdio(#stdin: `Null, #stdout: `Null, #stderr: `Null),
  #args: ["-c", "trap '' HUP; exec sleep ARG"],
  "sh"
))$;
EXIT
LAST
"""


def children(arg):
    found = []
    for p in os.listdir("/proc"):
        if not p.isdigit():
            continue
        try:
            argv = open(f"/proc/{p}/cmdline", "rb").read().split(b"\0")
        except OSError:
            continue
        if argv[:2] == [b"sleep", arg.encode()]:
            found.append(int(p))
    return found


def graphix_pid(prog):
    for p in os.listdir("/proc"):
        if not p.isdigit():
            continue
        try:
            argv = open(f"/proc/{p}/cmdline", "rb").read().split(b"\0")
        except OSError:
            continue
        if os.path.basename(argv[0]) == b"graphix" and prog.encode() in argv:
            return int(p)
    return None


def in_pty(prog):
    """graphix on prog in a fresh 24x80 pty, then `stty -a` in the same pty."""
    cmd = (f'{GX} --no-cache {prog}; rc=$?; echo; echo "RC=$rc"; '
           'echo STTY_BEGIN; stty -a; echo STTY_END')
    pid, fd = pty.fork()
    if pid == 0:
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
        os.execvp("bash", ["bash", "--norc", "--noprofile", "-c", cmd])
    out, deadline = b"", time.time() + 90
    while time.time() < deadline:
        r, _, _ = select.select([fd], [], [], 0.2)
        if r:
            try:
                chunk = os.read(fd, 65536)
            except OSError:
                break
            if not chunk:
                break
            out += chunk
        elif os.waitpid(pid, os.WNOHANG)[0]:
            break
    else:
        os.killpg(pid, signal.SIGKILL)
        out += b"\nTIMEOUT\n"
    try:
        os.waitpid(pid, 0)
    except ChildProcessError:
        pass
    os.close(fd)
    text = out.decode(errors="replace")
    rc = text.split("RC=")[-1].split()[0] if "RC=" in text else "?"
    stty = text.split("STTY_BEGIN")[-1].split("STTY_END")[0]
    toks = stty.replace(";", " ").split()
    mode = " ".join(t for t in toks if t.lstrip("-") in ("opost", "isig", "icanon", "echo"))
    return rc, out, mode


def tui_case(name, exit_fn, d):
    prog = os.path.join(d, f"{name}.gx")
    open(prog, "w").write(TUI.replace("EXIT", exit_fn))
    rc, out, mode = in_pty(prog)
    enter, leave, show = (out.count(s) for s in (b"\x1b[?1049h", b"\x1b[?1049l", b"\x1b[?25h"))
    label = "tui + " + exit_fn
    print(f"{label:17} rc={rc} enter-alt={enter} leave-alt={leave} show-cursor={show} "
          f"tty after: {mode}")


def child_case(label, arg, d, how):
    prog = os.path.join(d, f"child_{how}.gx")
    trig = "sys::time::after_idle(duration:1500.ms, c.pid ~ 0)"
    src = CHILD.replace("ARG", arg)
    if how == "tui":
        src = src.replace("USE", "use tui::paragraph::paragraph;")
        src = src.replace("EXIT", f"tui::exit({trig});").replace("LAST", 'paragraph(&"pid: [c.pid]")')
    else:
        src = src.replace("USE", "")
        src = src.replace("EXIT", f"sys::exit({trig});" if how == "sys" else "")
        src = src.replace("LAST", "c.pid")
    open(prog, "w").write(src)
    if how == "tui":
        rc = in_pty(prog)[0]
    else:
        with open(prog + ".out", "w") as o, open(prog + ".err", "w") as e:
            p = subprocess.Popen([GX, "--no-cache", prog], stdin=subprocess.DEVNULL,
                                 stdout=o, stderr=e, start_new_session=True)
            if how == "sigint":
                deadline = time.time() + 60
                while not children(arg) and time.time() < deadline:
                    time.sleep(0.2)
                time.sleep(0.5)
                gp = graphix_pid(prog)
                if gp:
                    os.kill(gp, signal.SIGINT)
            try:
                rc = p.wait(timeout=60)
            except subprocess.TimeoutExpired:
                os.killpg(p.pid, signal.SIGKILL)
                rc = "TIMEOUT"
    time.sleep(0.5)
    print(f"{label:23} rc={rc} kill_on_drop child alive after graphix exited: {bool(children(arg))}")


with tempfile.TemporaryDirectory() as d:
    try:
        tui_case("tui_sys_exit", "sys::exit", d)
        tui_case("tui_tui_exit", "tui::exit", d)
        child_case("child + sys::exit", "41.0101", d, "sys")
        child_case("child + tui::exit", "41.0202", d, "tui")
        child_case("child + SIGINT (Ctrl-C)", "41.0303", d, "sigint")
    finally:
        for arg in ("41.0101", "41.0202", "41.0303"):
            for pid in children(arg):
                os.kill(pid, signal.SIGKILL)
