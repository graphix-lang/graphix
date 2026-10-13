#!/usr/bin/env bash
# gui-core-01: Gui::clear hangs forever when the event loop already exited
# (closing the last window).
#
# On a private headless display (kwin_wayland --virtual hosting a rootful
# Xwayland; nothing opens on the desktop) each round starts graphix with one
# window, sends it WM_DELETE_WINDOW (what a WM close button sends) and waits
# 8 s for graphix to exit; a round that hangs is then sent SIGINT (Ctrl-C).
# GC01_GDB=1 runs graphix under gdb with dprintfs on GuiHandler::finish,
# Gui::clear and the store of the EventLoop back into EVENT_LOOP after
# run_app_on_demand returns, and prints that trace per round.
#
# Needs kwin_wayland (KDE 6), Xwayland, dbus-run-session, python-xlib (and gdb
# and nm for GC01_GDB=1). GRAPHIX may be a wrapper script around the binary.
#   GC01_ROUNDS=8 GRAPHIX=/path/to/graphix timeout -s KILL 178 bash gui-core-01.sh
#   GC01_GDB=1 GC01_ROUNDS=6 GRAPHIX=/path/to/graphix timeout -s KILL 178 bash gui-core-01.sh
#
# expected: every round "exited rc 0" (the program's only window closed).
# observed (HEAD 7324207f, debug build):
#   plain:  RESULT: 4 of 8 rounds hung after their last window closed
#           (5 of 8 in another run), each hung one also ignoring SIGINT:
#     round 0 STILL RUNNING 8s after its last window closed (hang) windows now: []
#         after SIGINT (Ctrl-C) to pid 758010: still running
#   GC01_GDB=1: 2 of 3 rounds hung (6 of 6 in another run); a round that
#   exits traces
#     GuiHandler::finish | Gui::clear called (sends ToGui::Stop, awaits rx) |
#     GuiHandler::finish | run_app_on_demand returned, EventLoop stored back ...
#   and every round that hangs traces
#     GuiHandler::finish | Gui::clear called (sends ToGui::Stop, awaits rx) |
#     run_app_on_demand returned, EventLoop stored back in EVENT_LOOP
#   In a hung round finish ran once: the shell's ToGui::Stop was never handled (handling it
#   calls finish again); it sits in winit's user-event queue inside the
#   EventLoop that EVENT_LOOP keeps alive, so Gui::clear's oneshot sender is
#   never dropped and rx.await never resolves.
set -euo pipefail

GRAPHIX=$(command -v "${GRAPHIX:-$HOME/tmp/target/debug/graphix}")
WORK=${WORK:-$(mktemp -d /tmp/gxrepro-gc01.XXXXXX)}
RTDIR=${RTDIR:-$(mktemp -d /tmp/gxrt.XXXXXX)}
mkdir -p "$WORK/home" "$RTDIR"
chmod 700 "$RTDIR"
rm -f "$WORK/session.log"

if [ "${GC01_GDB:-0}" = 1 ]; then
    REAL=$(readlink -f "$(grep -o '[^ ]*/real/graphix' "$GRAPHIX" 2>/dev/null | head -1 || true)")
    [ -x "$REAL" ] || REAL=$(readlink -f "$GRAPHIX")
    sym() { nm "$REAL" | awk -v p="$1" '!f && $2=="T" && $3 ~ p {print $3; f=1}'; }
    FINISH=$(sym '10GuiHandler.*NoExt.*6finish')
    CLEAR=$(sym '3Gui.*NoExt.*CustomDisplay.*5clear')
    STORED=$(sym 'LocalKey.*ToGui.*event_loop3run.*NoExtEs1_0u')
    cat > "$WORK/gxgdb" <<EOF2
#!/bin/bash
exec gdb -q -batch -nx -ex 'set debuginfod enabled off' -ex 'set pagination off' \\
 -ex 'set print thread-events off' -ex 'set print inferior-events off' \\
 -ex 'dprintf $FINISH,"MARK GuiHandler::finish\\n"' \\
 -ex 'dprintf $CLEAR,"MARK Gui::clear called (sends ToGui::Stop, awaits rx)\\n"' \\
 -ex 'dprintf $STORED,"MARK run_app_on_demand returned, EventLoop stored back in EVENT_LOOP\\n"' \\
 -ex run --args $REAL "\$@"
EOF2
    chmod +x "$WORK/gxgdb"
    GRAPHIX="$WORK/gxgdb"
fi

cat > "$WORK/prog.gx" <<'EOF'
use gui::{window, text::text};

[&window(#title: &"gxprobe-main", #size: &mut {width: 300.0, height: 120.0}, &text(&"hello"))]
EOF

cat > "$WORK/session.py" <<'EOF'
#!/usr/bin/env python3
import os, signal, subprocess, threading, time
from Xlib import X, display, protocol, error as xerror

W = os.environ['GXW']
out = open(W + '/session.log', 'w')
t0 = time.time()
def log(*a):
    print('[%6.2fs]' % (time.time() - t0), *a, file=out, flush=True)

r, w = os.pipe()
xenv = dict(os.environ); xenv.pop('DISPLAY', None)
xw = subprocess.Popen(['/usr/bin/Xwayland', '-displayfd', str(w), '-geometry', '1024x768',
                       '-nolisten', 'tcp', '-noreset'],
                      pass_fds=(w,), env=xenv, stdout=open(W + '/xwl.out', 'w'),
                      stderr=subprocess.STDOUT)
os.close(w)
num = os.read(r, 64).decode().strip()
assert num and num != '0', 'refusing display :' + num
DISP = ':' + num
log('private rootful Xwayland on', DISP)

genv = dict(os.environ)
for k in ('WAYLAND_DISPLAY', 'WAYLAND_SOCKET', 'XDG_CONFIG_HOME', 'XDG_DATA_HOME',
          'XDG_STATE_HOME', 'QT_QPA_PLATFORM', 'DBUS_SESSION_BUS_ADDRESS'):
    genv.pop(k, None)
genv.update(DISPLAY=DISP, HOME=os.environ['ORIG_HOME'], XDG_RUNTIME_DIR=os.environ['ORIG_RT'],
            XDG_CACHE_HOME=W + '/gxcache')
assert genv['DISPLAY'] != ':0' and 'WAYLAND_DISPLAY' not in genv
def launch(rnd):
    return subprocess.Popen([os.environ['GXBIN'], '--no-cache', os.environ['GXPROG']], env=genv,
                            stdout=open(W + '/round%d.out' % rnd, 'w'),
                            stderr=open(W + '/graphix.err', 'a'),
                            start_new_session=True)
gx = None

def cleanup():
    if gx is not None:
        try:
            os.killpg(gx.pid, signal.SIGKILL); gx.wait(timeout=5)
        except ProcessLookupError:
            pass
    xw.terminate()
    try:
        xw.wait(timeout=5)
    except subprocess.TimeoutExpired:
        xw.kill()

def watchdog():
    time.sleep(168)
    log('WATCHDOG: giving up')
    cleanup()
    os._exit(4)
threading.Thread(target=watchdog, daemon=True).start()

def children(pid):
    out = []
    try:
        for c in open('/proc/%d/task/%d/children' % (pid, pid)).read().split():
            out.append(int(c)); out.extend(children(int(c)))
    except OSError:
        pass
    return out

d = display.Display(DISP)
root = d.screen().root
NET_WM_NAME = d.intern_atom('_NET_WM_NAME')
UTF8 = d.intern_atom('UTF8_STRING')
WM_PROTOCOLS = d.intern_atom('WM_PROTOCOLS')
WM_DELETE = d.intern_atom('WM_DELETE_WINDOW')

def title(win):
    try:
        p = win.get_full_property(NET_WM_NAME, UTF8)
        if p is not None and p.value:
            v = p.value
            return v.decode() if isinstance(v, bytes) else str(v)
        n = win.get_wm_name()
        return n.decode() if isinstance(n, bytes) else n
    except xerror.XError:
        return None

def find():
    found = []
    def walk(win):
        try:
            cs = win.query_tree().children
        except xerror.XError:
            return
        for c in cs:
            t = title(c)
            if t and t.startswith('gxprobe-'):
                try:
                    if c.get_attributes().map_state == X.IsViewable:
                        found.append(c)
                except xerror.XError:
                    pass
            walk(c)
    walk(root)
    return found

hangs = 0
rounds = int(os.environ.get('GC01_ROUNDS', '6'))
for rnd in range(rounds):
    gx = launch(rnd)
    end = time.time() + 60
    ws = []
    while time.time() < end and not ws and gx.poll() is None:
        ws = find(); time.sleep(0.1)
    if not ws:
        log('round', rnd, 'no window'); break
    time.sleep(0.5)
    ev = protocol.event.ClientMessage(window=ws[0].id, client_type=WM_PROTOCOLS,
                                      data=(32, [WM_DELETE, X.CurrentTime, 0, 0, 0]))
    ws[0].send_event(ev, event_mask=X.NoEventMask); d.flush()
    end = time.time() + 8
    while time.time() < end and gx.poll() is None:
        time.sleep(0.1)
    hung = gx.poll() is None
    log('round', rnd, 'STILL RUNNING 8s after its last window closed (hang)' if hung
        else 'exited rc %s' % gx.returncode, 'windows now:', [hex(w.id) for w in find()])
    if hung:
        hangs += 1
        if os.environ.get('GC01_SIGINT') == '1':
            real = ([gx.pid] + children(gx.pid))[-1]
            os.kill(real, signal.SIGINT)
            end = time.time() + 3
            while time.time() < end and gx.poll() is None:
                time.sleep(0.1)
            log('    after SIGINT (Ctrl-C) to pid %d:' % real,
                'still running' if gx.poll() is None else 'exited rc %s' % gx.returncode)
    for c in reversed(children(gx.pid)):
        try:
            os.kill(c, signal.SIGKILL)
        except ProcessLookupError:
            pass
    try:
        os.killpg(gx.pid, signal.SIGKILL); gx.wait(timeout=5)
    except ProcessLookupError:
        pass
    time.sleep(0.3)
    marks = [l.strip() for l in open(W + '/round%d.out' % rnd) if l.startswith('MARK')]
    if marks:
        log('    trace:', ' | '.join(m[5:] for m in marks))
log('RESULT: %d of %d rounds hung after their last window closed' % (hangs, rounds))
cleanup()
EOF
chmod +x "$WORK/session.py"

env -i PATH=/usr/local/bin:/usr/bin:/bin LANG=C.UTF-8 \
    HOME="$WORK/home" XDG_CONFIG_HOME="$WORK/home/.config" \
    XDG_DATA_HOME="$WORK/home/.local/share" XDG_STATE_HOME="$WORK/home/.local/state" \
    XDG_CACHE_HOME="$WORK/home/.cache" XDG_RUNTIME_DIR="$RTDIR" \
    GXW="$WORK" GC01_ROUNDS="${GC01_ROUNDS:-6}" GC01_SIGINT="$([ "${GC01_GDB:-0}" = 1 ] && echo 0 || echo 1)" GXBIN="$GRAPHIX" GXPROG="$WORK/prog.gx" \
    ORIG_HOME="$HOME" ORIG_RT="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}" \
    dbus-run-session -- kwin_wayland --virtual --socket gxrepro01 --no-lockscreen \
    --no-global-shortcuts --no-kactivities --width 1024 --height 768 \
    --exit-with-session "$WORK/session.py" > "$WORK/kwin.out" 2>&1 || true
cat "$WORK/session.log"
echo "--- graphix stderr (tail):"; tail -20 "$WORK/graphix.err" 2>/dev/null || true
