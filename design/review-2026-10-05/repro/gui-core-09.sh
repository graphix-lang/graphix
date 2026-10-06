#!/usr/bin/env bash
# gui-core-09: a window the user closed reopens the next time the root
# array fires.
#
# CloseRequested (stdlib/graphix-package-gui/src/event_loop.rs:203) drops
# the window from the loop's maps only; the program's Array<&Window> still
# lists it and nothing tells the program. reconcile_windows (event_loop.rs
# :536) recreates every listed bind id missing from `windows`, so the next
# root update brings every user-closed window back.
#
# The program below shows gxprobe-main and gxprobe-a, and adds gxprobe-b
# to the root array when a flag file appears (the reviewer's toggle). The
# driver closes gxprobe-a as a window manager's close button does (a
# WM_DELETE_WINDOW client message), waits, then creates the flag.
# Everything runs on a private headless display: kwin_wayland --virtual
# (own D-Bus session, own XDG dirs) with a rootful Xwayland inside it.
# Nothing opens on the desktop. Needs kwin_wayland (KDE 6), Xwayland,
# dbus-run-session and python-xlib.
#
# command (GRAPHIX defaults to ~/tmp/target/debug/graphix):
#   timeout -s KILL 175 bash design/review-2026-10-05/repro/gui-core-09.sh
#
# expected: after the toggle the windows are gxprobe-b and gxprobe-main;
#   gxprobe-a stays closed.
# observed (HEAD c722befe, debug build):
#   1. program started, windows: [('gxprobe-a', '0x400014'), ('gxprobe-main', '0x400002')]
#   3. window a closed, windows: [('gxprobe-main', '0x400002')]
#   4. 2s later, windows: [('gxprobe-main', '0x400002')] graphix running: True
#   6. after the toggle, windows: [('gxprobe-a', '0x400026'), ('gxprobe-b', '0x400038'), ('gxprobe-main', '0x400002')]
#   RESULT: BUG - the closed window gxprobe-a reopened (old id 0x400014, new id 0x400026)
set -euo pipefail

GRAPHIX=$(command -v "${GRAPHIX:-$HOME/tmp/target/debug/graphix}")
WORK=${WORK:-$(mktemp -d /tmp/gxrepro-gc09.XXXXXX)}
# short: it holds the private compositor's wayland socket (108-byte limit)
RTDIR=${RTDIR:-$(mktemp -d /tmp/gxrt.XXXXXX)}
mkdir -p "$WORK/home" "$RTDIR"
chmod 700 "$RTDIR"
rm -f "$WORK/flag-toggle" "$WORK/session.log"

sed "s#FLAGPATH#$WORK/flag-toggle#" > "$WORK/reopen.gx" <<'EOF'
use gui::{window, text::text};

let tick = sys::time::timer(duration:200.ms, true);
let flag = sys::fs::is_file(tick ~ "FLAGPATH");
let extra = false;
extra <- uniq(select flag { error as _ => never(), _ => true });
println(extra ~ "extra=[extra]");
let wm = &window(#title: &"gxprobe-main", &text(&"main"));
let wa = &window(#title: &"gxprobe-a", &text(&"a"));
let wb = &window(#title: &"gxprobe-b", &text(&"b"));
select extra { true => [wm, wa, wb], false => [wm, wa] }
EOF

cat > "$WORK/session.py" <<'EOF'
#!/usr/bin/env python3
import os, signal, subprocess, threading, time
from Xlib import X, display, protocol, error as xerror

W = os.environ['GXW']
FLAG = W + '/flag-toggle'
out = open(W + '/session.log', 'w')
t0 = time.time()
def log(*a):
    print('[%6.2fs]' % (time.time() - t0), *a, file=out, flush=True)

r, w = os.pipe()
xenv = dict(os.environ); xenv.pop('DISPLAY', None)
xw = subprocess.Popen(['/usr/bin/Xwayland', '-displayfd', str(w), '-geometry', '1600x1200',
                       '-nolisten', 'tcp', '-noreset'],
                      pass_fds=(w,), env=xenv, stdout=open(W + '/xwl.out', 'w'),
                      stderr=subprocess.STDOUT)
os.close(w)
num = os.read(r, 64).decode().strip()
assert num and num != '0', 'refusing display :' + num
DISP = ':' + num
log('private rootful Xwayland on', DISP, 'inside kwin wayland socket', os.environ.get('WAYLAND_DISPLAY'))

genv = dict(os.environ)
for k in ('WAYLAND_DISPLAY', 'WAYLAND_SOCKET', 'XDG_CONFIG_HOME', 'XDG_DATA_HOME',
          'XDG_STATE_HOME', 'QT_QPA_PLATFORM', 'DBUS_SESSION_BUS_ADDRESS'):
    genv.pop(k, None)
genv.update(DISPLAY=DISP, HOME=os.environ['ORIG_HOME'], XDG_RUNTIME_DIR=os.environ['ORIG_RT'],
            XDG_CACHE_HOME=W + '/gxcache')
if os.environ.get('ORIG_DBUS'):
    genv['DBUS_SESSION_BUS_ADDRESS'] = os.environ['ORIG_DBUS']
assert genv['DISPLAY'] != ':0' and 'WAYLAND_DISPLAY' not in genv
gx = subprocess.Popen([os.environ['GXBIN'], '--no-cache', os.environ['GXPROG']], env=genv,
                      stdout=subprocess.PIPE, stderr=open(W + '/graphix.err', 'w'), text=True,
                      start_new_session=True)
log('graphix started, pid', gx.pid)
stdout_lines = []
def reader():
    for line in gx.stdout:
        stdout_lines.append(line.rstrip())
        log('graphix stdout:', line.rstrip())
threading.Thread(target=reader, daemon=True).start()

def cleanup():
    try:
        os.killpg(gx.pid, signal.SIGTERM)
        try:
            gx.wait(timeout=5)
        except subprocess.TimeoutExpired:
            os.killpg(gx.pid, signal.SIGKILL); gx.wait(timeout=5)
    except ProcessLookupError:
        pass
    xw.terminate()
    try:
        xw.wait(timeout=5)
    except subprocess.TimeoutExpired:
        xw.kill()

def watchdog():
    time.sleep(150)
    log('WATCHDOG: giving up')
    cleanup()
    os._exit(4)
threading.Thread(target=watchdog, daemon=True).start()

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

def windows():
    found = []
    def walk(win):
        try:
            children = win.query_tree().children
        except xerror.XError:
            return
        for c in children:
            t = title(c)
            if t and t.startswith('gxprobe-'):
                try:
                    if c.get_attributes().map_state == X.IsViewable:
                        found.append((t, hex(c.id)))
                except xerror.XError:
                    pass
            walk(c)
    walk(root)
    return sorted(found)

def names(ws):
    return {t for t, _ in ws}

def wait_for(pred, timeout, what):
    end = time.time() + timeout
    while time.time() < end:
        ws = windows()
        if pred(ws):
            return ws
        if gx.poll() is not None:
            log('graphix exited early, rc', gx.returncode)
            return None
        time.sleep(0.25)
    log('TIMEOUT waiting for', what, 'windows now', windows())
    return None

ws = wait_for(lambda ws: {'gxprobe-main', 'gxprobe-a'} <= names(ws), 110, 'main and a')
if ws is None:
    cleanup(); os._exit(2)
log('1. program started, windows:', ws)
time.sleep(1.0)
a_id = int(dict(ws)['gxprobe-a'], 16)
ev = protocol.event.ClientMessage(window=a_id, client_type=WM_PROTOCOLS,
                                  data=(32, [WM_DELETE, X.CurrentTime, 0, 0, 0]))
d.create_resource_object('window', a_id).send_event(ev, event_mask=X.NoEventMask)
d.flush()
log('2. sent WM_DELETE_WINDOW to gxprobe-a', hex(a_id), '(the close button)')
ws = wait_for(lambda ws: 'gxprobe-a' not in names(ws), 15, 'a to close')
if ws is None:
    cleanup(); os._exit(2)
log('3. window a closed, windows:', ws)
time.sleep(2.0)
log('4. 2s later, windows:', windows(), 'graphix running:', gx.poll() is None)
open(FLAG, 'w').close()
log('5. created the toggle flag: the program adds window b')
wait_for(lambda ws: 'gxprobe-b' in names(ws), 30, 'b to open')
time.sleep(2.0)
ws = windows()
log('6. after the toggle, windows:', ws)
back = dict(ws).get('gxprobe-a')
if back:
    log('RESULT: BUG - the closed window gxprobe-a reopened (old id %s, new id %s)' % (hex(a_id), back))
else:
    log('RESULT: window gxprobe-a stayed closed')
log('graphix stdout lines:', stdout_lines)
cleanup()
EOF
chmod +x "$WORK/session.py"

env -i PATH=/usr/local/bin:/usr/bin:/bin LANG=C.UTF-8 \
    HOME="$WORK/home" XDG_CONFIG_HOME="$WORK/home/.config" \
    XDG_DATA_HOME="$WORK/home/.local/share" XDG_STATE_HOME="$WORK/home/.local/state" \
    XDG_CACHE_HOME="$WORK/home/.cache" XDG_RUNTIME_DIR="$RTDIR" \
    GXW="$WORK" GXBIN="$GRAPHIX" GXPROG="$WORK/reopen.gx" ORIG_HOME="$HOME" \
    ORIG_RT="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}" ORIG_DBUS="${DBUS_SESSION_BUS_ADDRESS:-}" \
    dbus-run-session -- kwin_wayland --virtual --socket gxrepro --no-lockscreen \
    --no-global-shortcuts --no-kactivities --width 1600 --height 1200 \
    --exit-with-session "$WORK/session.py" > "$WORK/kwin.out" 2>&1 || true
cat "$WORK/session.log"
