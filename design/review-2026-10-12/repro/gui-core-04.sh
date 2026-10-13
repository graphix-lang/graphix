#!/usr/bin/env bash
# gui-core-04: a window wider than 8192 physical pixels panics the GUI.
#
# GpuState::new (render.rs:173) asks for a device with
# DeviceDescriptor::default(), i.e. wgpu::Limits::default(), whose
# max_texture_dimension_2d is 8192, whatever the adapter supports.
# WindowSurface::new / resize then configure the surface at the window's
# physical size (render.rs:247, 260); past the limit wgpu raises a validation
# error, and the default uncaptured-error handler panics on the main thread.
# A program's #size (or a HiDPI scale: 4200 logical px at scale 2) reaches it.
#
# On a private headless display (kwin_wayland --virtual + rootful Xwayland;
# nothing opens on the desktop). Needs kwin_wayland (KDE 6), Xwayland,
# dbus-run-session and python-xlib.
#   GRAPHIX=/path/to/graphix timeout -s KILL 120 bash gui-core-04.sh
#
# expected: the window opens (iced's own compositor asks for the adapter's
# limits), or the program is told its size cannot be shown.
# observed (HEAD 7324207f, debug build): graphix exits at once,
#   thread 'main' panicked at wgpu-27.0.1/src/backend/wgpu_core.rs:3793:18:
#   wgpu error: Validation Error ... In Surface::configure: `Surface` width and
#   height must be within the maximum supported texture size. Requested was
#   (9000, 300), maximum extent for either dimension is 8192.
set -euo pipefail

GRAPHIX=$(command -v "${GRAPHIX:-$HOME/tmp/target/debug/graphix}")
WORK=${WORK:-$(mktemp -d /tmp/gxrepro-gc04.XXXXXX)}
RTDIR=${RTDIR:-$(mktemp -d /tmp/gxrt.XXXXXX)}
mkdir -p "$WORK/home" "$RTDIR"
chmod 700 "$RTDIR"
rm -f "$WORK/session.log"

cat > "$WORK/prog.gx" <<'EOF'
use gui::{window, text::text};

[&window(#title: &"gxprobe-wide", #size: &mut {width: 9000.0, height: 300.0}, &text(&"wide"))]
EOF

cat > "$WORK/session.py" <<'EOF'
#!/usr/bin/env python3
import os, signal, subprocess, threading, time
from Xlib import X, display, error as xerror

W = os.environ['GXW']
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
genv = dict(os.environ)
for k in ('WAYLAND_DISPLAY', 'WAYLAND_SOCKET', 'XDG_CONFIG_HOME', 'XDG_DATA_HOME',
          'XDG_STATE_HOME', 'QT_QPA_PLATFORM', 'DBUS_SESSION_BUS_ADDRESS'):
    genv.pop(k, None)
genv.update(DISPLAY=DISP, HOME=os.environ['ORIG_HOME'], XDG_RUNTIME_DIR=os.environ['ORIG_RT'],
            XDG_CACHE_HOME=W + '/gxcache')
assert genv['DISPLAY'] != ':0' and 'WAYLAND_DISPLAY' not in genv
gx = subprocess.Popen([os.environ['GXBIN'], '--no-cache', os.environ['GXPROG']], env=genv,
                      stdout=open(W + '/graphix.out', 'w'), stderr=open(W + '/graphix.err', 'w'),
                      start_new_session=True)
d = display.Display(DISP)
root = d.screen().root
NET_WM_NAME = d.intern_atom('_NET_WM_NAME')
UTF8 = d.intern_atom('UTF8_STRING')

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
                        found.append(t)
                except xerror.XError:
                    pass
            walk(c)
    walk(root)
    return sorted(found)

time.sleep(6.0)
log('windows after 6s:', find(), '  graphix running:', gx.poll() is None)
try:
    os.killpg(gx.pid, signal.SIGKILL); gx.wait(timeout=5)
except ProcessLookupError:
    pass
log('graphix stdout:', repr(open(W + '/graphix.out').read()),
    ' stderr:', repr(open(W + '/graphix.err').read()))
xw.terminate()
try:
    xw.wait(timeout=5)
except subprocess.TimeoutExpired:
    xw.kill()
EOF
chmod +x "$WORK/session.py"

env -i PATH=/usr/local/bin:/usr/bin:/bin LANG=C.UTF-8 \
    HOME="$WORK/home" XDG_CONFIG_HOME="$WORK/home/.config" \
    XDG_DATA_HOME="$WORK/home/.local/share" XDG_STATE_HOME="$WORK/home/.local/state" \
    XDG_CACHE_HOME="$WORK/home/.cache" XDG_RUNTIME_DIR="$RTDIR" \
    GXW="$WORK" GXBIN="$GRAPHIX" GXPROG="$WORK/prog.gx" ORIG_HOME="$HOME" \
    ORIG_RT="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}" \
    dbus-run-session -- kwin_wayland --virtual --socket gxrepro04 --no-lockscreen \
    --no-global-shortcuts --no-kactivities --width 1600 --height 1200 \
    --exit-with-session "$WORK/session.py" > "$WORK/kwin.out" 2>&1 || true
cat "$WORK/session.log"
