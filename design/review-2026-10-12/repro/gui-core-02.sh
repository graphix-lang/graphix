#!/usr/bin/env bash
# gui-core-02: a focused text_input makes the event loop render flat out.
#
# Same session as gui-core-01.sh, but graphix runs under gdb with a counting
# breakpoint on graphix_package_gui::frame::frame (gui-core-02.gdb), so the
# frames are counted directly. A focused text_input asks iced for a redraw
# At(now + ~500ms) (cursor blink); render() turns any request into
# needs_redraw = true (event_loop.rs:319), and about_to_wait wakes at once for
# any window with needs_redraw (event_loop.rs:471), so the At deadline is
# never waited for: a frame every vsync instead of two a second.
#
#   PROBERUN=.../probe-run GRAPHIX=/path/to/real/graphix \
#     timeout -s KILL 178 bash gui-core-02.sh
# (GRAPHIX must be the binary itself: gdb runs it; PROBERUN caps gdb's scope.)
#
# expected: about 8 frames in the focused 4 s (the 500 ms blink), 0 unfocused.
# observed (HEAD 7324207f, debug build):
#   2f. frames in that window: 0          (unfocused, 4 s)
#   4f. frames in that window: 240        (focused, 4 s: 60 per second)
#   4. focused: 0.41 CPU-seconds, 1274 main-thread context switches in 4s
set -euo pipefail

GRAPHIX=$(command -v "${GRAPHIX:-$HOME/tmp/target/debug/graphix}")
WORK=${WORK:-$(mktemp -d /tmp/gxrepro-gc02.XXXXXX)}
RTDIR=${RTDIR:-$(mktemp -d /tmp/gxrt.XXXXXX)}
mkdir -p "$WORK/home" "$RTDIR"
chmod 700 "$RTDIR"
rm -f "$WORK/session.log"

cat > "$WORK/prog.gx" <<'EOF'
use gui::{window, column::column, text_input::text_input};

let name = "";
let col = column(
    #padding: &`All(30.0),
    #width: &`Fill,
    &[text_input(#placeholder: &"type", #width: &`Fill, #on_input: |v| name <- v, &name)]
);
[&window(#title: &"gxprobe-main", #size: &mut {width: 400.0, height: 200.0}, &col)]
EOF

cat > "$WORK/session.py" <<'EOF'
#!/usr/bin/env python3
import os, signal, subprocess, threading, time
from Xlib import X, display, protocol, error as xerror
from Xlib.ext import xtest

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
log('private rootful Xwayland on', DISP)

genv = dict(os.environ)
for k in ('WAYLAND_DISPLAY', 'WAYLAND_SOCKET', 'XDG_CONFIG_HOME', 'XDG_DATA_HOME',
          'XDG_STATE_HOME', 'QT_QPA_PLATFORM', 'DBUS_SESSION_BUS_ADDRESS'):
    genv.pop(k, None)
genv.update(DISPLAY=DISP, HOME=os.environ['ORIG_HOME'], XDG_RUNTIME_DIR=os.environ['ORIG_RT'],
            XDG_CACHE_HOME=W + '/gxcache')
assert genv['DISPLAY'] != ':0' and 'WAYLAND_DISPLAY' not in genv
def launch():
    return subprocess.Popen([os.environ['PROBERUN'], '6G', '/usr/bin/gdb', '-q', '-batch', '-x', os.environ['GDBSCRIPT'], '--args', os.environ['GXBIN'], '--no-cache', os.environ['GXPROG']], env=dict(genv, GXFRAMES=W + '/frames.txt'),
                            stdout=subprocess.PIPE, stderr=open(W + '/graphix.err', 'a'),
                            text=True, start_new_session=True)
gx = launch()
log('graphix started, pid', gx.pid)

def cleanup():
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
    time.sleep(165)
    log('WATCHDOG: giving up')
    cleanup()
    os._exit(4)
threading.Thread(target=watchdog, daemon=True).start()

def cpu_ticks():
    # utime + stime of every thread of the process (the wrapper's child too)
    total = 0
    for pid in [gx.pid] + children(gx.pid):
        try:
            f = open('/proc/%d/stat' % pid).read().rsplit(')', 1)[1].split()
            total += int(f[11]) + int(f[12])
        except OSError:
            pass
    return total

def children(pid):
    out = []
    try:
        for c in open('/proc/%d/task/%d/children' % (pid, pid)).read().split():
            out.append(int(c)); out.extend(children(int(c)))
    except OSError:
        pass
    return out

def main_switches():
    # context switches of the real graphix's main thread (the deepest descendant)
    pids = [gx.pid] + children(gx.pid)
    st = open('/proc/%d/status' % pids[-1]).read()
    return sum(int(l.split()[1]) for l in st.splitlines() if 'ctxt_switches' in l)

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

end = time.time() + 100
ws = []
while time.time() < end and not ws:
    ws = find()
    if gx.poll() is not None:
        log('graphix exited early', gx.returncode); cleanup(); os._exit(2)
    time.sleep(0.25)
if not ws:
    log('no window'); cleanup(); os._exit(2)
win = ws[0]
log('1. window up', hex(win.id))
time.sleep(3.0)
hz = os.sysconf('SC_CLK_TCK')
c0 = cpu_ticks(); s0 = main_switches(); time.sleep(4.0); c1 = cpu_ticks(); s1 = main_switches()
def frames(a, b):
    try:
        return sum(1 for l in open(W + '/frames.txt') if a <= float(l) < b)
    except OSError:
        return -1
log('2f. frames in that window:', frames(time.time() - 4.0, time.time()))
log('2. unfocused: %.2f CPU-seconds, %d main-thread context switches in 4s' % ((c1 - c0) / hz, s1 - s0))
g = win.translate_coords(root, 0, 0)
x, y = -g.x + 200, -g.y + 45
xtest.fake_input(d, X.MotionNotify, x=x, y=y)
xtest.fake_input(d, X.ButtonPress, 1)
xtest.fake_input(d, X.ButtonRelease, 1)
d.sync()
log('3. clicked at', x, y)
time.sleep(2.0)
c0 = cpu_ticks(); s0 = main_switches(); time.sleep(4.0); c1 = cpu_ticks(); s1 = main_switches()
log('4f. frames in that window:', frames(time.time() - 4.0, time.time()))
log('4. focused: %.2f CPU-seconds, %d main-thread context switches in 4s' % ((c1 - c0) / hz, s1 - s0))
ev = protocol.event.ClientMessage(window=win.id, client_type=WM_PROTOCOLS,
                                  data=(32, [WM_DELETE, X.CurrentTime, 0, 0, 0]))
win.send_event(ev, event_mask=X.NoEventMask)
d.flush()
log('5. sent WM_DELETE_WINDOW to the only window')
end = time.time() + 10
while time.time() < end and gx.poll() is None:
    time.sleep(0.25)
log('6. windows now:', [hex(w.id) for w in find()])
if gx.poll() is None:
    log('RESULT: BUG - graphix still running 10s after its last window closed')
else:
    log('RESULT: graphix exited, rc', gx.returncode)
try:
    os.killpg(gx.pid, signal.SIGKILL); gx.wait(timeout=5)
except ProcessLookupError:
    pass
# close-only rounds: the shell's Stop races the loop's exit
for rnd in range(int(os.environ.get('GC01_ROUNDS', '0'))):
    gx = launch()
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
    log('round', rnd, 'exited' if gx.poll() is not None
        else 'STILL RUNNING 8s after its last window closed (hang)',
        'windows now:', [hex(w.id) for w in find()])
    try:
        os.killpg(gx.pid, signal.SIGKILL); gx.wait(timeout=5)
    except ProcessLookupError:
        pass
cleanup()
EOF
chmod +x "$WORK/session.py"

env -i PATH=/usr/local/bin:/usr/bin:/bin LANG=C.UTF-8 \
    HOME="$WORK/home" XDG_CONFIG_HOME="$WORK/home/.config" \
    XDG_DATA_HOME="$WORK/home/.local/share" XDG_STATE_HOME="$WORK/home/.local/state" \
    XDG_CACHE_HOME="$WORK/home/.cache" XDG_RUNTIME_DIR="$RTDIR" \
    PROBERUN="${PROBERUN:?set PROBERUN to a memory-capping runner, or to env}" GDBSCRIPT="${GDBSCRIPT:-$(dirname "$(readlink -f "$0")")/gui-core-02.gdb}" GXW="$WORK" GC01_ROUNDS="${GC01_ROUNDS:-0}" GXBIN="$GRAPHIX" GXPROG="$WORK/prog.gx" ORIG_HOME="$HOME" \
    ORIG_RT="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}" \
    dbus-run-session -- kwin_wayland --virtual --socket gxrepro02 --no-lockscreen \
    --no-global-shortcuts --no-kactivities --width 1600 --height 1200 \
    --exit-with-session "$WORK/session.py" > "$WORK/kwin.out" 2>&1 || true
cat "$WORK/session.log"
echo "--- graphix stderr:"; cat "$WORK/graphix.err" 2>/dev/null | tail -20
