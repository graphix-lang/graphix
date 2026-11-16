#!/usr/bin/env bash
# gui-core: WindowSizeV takes any positive edge, but an edge under half a
# logical pixel rounds to a 0-pixel window: under Wayland the compositor
# kills the connection (xdg_surface invalid_size) and every window of the
# program is gone; under X11 CreateWindow fails.
#
# On a private headless kwin_wayland --virtual (nothing opens on the
# desktop): CASE=later opens a 300 x 120 window and sets its size to
# 0.3 x 0.3 after 1 s; CASE=start opens one at 0.3 x 0.3; X11=1 runs the
# program on a private rootful Xwayland instead of as a Wayland client.
# Needs kwin_wayland (KDE 6), Xwayland and dbus-run-session.
#   CASE=later timeout -s KILL 120 bash gui-core-subpixel-window-size.sh
#   CASE=start X11=1 timeout -s KILL 120 bash gui-core-subpixel-window-size.sh
#
# expected: the size is refused as a window size (as 0 and 8193 are), the
#   window keeps the size it has or fails alone with that reason.
# observed (HEAD e1be2fbf2, debug build):
#   later (Wayland): rc 1 at 1.2 s: "xdg_surface#23: error 5: invalid window
#     geometry size (0x0)", "Error: the display failed / running the event
#     loop / Exit Failure: 1"
#   start (Wayland): rc 1: "reconcile windows: gpu init / no suitable GPU
#     adapter found" after the same protocol error (the dead connection is
#     reported as a missing GPU)
#   start (X11): rc 1: "reconcile windows: failed to create window ... X11
#     error ... CreateWindow" (BadValue)
set -euo pipefail
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/real/graphix}
CASE=${CASE:-later}
WORK=$(mktemp -d /tmp/gxsize.XXXXXX); RTDIR=$(mktemp -d /tmp/gxrt.XXXXXX); chmod 700 "$RTDIR"
mkdir -p "$WORK/home"
cat > "$WORK/later.gx" <<'EOF'
use gui::{window, text::text};
let sz = {width: 300.0, height: 120.0};
sz <- sys::time::timer(duration:1.s, false) ~ {width: 0.3, height: 0.3};
println(sz ~ "size is [sz]");
[&window(#title: &"gxprobe-main", #size: &mut sz, &text(&"hello"))]
EOF
cat > "$WORK/start.gx" <<'EOF'
use gui::{window, text::text};
[&window(#title: &"gxprobe-main", #size: &mut {width: 0.3, height: 0.3}, &text(&"hello"))]
EOF
cat > "$WORK/session.py" <<'EOF'
#!/usr/bin/env python3
import os, signal, subprocess, time
W = os.environ['GXW']; t0 = time.time()
env = dict(os.environ); env['HOME'] = os.environ['ORIG_HOME']; env.pop('DISPLAY', None)
xw = None
if os.environ.get('X11') == '1':
    r, w = os.pipe()
    xw = subprocess.Popen(['/usr/bin/Xwayland', '-displayfd', str(w), '-nolisten', 'tcp',
                           '-noreset'], pass_fds=(w,), stdout=subprocess.DEVNULL,
                          stderr=subprocess.DEVNULL)
    os.close(w); env['DISPLAY'] = ':' + os.read(r, 64).decode().strip()
    assert env['DISPLAY'] != ':0'; env.pop('WAYLAND_DISPLAY')
gx = subprocess.Popen([os.environ['GXBIN'], '--no-cache', W + '/' + os.environ['CASE'] + '.gx'],
                      env=env, stdout=open(W + '/gx.out', 'w'), stderr=open(W + '/gx.err', 'w'),
                      stdin=subprocess.DEVNULL, start_new_session=True)
end = time.time() + 6
while time.time() < end and gx.poll() is None: time.sleep(0.1)
print('[%5.2fs] graphix %s' % (time.time() - t0, 'still running (window up)' if gx.poll() is None
                                else 'exited rc %s' % gx.returncode))
if gx.poll() is None: os.killpg(gx.pid, signal.SIGKILL)
if xw: xw.kill()
print('stdout:', open(W + '/gx.out').read())
print('stderr:', '\n'.join(l for l in open(W + '/gx.err').read().splitlines()[:8]))
EOF
chmod +x "$WORK/session.py"
env -i PATH=/usr/local/bin:/usr/bin:/bin LANG=C.UTF-8 HOME="$WORK/home" \
  XDG_CONFIG_HOME="$WORK/home/.config" XDG_DATA_HOME="$WORK/home/.local/share" \
  XDG_STATE_HOME="$WORK/home/.local/state" XDG_CACHE_HOME="$WORK/home/.cache" \
  XDG_RUNTIME_DIR="$RTDIR" GXW="$WORK" GXBIN="$GRAPHIX" CASE="$CASE" X11="${X11:-0}" \
  ORIG_HOME="$HOME" RUST_BACKTRACE=0 \
  dbus-run-session -- kwin_wayland --virtual --socket gxsize$$ --no-lockscreen \
  --no-global-shortcuts --no-kactivities --width 1024 --height 768 \
  --exit-with-session "$WORK/session.py" 2> "$WORK/kwin.err" || true
rm -rf "$RTDIR"
