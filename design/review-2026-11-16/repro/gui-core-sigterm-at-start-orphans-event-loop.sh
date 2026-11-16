#!/usr/bin/env bash
# gui-core: a SIGTERM (or a script's Ctrl-C) that lands while the GUI is
# starting leaves the process running forever with no window: the shell
# drops the half-built display without clear(), and the winit loop on the
# main thread has nothing that will ever stop it.
#
# On a private headless display (kwin_wayland --virtual hosting a rootful
# Xwayland; nothing opens on the desktop) each round starts graphix on a
# one-window program and sends it SIGTERM after a delay swept in 1 ms steps
# across the ~70 ms it takes the window to appear; a round whose process is
# alive 4 s later is a hang, and its threads are dumped 12 s after SIGTERM.
# Needs kwin_wayland (KDE 6), Xwayland, dbus-run-session and python-xlib.
#   timeout -s KILL 178 bash gui-core-sigterm-at-start-orphans-event-loop.sh
#
# expected: every round ends with the shell's status for SIGTERM (143).
# observed (HEAD e1be2fbf2, debug build): 8 of 100 rounds (delays 63-75 ms)
#   were STILL RUNNING 4 s after SIGTERM with no window; 12 s after it the
#   process is the main thread in do_epoll_wait (the winit loop) and the
#   idle graphix-eval pool: the tokio thread has ended, so nothing is left
#   that could stop the loop, and main never reaches its join.
set -euo pipefail
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/real/graphix}
WORK=$(mktemp -d /tmp/gxterm.XXXXXX); RTDIR=$(mktemp -d /tmp/gxrt.XXXXXX); chmod 700 "$RTDIR"
mkdir -p "$WORK/home"
cat > "$WORK/prog.gx" <<'EOF'
use gui::{window, text::text};
[&window(#title: &"gxprobe-main", #size: &mut {width: 300.0, height: 120.0}, &text(&"hello"))]
EOF
cat > "$WORK/session.py" <<'EOF'
#!/usr/bin/env python3
import os, signal, subprocess, time
from Xlib import X, display, error as xerror
W = os.environ['GXW']; t0 = time.time()
def say(*a): print('[%6.2fs]' % (time.time() - t0), *a, flush=True)
env = dict(os.environ); env['HOME'] = os.environ['ORIG_HOME']
r, w = os.pipe()
xw = subprocess.Popen(['/usr/bin/Xwayland', '-displayfd', str(w), '-nolisten', 'tcp', '-noreset'],
                      pass_fds=(w,), stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
os.close(w); env['DISPLAY'] = ':' + os.read(r, 64).decode().strip(); env.pop('WAYLAND_DISPLAY')
assert env['DISPLAY'] != ':0'
d = display.Display(env['DISPLAY']); root = d.screen().root
def windows():
    out = []
    def walk(win):
        try: cs = win.query_tree().children
        except xerror.XError: return
        for c in cs:
            try:
                n = c.get_wm_name()
                if n and 'gxprobe' in str(n) and c.get_attributes().map_state == X.IsViewable: out.append(n)
            except xerror.XError: pass
            walk(c)
    walk(root); return out
def launch():
    return subprocess.Popen([os.environ['GXBIN'], '--no-cache', W + '/prog.gx'], env=env,
                            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                            stdin=subprocess.DEVNULL, start_new_session=True)
def kill(p):
    try: os.killpg(p.pid, signal.SIGKILL)
    except ProcessLookupError: pass
    p.wait()
ts = []
for k in range(3):
    p = launch(); s = time.time()
    while not windows() and time.time() - s < 20: time.sleep(0.002)
    ts.append(time.time() - s); kill(p); time.sleep(0.3)
T = min(ts); say('launch to window: %s s' % ['%.3f' % t for t in ts])
hung = 0
for i in range(100):
    dl = max(0.0, T - 0.010 + (i // 2) * 0.001)
    p = launch(); time.sleep(dl); os.kill(p.pid, signal.SIGTERM); s = time.time()
    while p.poll() is None and time.time() - s < 4: time.sleep(0.02)
    if p.poll() is None:
        hung += 1
        say('delay %.3f: STILL RUNNING 4 s after SIGTERM, windows %s' % (dl, windows()))
        if hung == 1:
            time.sleep(8)
            for tid in sorted(os.listdir('/proc/%d/task' % p.pid), key=int)[:3]:
                comm = open('/proc/%d/task/%s/comm' % (p.pid, tid)).read().strip()
                wchan = open('/proc/%d/task/%s/wchan' % (p.pid, tid)).read().strip()
                say('   12 s after SIGTERM: task', 'main' if int(tid) == p.pid else tid, comm, wchan)
    kill(p); time.sleep(0.2)
say('RESULT: %d of 100 rounds hung after SIGTERM' % hung)
xw.kill()
EOF
chmod +x "$WORK/session.py"
env -i PATH=/usr/local/bin:/usr/bin:/bin LANG=C.UTF-8 HOME="$WORK/home" \
  XDG_CONFIG_HOME="$WORK/home/.config" XDG_DATA_HOME="$WORK/home/.local/share" \
  XDG_STATE_HOME="$WORK/home/.local/state" XDG_CACHE_HOME="$WORK/home/.cache" \
  XDG_RUNTIME_DIR="$RTDIR" GXW="$WORK" GXBIN="$GRAPHIX" ORIG_HOME="$HOME" \
  dbus-run-session -- kwin_wayland --virtual --socket gxterm$$ --no-lockscreen \
  --no-global-shortcuts --no-kactivities --width 1024 --height 768 \
  --exit-with-session "$WORK/session.py" 2> "$WORK/kwin.err" || true
rm -rf "$RTDIR"
