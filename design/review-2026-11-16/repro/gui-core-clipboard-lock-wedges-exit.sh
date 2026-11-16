#!/usr/bin/env bash
# gui-core: the clipboard's one mutex is held across a blocking read, so a
# clipboard owner that never answers wedges every later clipboard op of the
# process (the GUI thread's copy/paste included) and its exit.
#
# On a private headless kwin_wayland --virtual (nothing opens on the
# desktop) process A (graphix) copies a text, then is SIGSTOPped: a frozen
# app that owns the clipboard. Process B then runs PROG:
#   MODE=wedge: read_text at 3 s, write_text at 3.5 s
#   MODE=exit:  read_text at 3 s, sys::exit at 6 s
# B's main thread and threads are dumped 12 s in (/proc wchan).
# Needs kwin_wayland (KDE 6) and dbus-run-session.
#   MODE=wedge timeout -s KILL 170 bash gui-core-clipboard-lock-wedges-exit.sh
#   MODE=exit  timeout -s KILL 170 bash gui-core-clipboard-lock-wedges-exit.sh
#
# expected: wedge: the read may wait on the frozen owner, but the write
#   answers at once (arboard's write needs nothing of the owner);
#   exit: the process ends at 6 s.
# observed (HEAD e1be2fbf2, debug build):
#   wedge: "write asked" printed at 3.5 s; neither the read nor the write
#     answered within 30 s.
#   exit: "exiting" printed at 6 s; the process was still running at 33 s:
#     task main graphix S futex_wait        <- clipboard::release() at shutdown
#     task tokio-rt-worker S anon_pipe_read <- the read, holding CLIPBOARD
#   control (no frozen owner): exits rc 0 at 6.1 s.
# Under X11 (rootful Xwayland) arboard gives up after 6 formats x 4 s: a
#   write_text asked 0.5 s after the read answered 24.0 s later.
set -euo pipefail
GRAPHIX=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/real/graphix}
MODE=${MODE:-exit}
WORK=$(mktemp -d /tmp/gxclip.XXXXXX); RTDIR=$(mktemp -d /tmp/gxrt.XXXXXX); chmod 700 "$RTDIR"
mkdir -p "$WORK/home"
cat > "$WORK/owner.gx" <<'EOF'
use gui::clipboard;
let go = sys::time::timer(duration:200.ms, false)$;
let w = clipboard::write_text(go ~ "owned by a process that will freeze");
println(w ~ "owner wrote: [w]");
null
EOF
cat > "$WORK/wedge.gx" <<'EOF'
use gui::clipboard;
let go = sys::time::timer(duration:3.s, false)$;
let t0 = sys::time::now(go);
let r = clipboard::read_text(go);
let go2 = sys::time::timer(duration:3500.ms, false)$;
let w = clipboard::write_text(go2 ~ "hello");
println(go2 ~ "write asked");
println(r ~ "read answered [sys::time::diff(sys::time::now(r), t0)] after it was asked: [r]");
println(w ~ "write answered [sys::time::diff(sys::time::now(w), t0)] after the read was asked: [w]");
null
EOF
cat > "$WORK/exit.gx" <<'EOF'
use gui::clipboard;
let go = sys::time::timer(duration:3.s, false)$;
let r = clipboard::read_text(go);
println(r ~ "read answered: [r]");
let done = sys::time::timer(duration:6.s, false)$;
println(done ~ "exiting");
sys::exit(done ~ 0);
null
EOF
cat > "$WORK/session.py" <<'EOF'
#!/usr/bin/env python3
import os, signal, subprocess, time
W = os.environ['GXW']; t0 = time.time()
def say(*a): print('[%6.2fs]' % (time.time() - t0), *a, flush=True)
env = dict(os.environ); env['HOME'] = os.environ['ORIG_HOME']; env.pop('DISPLAY', None)
assert env['WAYLAND_DISPLAY'].startswith('gxclip')
own = subprocess.Popen([os.environ['GXBIN'], '--no-cache', W + '/owner.gx'], env=env,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, start_new_session=True)
gx = subprocess.Popen([os.environ['GXBIN'], '--no-cache', W + '/' + os.environ['MODE'] + '.gx'],
                      env=env, stdout=open(W + '/gx.out', 'w'), stderr=subprocess.STDOUT,
                      stdin=subprocess.DEVNULL, start_new_session=True)
time.sleep(1.5); os.kill(own.pid, signal.SIGSTOP); say('clipboard owner', own.pid, 'frozen')
time.sleep(12)
for tid in sorted(os.listdir('/proc/%d/task' % gx.pid), key=int):
    try:
        comm = open('/proc/%d/task/%s/comm' % (gx.pid, tid)).read().strip()
        wchan = open('/proc/%d/task/%s/wchan' % (gx.pid, tid)).read().strip()
    except OSError:
        continue
    say('task', 'main' if int(tid) == gx.pid else tid, comm, wchan)
end = time.time() + 20
while time.time() < end and gx.poll() is None: time.sleep(0.1)
say('graphix', 'STILL RUNNING' if gx.poll() is None else 'exited rc %s' % gx.returncode)
for p in (gx, own):
    try: os.killpg(p.pid, signal.SIGKILL)
    except ProcessLookupError: pass
say('output:', open(W + '/gx.out').read())
EOF
chmod +x "$WORK/session.py"
env -i PATH=/usr/local/bin:/usr/bin:/bin LANG=C.UTF-8 HOME="$WORK/home" \
  XDG_CONFIG_HOME="$WORK/home/.config" XDG_DATA_HOME="$WORK/home/.local/share" \
  XDG_STATE_HOME="$WORK/home/.local/state" XDG_CACHE_HOME="$WORK/home/.cache" \
  XDG_RUNTIME_DIR="$RTDIR" GXW="$WORK" GXBIN="$GRAPHIX" MODE="$MODE" ORIG_HOME="$HOME" \
  dbus-run-session -- kwin_wayland --virtual --socket gxclip$$ --no-lockscreen \
  --no-global-shortcuts --no-kactivities --width 1024 --height 768 \
  --exit-with-session "$WORK/session.py" 2> "$WORK/kwin.err" || true
rm -rf "$RTDIR"
