#!/usr/bin/env python3
# gui-core-06: messages handled in Rust change widget state after the frame
# is drawn, and about_to_wait schedules no wake for them.
#
# GuiHandler::about_to_wait (stdlib/graphix-package-gui/src/event_loop.rs)
# draws every dirty window, then drains the messages ui.update produced.
# on_message mutates the widget (TextEditorW::on_message performs the
# editor Action) and sets tw.needs_redraw, but the wake (event_loop.rs:418)
# is computed only from deferred_until and next_redraw, so the loop goes
# back to ControlFlow::Wait with the window dirty. The change shows up at
# the next unrelated OS event.
#
# The script starts a private headless compositor (kwin_wayland --virtual
# on an abstract socket, its own D-Bus session and XDG dirs: nothing appears
# on the desktop), runs an editable 200-line text_editor in it, drives the
# pointer with org_kde_kwin_fake_input and captures the window through
# KWin's ScreenShot2 D-Bus API:
#   s1  pointer resting over the editor
#   s2  2.5 s after one wheel event (axis 50 px = 12 lines), no other input
#   s3  after a 1 px pointer motion
# Needs kwin_wayland (KDE 6), dbus-run-session and python3-dbus.
#
# command (from the repo root; GRAPHIX defaults to ~/tmp/target/debug/graphix):
#   timeout -s KILL 170 env -u DISPLAY -u WAYLAND_DISPLAY dbus-run-session -- \
#     python3 design/review-2026-10-05/repro/gui-core-06.py [path/to/graphix]
#
# expected: s2 shows the editor scrolled (first line "line 012"); s2 != s1.
# observed (HEAD c722befe, debug build):
#   first visible line: s1 line 000, s2 line 000, s3 line 012
#   s2 == s1: True   s3 == s2: False
#   (with WAYLAND_DEBUG=client added to graphix's environment: one
#   wl_surface.commit 1 ms after the wl_pointer.axis, drawn before the
#   Scroll action was performed, then no commit until the wl_pointer.motion
#   2.5 s later.)

import hashlib
import os
import secrets
import select
import socket
import struct
import subprocess
import sys
import tempfile
import time

import dbus

GRAPHIX = (
    sys.argv[1]
    if len(sys.argv) > 1
    else os.path.expanduser("~/tmp/target/debug/graphix")
)
WORK = os.environ.get("GC06_WORK") or tempfile.mkdtemp(prefix="gc06-")

LINES = "\\n".join("line %03d" % i for i in range(200))
PROGRAM = """use gui::{window, text_editor::text_editor};

let content = "%s";

[&window(
  #title: &"Probe",
  &text_editor(#on_edit: |v| content <- v, #height: &400.0, #size: &20.0, &content)
)]
""" % LINES


def wl_string(s):
    b = s.encode() + b"\0"
    return struct.pack("<I", len(b)) + b + b"\0" * ((4 - len(b) % 4) % 4)


def fixed(v):
    return struct.pack("<i", int(round(v * 256)))


class FakeInput:
    """A pointer device in the private kwin, via org_kde_kwin_fake_input."""

    def __init__(self, addr):
        self.s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.s.connect(addr)
        self.nid, self.buf, self.globals = 2, b"", {}
        reg = self.new_id()
        self.send(1, 1, struct.pack("<I", reg))
        self.roundtrip(lambda o, op, p: o == reg and op == 0 and self.on_global(p))
        name, ver = self.globals["org_kde_kwin_fake_input"]
        self.fi = self.new_id()
        self.send(
            reg,
            0,
            struct.pack("<I", name)
            + wl_string("org_kde_kwin_fake_input")
            + struct.pack("<II", min(ver, 4), self.fi),
        )
        self.send(self.fi, 0, wl_string("gc06") + wl_string("repro"))
        self.roundtrip()

    def new_id(self):
        self.nid += 1
        return self.nid - 1

    def send(self, obj, op, payload=b""):
        self.s.sendall(struct.pack("<II", obj, ((8 + len(payload)) << 16) | op) + payload)

    def roundtrip(self, handler=None):
        cb = self.new_id()
        self.send(1, 0, struct.pack("<I", cb))
        deadline = time.time() + 5
        while time.time() < deadline:
            self.s.settimeout(0.5)
            try:
                data = self.s.recv(65536)
            except socket.timeout:
                continue
            self.buf += data
            while len(self.buf) >= 8:
                obj, so = struct.unpack("<II", self.buf[:8])
                size, op = so >> 16, so & 0xFFFF
                if len(self.buf) < size:
                    break
                payload, self.buf = self.buf[8:size], self.buf[size:]
                if obj == 1 and op == 0:
                    raise RuntimeError("wayland error %r" % payload)
                if obj == cb and op == 0:
                    return
                if handler:
                    handler(obj, op, payload)
        raise RuntimeError("roundtrip timeout")

    def on_global(self, p):
        name, slen = struct.unpack("<II", p[:8])
        off = 8 + ((slen + 3) & ~3)
        self.globals[p[8 : 8 + slen - 1].decode()] = (
            name,
            struct.unpack("<I", p[off : off + 4])[0],
        )

    def motion_abs(self, x, y):
        self.send(self.fi, 9, fixed(x) + fixed(y))
        self.roundtrip()

    def motion_rel(self, dx, dy):
        self.send(self.fi, 1, fixed(dx) + fixed(dy))
        self.roundtrip()

    def axis(self, value):
        self.send(self.fi, 3, struct.pack("<I", 0) + fixed(value))
        self.roundtrip()


def capture_window():
    """The active window's pixels (BGRA rows) through KWin ScreenShot2."""
    obj = dbus.SessionBus().get_object("org.kde.KWin", "/org/kde/KWin/ScreenShot2")
    iface = dbus.Interface(obj, "org.kde.KWin.ScreenShot2")
    r, w = os.pipe()
    fd = dbus.types.UnixFd(w)
    res = iface.CaptureActiveWindow(dbus.Dictionary({}, signature="sv"), fd)
    os.close(w)
    del fd
    need = int(res["stride"]) * int(res["height"])
    data = bytearray()
    deadline = time.time() + 10
    while len(data) < need and time.time() < deadline:
        if select.select([r], [], [], 0.5)[0]:
            chunk = os.read(r, 1 << 20)
            if not chunk:
                break
            data += chunk
    os.close(r)
    return int(res["width"]), int(res["height"]), int(res["stride"]), bytes(data)


def save_png(shot, path):
    width, height, stride, data = shot
    ppm = path[:-4] + ".ppm"
    with open(ppm, "wb") as f:
        f.write(b"P6\n%d %d\n255\n" % (width, height))
        for y in range(height):
            row = data[y * stride : y * stride + width * 4]
            rgb = bytearray(width * 3)
            rgb[0::3], rgb[1::3], rgb[2::3] = row[2::4], row[1::4], row[0::4]
            f.write(rgb)
    subprocess.run(["magick", ppm, path], check=False)


def kill_tree(pid):
    children = {}
    for d in filter(str.isdigit, os.listdir("/proc")):
        try:
            with open("/proc/%s/stat" % d) as f:
                children.setdefault(int(f.read().rsplit(")", 1)[1].split()[1]), []).append(int(d))
        except (OSError, IndexError, ValueError):
            pass
    todo = [pid]
    while todo:
        p = todo.pop()
        todo.extend(children.get(p, []))
        try:
            os.kill(p, 9)
        except ProcessLookupError:
            pass


def main():
    assert "WAYLAND_DISPLAY" not in os.environ and "DISPLAY" not in os.environ
    prog = os.path.join(WORK, "gc06.gx")
    with open(prog, "w") as f:
        f.write(PROGRAM)
    for d in ("config", "data", "cache", "state", "rt"):
        os.makedirs(os.path.join(WORK, d), mode=0o700, exist_ok=True)
    addr = "\0gc06-" + secrets.token_hex(6)
    lsock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    lsock.bind(addr)
    lsock.listen(16)
    kenv = dict(os.environ)
    kenv.update(
        XDG_CONFIG_HOME=WORK + "/config",
        XDG_DATA_HOME=WORK + "/data",
        XDG_CACHE_HOME=WORK + "/cache",
        XDG_STATE_HOME=WORK + "/state",
        XDG_RUNTIME_DIR=WORK + "/rt",
        KWIN_WAYLAND_NO_PERMISSION_CHECKS="1",
        KWIN_SCREENSHOT_NO_PERMISSION_CHECKS="1",
    )
    kwin = subprocess.Popen(
        ["kwin_wayland", "--virtual", "--wayland-fd", str(lsock.fileno()),
         "--width", "1024", "--height", "768", "--no-lockscreen",
         "--no-global-shortcuts", "--no-kactivities"],
        env=kenv, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        pass_fds=(lsock.fileno(),),
    )
    gx = None
    try:
        time.sleep(3)
        pointer = FakeInput(addr)
        csock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        csock.connect(addr)
        genv = dict(os.environ)
        genv.update(WAYLAND_SOCKET=str(csock.fileno()), XDG_CACHE_HOME=WORK + "/cache")
        with open(WORK + "/graphix.log", "wb") as log:
            gx = subprocess.Popen(
                [GRAPHIX, "--no-cache", prog], env=genv, stdout=log, stderr=log,
                pass_fds=(csock.fileno(),),
            )
        csock.close()
        deadline = time.time() + 90
        while True:
            time.sleep(1)
            try:
                if capture_window()[0] == 800:
                    break
            except dbus.DBusException:
                pass
            if time.time() > deadline:
                raise RuntimeError("the window never appeared")
        time.sleep(3)
        pointer.motion_abs(512, 300)
        time.sleep(1.5)
        s1 = capture_window()
        pointer.axis(50.0)
        time.sleep(2.5)
        s2 = capture_window()
        pointer.motion_rel(1, 0)
        time.sleep(1.5)
        s3 = capture_window()
        digest = lambda s: hashlib.sha256(s[3]).hexdigest()[:16]
        for name, s in (("s1", s1), ("s2", s2), ("s3", s3)):
            save_png(s, os.path.join(WORK, name + ".png"))
            print(name, digest(s))
        print("s2 == s1:", digest(s2) == digest(s1), "(True = the scroll was not drawn)")
        print("s3 == s2:", digest(s3) == digest(s2))
        print("screenshots in", WORK)
    finally:
        if gx is not None:
            kill_tree(gx.pid)
            gx.wait()
        kwin.kill()
        kwin.wait()


main()
