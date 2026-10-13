#!/usr/bin/env python3
# x-rerun-5-02: small-pkgs-09 still reproduces: nested pack headers make
# pack::read abort the process.
#
# pack::read (stdlib/graphix-package-pack/src/lib.rs, Packed::parse) hands its
# bytes to netidx-value's Value::decode, which reserves each array (tag 19) or
# map (tag 21) header's claimed element count before any element arrives. The
# size check bounds one container against the remaining input, not the frames
# already open, so k nested headers reserve k times.
#
# command: timeout -s KILL 170 python3 -I design/review-2026-10-12/repro/x-rerun-5-02.py <graphix>
# expected: each run prints error:["PackErr", ...] and exits 0 in modest memory.
# observed (HEAD 7324207f, debug build):
#   nested K=70000 (8.7 MB) rc=134  memory allocation of 2147483648 bytes failed
#   quad 3 MB      (3.0 MB) rc=134  memory allocation of 479106560 bytes failed
#   (rc 134 = SIGABRT: the whole runtime dies; quad 500 KB returns BufferShort
#   after ~1-5 GB resident.)
import os, resource, subprocess, sys, tempfile

G = sys.argv[1] if len(sys.argv) > 1 else "graphix"


def varint4(v):
    return bytes([0x80 | (v & 0x7F), 0x80 | ((v >> 7) & 0x7F), 0x80 | ((v >> 14) & 0x7F), (v >> 21) & 0x7F])


def nested(k):
    return (bytes([19]) + varint4(1 << 27)) * k + b"\xff" * ((8 << 20) + 64)


def quad(l):
    return b"".join(bytes([19]) + varint4((l - 5 * (i + 1)) * 16) for i in range(l // 5))


PROG = """let r: Result<Array<Any>, [`PackErr(string), `InvalidCast(string)]> = pack::read(sys::fs::read_all_bin("{path}")$);
println(r);
sys::exit(sys::time::after_idle(duration:100.ms, r ~ 0))
"""

with tempfile.TemporaryDirectory() as d:
    env = dict(os.environ, XDG_CACHE_HOME=os.path.join(d, "cache"))
    for name, data in [("nested_K70000", nested(70000)), ("quad_3MB", quad(3_000_000))]:
        path = os.path.join(d, name + ".bin")
        with open(path, "wb") as f:
            f.write(data)
        gx = os.path.join(d, name + ".gx")
        with open(gx, "w") as f:
            f.write(PROG.format(path=path))
        p = subprocess.run(["timeout", "-s", "KILL", "60", G, "--no-cache", gx], env=env,
                           stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                           preexec_fn=lambda: resource.setrlimit(resource.RLIMIT_CORE, (0, 0)))
        out = [l for l in p.stdout.decode(errors="replace").splitlines() if l.strip()]
        last = next((l for l in out if "PackErr" in l or "memory allocation" in l), out[-1] if out else "")
        print(f"{name:14} ({len(data) / 1e6:.1f} MB) rc={p.returncode:<3} {last.strip()[:80]}")
