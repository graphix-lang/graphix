#!/usr/bin/env python3
# small-pkgs-09: nested container headers make pack::read reserve memory
# quadratic in its input, and a few MB abort the process.
#
# Value::decode (../netidx/netidx-value/src/lib.rs:563-593) reserves the
# element count an array (tag 19) or map (tag 21) header claims before any
# element arrives (values.reserve(elts) :573, pairs.reserve(elts) :589). The
# only check bounds ONE container by min(MAX_VEC = 2 GiB, 256 x the input
# bytes remaining). Nested headers each pass it against the same remaining
# input and every open frame keeps its reservation, so 5 bytes of header
# reserve up to 2 GiB and k nested headers reserve the sum. pack::read
# (stdlib/graphix-package-pack/src/lib.rs:79) hands its bytes straight to it.
#
# Inputs, each read by pack::read(sys::fs::read_all_bin(path)$):
#   nested K : K headers [19, varint(2^27)] (2 GiB each), then 8 MiB of 0xff
#              (an unknown tag: a decode that gets past the headers fails at
#              once with PackErr UnknownTag)
#   quad     : L/5 headers [19, varint(remaining * 16)] (the reviewer's
#              shape: no 2 GiB cap needed), L = 500 KB and 3 MB
#
# command: timeout -s KILL 170 python3 design/review-2026-10-05/repro/small-pkgs-09.py <graphix>
#
# expected: every run prints error:["PackErr", ...] and exits 0 in modest
#   memory (a malformed input is a catchable PackErr; the decoder's own
#   comment says hostile nesting costs memory "bounded by the existing
#   per-container size checks").
# observed (HEAD c722befe, debug build, x86-64 Linux 7.2, 128 TiB user
# address space, vm.overcommit_memory=0, THP always, the probe's 6 GB
# cgroup cap; VmPeak/VmHWM polled from /proc):
#   nested K=10     (8.4 MB) rc=0   0.2 s VmPeak   0 TiB VmHWM   66 MiB  error:["PackErr", "UnknownTag"]
#   nested K=60000  (8.7 MB) rc=0   0.6 s VmPeak 117 TiB VmHWM  537 MiB  error:["PackErr", "UnknownTag"]
#   nested K=70000  (8.7 MB) rc=134 0.5 s VmPeak 128 TiB VmHWM 1468 MiB  memory allocation of 2147483648 bytes failed
#   quad 500 KB     (0.5 MB) rc=0   1.5 s VmPeak   6 TiB VmHWM 3288 MiB  error:["PackErr", "BufferShort"]
#   quad 3 MB       (3.0 MB) rc=134 1.1 s VmPeak 128 TiB VmHWM 3123 MiB  memory allocation of 479107840 bytes failed
#   rc 134 is SIGABRT: the allocator's failure aborts the whole process.
#   Across runs the 500 KB case peaks at 2.5-5.8 GB resident: malloc's
#   header write first-touches each reservation, mostly as transparent huge
#   pages (AnonHugePages to 3.7 GB), plus ~0.4 GB of page tables.
#   Core dumps are off here; with them on, systemd-coredump spends 35-70 s
#   on the 128 TiB image before the process is gone.
import os, resource, subprocess, sys, tempfile, time

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


def nocore():
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))


def vm(pid_match):
    peak = hwm = 0
    for pid in os.listdir("/proc"):
        if not pid.isdigit():
            continue
        try:
            with open(f"/proc/{pid}/cmdline", "rb") as f:
                argv = f.read().split(b"\0")
            if pid_match not in argv or os.path.basename(argv[0]) in (b"timeout", b"bash", b"systemd-run"):
                continue
            with open(f"/proc/{pid}/status") as f:
                for line in f:
                    if line.startswith("VmPeak:"):
                        peak = int(line.split()[1])
                    elif line.startswith("VmHWM:"):
                        hwm = int(line.split()[1])
        except OSError:
            pass
    return peak, hwm


with tempfile.TemporaryDirectory() as d:
    env = dict(os.environ, XDG_CACHE_HOME=os.path.join(d, "cache"))
    for name, data in [("nested K=10", nested(10)), ("nested K=60000", nested(60000)),
                       ("nested K=70000", nested(70000)), ("quad 500 KB", quad(500_000)),
                       ("quad 3 MB", quad(3_000_000))]:
        tag = name.replace(" ", "_").replace("=", "")
        path = os.path.join(d, tag + ".bin")
        with open(path, "wb") as f:
            f.write(data)
        gx = os.path.join(d, tag + ".gx")
        with open(gx, "w") as f:
            f.write(PROG.format(path=path))
        t0 = time.time()
        p = subprocess.Popen(["timeout", "-s", "KILL", "60", G, "--no-cache", gx], env=env,
                             stdout=subprocess.PIPE, stderr=subprocess.STDOUT, preexec_fn=nocore)
        peak = hwm = 0
        while p.poll() is None:
            pk, hw = vm(gx.encode())
            peak, hwm = max(peak, pk), max(hwm, hw)
            time.sleep(0.002)
        out = [l for l in p.stdout.read().decode(errors="replace").splitlines() if l.strip()]
        last = next((l for l in out if "PackErr" in l or "memory allocation" in l), out[-1] if out else "")
        print(f"{name:15} ({len(data) / 1e6:.1f} MB) rc={p.returncode:<3} {time.time() - t0:.1f} s "
              f"VmPeak {peak / 2**30:.0f} TiB VmHWM {hwm / 1024:.0f} MiB  {last.strip()[:80]}")
