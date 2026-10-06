#!/usr/bin/env bash
# fuzz-main-aux-09: leakcheck scores a mode whose child died as slope 0
# and passes; a relative binary path fails after the sandbox chdir;
# minimize runs its program unsandboxed in the caller's cwd
# (graphix-fuzz/src/main.rs:504, :479, :315-321).
#
# command: FUZZ=/path/to/graphix-fuzz bash design/review-2026-10-05/repro/fuzz-main-aux-09.sh
#   (part 1 takes 16 witnesses x 2 modes x 6 s = ~3.2 min; FAST=1 builds
#   an LD_PRELOAD shim with gcc that divides leakcheck's sleeps by 10)
#
# The stand-in shell idles under --no-fusion and dies of SIGSEGV in the
# fused mode, i.e. a fused shell that crashes on every witness.
#
# expected:
#   1. leakcheck fails (exit 1): no witness was measured in the fused mode
#   2. `leakcheck ./graphix 1` runs the gate as the absolute path does
#   3. minimize, like check, leaves the caller's cwd empty
# observed (HEAD c722befe, debug build):
#   1. every witness prints "child died early — skipping" and then
#      "interp 0.0 kB/s, jit 0.0 kB/s" with no LEAK; the last line is
#      "leakcheck: 16 witnesses, 0 leaks" and the exit status is 0
#      (also with a stand-in that exits 1 in both modes, as a shell
#      that no longer compiles the witnesses would)
#   2. "Error: No such file or directory (os error 2)", exit 1
#   3. check: cwd empty; minimize: cwd holds wrote_into_cwd.txt
set -u
FUZZ=${FUZZ:-graphix-fuzz}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
cat > "$T/graphix" <<'EOF'
#!/bin/sh
if [ "$1" = --no-fusion ]; then exec sleep 100; else kill -SEGV $$; fi
EOF
chmod +x "$T/graphix"

pre=
if [ "${FAST:-0}" = 1 ]; then
    cat > "$T/fast.c" <<'EOF'
#define _GNU_SOURCE
#include <dlfcn.h>
#include <time.h>
static struct timespec tenth(const struct timespec *r) {
    long long ns = ((long long)r->tv_sec * 1000000000LL + r->tv_nsec) / 10;
    struct timespec s = { ns / 1000000000LL, ns % 1000000000LL };
    return s;
}
int nanosleep(const struct timespec *r, struct timespec *m) {
    static int (*f)(const struct timespec *, struct timespec *);
    if (!f) f = dlsym(RTLD_NEXT, "nanosleep");
    struct timespec s = tenth(r);
    return f(&s, m);
}
int clock_nanosleep(clockid_t c, int fl, const struct timespec *r, struct timespec *m) {
    static int (*f)(clockid_t, int, const struct timespec *, struct timespec *);
    if (!f) f = dlsym(RTLD_NEXT, "clock_nanosleep");
    if (fl & TIMER_ABSTIME) return f(c, fl, r, m);
    struct timespec s = tenth(r);
    return f(c, fl, &s, m);
}
EOF
    gcc -O2 -shared -fPIC -o "$T/fast.so" "$T/fast.c" -ldl || exit 2
    pre=$T/fast.so
fi

echo "== 1. leakcheck on a shell whose fused mode segfaults"
LD_PRELOAD=$pre "$FUZZ" leakcheck "$T/graphix" 1
echo "exit status: $?"

echo "== 2. leakcheck with a relative binary path"
(cd "$T" && "$FUZZ" leakcheck ./graphix 1)
echo "exit status: $?"

echo "== 3. a program that writes a relative path: check vs minimize"
printf 'sys::fs::write_all(#path: "wrote_into_cwd.txt", "x")\n' > "$T/fswrite.gx"
mkdir -p "$T/check_cwd" "$T/minimize_cwd"
(cd "$T/check_cwd" && "$FUZZ" check "$T/fswrite.gx" >/dev/null 2>&1)
echo "check cwd: [$(ls -A "$T/check_cwd")]"
(cd "$T/minimize_cwd" && "$FUZZ" minimize "$T/fswrite.gx" >/dev/null 2>&1)
echo "minimize cwd: [$(ls -A "$T/minimize_cwd")]"
