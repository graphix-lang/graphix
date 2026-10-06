#!/usr/bin/env bash
# f-jit-03: link_batch's spawn-failure fallback relinks functions already
# moved into the dropped closure (graphix-compiler/src/fusion/emit/jit.rs,
# Jit::link_batch). take_functions empties `pending` into `work`, `work`
# moves into the thread closure, a failed spawn drops the closure, and the
# Err arm's self.link(pending) compiles the Function::new() placeholders.
#
# The program has 512 #[native] regions (the jit_arena_rotation.rs shape),
# so the 256th starts a batch. An LD_PRELOAD shim makes pthread_create fail
# with EAGAIN (what RLIMIT_NPROC, pids.max or a refused stack mmap return)
# for the first thread asking for an 8 MiB stack: the batch thread. The
# batch's compile workers come later, from inside that thread.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/f-jit-03.sh
#          (needs gcc)
#
# expected: every run prints 392960 and exits 0. With the batch thread
#   refused, the batch links synchronously, as the Err arm intends.
# observed (HEAD c722befe, debug build):
#   1. no shim: 392960, exit 0
#   2. batch thread refused: no output, exit 1. stderr shows 16 x
#      "panicked at .../cranelift-codegen-0.131.3/src/remove_constant_phis.rs:265:10:
#       remove_constant_phis: entry block unknown"
#      (from fusion::emit::jit::backend), then
#      "Error: loading initial modules / Caused by: channel closed"
#   3. control, only the backend_all workers refused (spawns 2-16, 18-32;
#      their failure is handled): 392960, exit 0
# Without the shim, a cgroup pids limit hits the same path:
#   TOKIO_WORKER_THREADS=2 GRAPHIX_PAR=off RAYON_NUM_THREADS=2 GRAPHIX_EVAL_THREADS=1 \
#   RUST_BACKTRACE=1 systemd-run --user --scope -p TasksMax=46 -- graphix --no-netidx \
#   --no-cache regions.gx
# crashed in 2 of 3 runs with the same panic. Its backtrace reads
# backend <- backend_all <- Jit::link <- Jit::link_batch <- FusionCtx::with_jit
# <- fusion::build_region. Lower limits fail earlier, in rayon's global pool.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

cat > "$dir/failspawn.c" <<'EOF'
#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int (*create_fn)(pthread_t *, const pthread_attr_t *, void *(*)(void *), void *);
static atomic_int matched;

/* FAILSPAWN=",1," fails the 1st pthread_create asking for an 8 MiB stack,
   ",2,3," the 2nd and 3rd, and so on. */
int pthread_create(pthread_t *t, const pthread_attr_t *attr, void *(*start)(void *),
                   void *arg) {
    static create_fn real;
    if (!real) real = (create_fn)dlsym(RTLD_NEXT, "pthread_create");
    const char *which = getenv("FAILSPAWN");
    size_t sz = 0;
    if (which && attr && pthread_attr_getstacksize(attr, &sz) == 0 && sz == 8u << 20) {
        char key[16];
        snprintf(key, sizeof key, ",%d,", atomic_fetch_add(&matched, 1) + 1);
        if (strstr(which, key)) {
            fprintf(stderr, "[failspawn] 8 MiB thread %s refused (EAGAIN)\n", key);
            return EAGAIN;
        }
    }
    return real(t, attr, start, arg);
}
EOF
gcc -O2 -shared -fPIC -o "$dir/failspawn.so" "$dir/failspawn.c" -ldl || exit 2

python3 - "$dir/regions.gx" <<'EOF'
import sys
n = 512
s = "let x = sys::time::after_idle(duration:1.ms, 3);\n"
s += "".join(f"let r{i} = #[native] (x * {i} + 1);\n" for i in range(n))
s += "let total = " + " + ".join(f"r{i}" for i in range(n)) + ";\n"
s += "println(total);\n"
s += f"sys::exit(select total {{ {sum(3 * i + 1 for i in range(n))} => 0, _ => 1 }})\n"
open(sys.argv[1], "w").write(s)
EOF

run() {
  echo "== $1"
  env "${@:2}" timeout -s KILL 120 "$GRAPHIX" --no-netidx --no-cache "$dir/regions.gx" \
    > "$dir/out" 2> "$dir/err"
  echo "exit $?, stdout: $(tr '\n' ' ' < "$dir/out")"
  echo "cranelift panics: $(grep -c 'remove_constant_phis: entry block unknown' "$dir/err")"
  grep -m1 -A3 '^Error' "$dir/err" || true
}

run "no shim"
run "batch thread refused" LD_PRELOAD="$dir/failspawn.so" FAILSPAWN=",1,"
workers=",$(seq -s, 2 16),$(seq -s, 18 32),"
run "control: only backend_all workers refused" LD_PRELOAD="$dir/failspawn.so" FAILSPAWN="$workers"
