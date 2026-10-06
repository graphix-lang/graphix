#!/usr/bin/env bash
# shell-07: --warm exits 0 for a program that fails to compile, and silently
# skips --check.
#
# Shell::run (graphix-shell/src/lib.rs:444) returns Ok(()) right after init()
# when --warm is set. GX::new keeps a program compile error in `program`
# (graphix-rt/src/gx.rs:318-331) and drops the program-image sender; only
# load_env's gx.program() would report the error, and --warm never reaches
# it. Check mode compiles no program in init(), so --warm --check (and
# --warm --expand) never check. --warm --no-cache does nothing at all.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/shell-07.sh
#
# expected: a program that fails to compile fails the --warm run (non-zero
#   exit, the compile error on stderr); --warm --check checks (or the flags
#   are refused together).
# observed (HEAD c722befe, debug build):
#   plain run broken.gx           exit 1, the type error on stderr
#   --warm good.gx                exit 0, 2 cache entries (registration + program)
#   --warm broken.gx              exit 0, 0 bytes stderr, 1 cache entry (registration only)
#   --check broken.gx             exit 1
#   --warm --check broken.gx      exit 0, 0 bytes stderr
#   --warm --no-cache broken.gx   exit 0, 0 bytes stderr, no cache entry
#   --warm --log-dir broken.gx    exit 0; under RUST_LOG=warn the log's only line
#                                 is "WARN [graphix_shell] Program image not
#                                 taken: runtime exited" (the runtime did not
#                                 exit; the program failed to compile)
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
cd "$dir" || exit 1
printf 'let x: i64 = "not an int";\nx\n' > broken.gx
printf 'let x: i64 = 42;\nx\n' > good.gx
probe() {
  local label=$1; shift
  rm -rf "$dir/cache"; mkdir "$dir/cache"
  XDG_CACHE_HOME="$dir/cache" timeout -s KILL 120 "$GRAPHIX" "$@" >out 2>err
  local code=$?
  printf '%-30s exit %s, %s bytes stderr, %s cache entries\n' "$label" "$code" \
    "$(wc -c <err)" "$(find "$dir/cache" -type f | wc -l)"
}
probe "plain run broken.gx" --no-cache broken.gx
probe "--warm good.gx" --warm good.gx
probe "--warm broken.gx" --warm broken.gx
probe "--check broken.gx" --check broken.gx
probe "--warm --check broken.gx" --warm --check broken.gx
probe "--warm --no-cache broken.gx" --warm --no-cache broken.gx
mkdir "$dir/log"
RUST_LOG=warn probe "--warm --log-dir broken.gx" --log-dir "$dir/log" --warm broken.gx
echo "log: $(grep -v '^$' "$dir/log/graphix.log")"
