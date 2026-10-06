#!/usr/bin/env bash
# x-image-07: a cache entry that fails to read is never replaced, and a bad
# program entry skips the intact registration entry.
#
# Shell::init (graphix-shell/src/lib.rs:286-310) maps the program entry when
# its file exists, else the registration entry, and arms a save only for an
# entry that is MISSING. When GX::new's restore fails (graphix-rt/src/gx.rs:
# 296-305) the runtime compiles cold and writes neither entry: `save` is None
# for a Load, and `program_image` was not armed because the program entry
# counted as loaded. The bad file stays until the executable's build id
# changes; `--warm` does not replace it either. The warning reaches only
# --log-dir, and it names neither the entry nor its path.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-image-07.sh
#
# expected: a run over a bad entry replaces it (or at least restores the
#   intact registration entry), so the next run is warm again.
# observed (HEAD c722befe, debug build, x86_64; wall ms over two runs):
#   warm 53-72, --no-cache 96-104
#   program entry truncated to 0 bytes (what a crash after the un-fsynced
#     write can leave): 95-109 on every run, the file stays 0 bytes; `--warm`
#     exits 0 and leaves it 0 bytes; log "reading the registration image:
#     InvalidFormat; compiling cold"; 0 bytes on stderr without --log-dir
#     (truncated to 10 bytes instead: the same, with "TooBig")
#   program entry deleted instead: rewritten by the next run, warm after it
#   registration entry truncated to 0 bytes: every --check 80-99 (healthy
#     55-65), the file stays 0 bytes
#   program entry from a host with other CPU features (one ISA flag flipped
#     in the header): 91-100 on every run, never replaced ("the image was
#     written for another isa")
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
cd "$dir" || exit 1
printf 'sys::exit(sys::time::after_idle(duration:1.ms, 0));\n1\n' > quick.gx
run() {
  local s
  s=$(date +%s%N)
  timeout -s KILL 60 "$GRAPHIX" "$@" >/dev/null 2>&1
  echo -n "$(( ($(date +%s%N) - s) / 1000000 ))ms "
}
logged() {
  rm -rf "$dir/log"; mkdir "$dir/log"
  RUST_LOG=info timeout -s KILL 60 "$GRAPHIX" --log-dir "$dir/log" "$@" >/dev/null 2>&1
  grep -h -E "image|isa" "$dir/log/graphix.log" | grep -v "registration image: env" \
    | sed -e 's/^.*\] //' -e "s|$dir/cache/graphix/registration/||" | cut -c1-110
}
size() { stat -c %s "$1"; }

echo "=== a cold run writes both entries"
logged quick.gx
P=$(sed -n 's/.*Program image written to //p' "$dir/log/graphix.log")
R=$(sed -n 's/.*Registration image written to //p' "$dir/log/graphix.log")
cp "$P" p.good; cp "$R" r.good
echo "warm:       $(run quick.gx; run quick.gx; run quick.gx)"
echo "--no-cache: $(run --no-cache quick.gx; run --no-cache quick.gx; run --no-cache quick.gx)"

echo "=== program entry truncated to 0 bytes"
truncate -s 0 "$P"
echo "runs:   $(run quick.gx; run quick.gx; run quick.gx)size $(size "$P")"
run --warm quick.gx >/dev/null; echo "--warm: size $(size "$P")"
echo "stderr without --log-dir: $(RUST_LOG=warn timeout -s KILL 60 "$GRAPHIX" quick.gx 2>&1 >/dev/null | wc -c) bytes"
logged quick.gx

echo "=== program entry deleted instead"
rm -f "$P"
echo "runs:   $(run quick.gx; run quick.gx; run quick.gx)size $(size "$P")"

echo "=== registration entry truncated to 0 bytes"
echo "--check, healthy: $(run --check quick.gx; run --check quick.gx; run --check quick.gx)"
truncate -s 0 "$R"
echo "--check:          $(run --check quick.gx; run --check quick.gx; run --check quick.gx)size $(size "$R")"

echo "=== program entry written on a host with other CPU features"
cp r.good "$R"; cp p.good "$P"
perl -0777 -pi -e 's/(has_sse3=has_sse3=)1/${1}0/' "$P"
echo "runs:   $(run quick.gx; run quick.gx; run quick.gx)differs from the good entry: $(cmp -s p.good "$P" && echo no || echo yes)"
logged quick.gx
