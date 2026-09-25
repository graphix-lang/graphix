#!/bin/bash
# soak-macos.sh <campaign> <base-seed> [workers] [fuzz:generate:reactive:typemorph]
#   — macOS launcher (soak.sh needs /proc + setsid; this is the katana
#   equivalent). NOTE the arg order differs from soak.sh.
#
# ONE process, not one per source: the campaign's four sources share a
# single pool that divides the box by MEASURED CPU. Separate lane processes
# could only divide a box through the OS scheduler, and equal worker
# counts bought wildly unequal CPU (13/19/66 measured).
set -euo pipefail
export GRAPHIX_FUZZ_TIMEOUT_SCALE=${GRAPHIX_FUZZ_TIMEOUT_SCALE:-4}
campaign=${1:?campaign}; seed=${2:?base-seed}
workers=${3:-$(( $(sysctl -n hw.ncpu) * 8 ))}
mix=${4:-50:25:25:10}
case $workers in ''|*[!0-9]*|0) echo "workers must be positive" >&2; exit 2 ;; esac
[ "$workers" -le $(( $(sysctl -n hw.ncpu) * 16 )) ] || {
  echo "workers $workers exceeds ncpu*16 — args are <campaign> <base-seed> [workers] [mix]" >&2
  exit 2; }
[[ $mix =~ ^[0-9]+(\.[0-9]+)?(:[0-9]+(\.[0-9]+)?){3}$ ]] || {
  echo "mix must be fuzz:generate:reactive:typemorph, e.g. 50:25:25:10" >&2; exit 2; }
root=~/tmp/target/fuzz; dir="$root/$campaign"
binary=~/tmp/target/release/graphix-fuzz
[ -e "$dir" ] && { echo "campaign dir exists: $dir" >&2; exit 1; }
ulimit -n 10240
"$binary" regress 2>&1 | tee /tmp/soak-regress.$$ | grep -aE "^(regression corpus|fusion manifest): "
grep -q ', 0 regressions' /tmp/soak-regress.$$ || { echo "regress gate failed" >&2; exit 1; }
mkdir -p "$dir/corpus" "$dir/state"
cp "$binary" "$dir/graphix-fuzz"
mv /tmp/soak-regress.$$ "$dir/regress.log"
printf 'workers=%s\nmix=%s\nbase_seed=%s\n' "$workers" "$mix" "$seed" > "$dir/state/config"
GRAPHIX_FUZZ_PAR=$workers GRAPHIX_FUZZ_CORPUS="$dir/corpus" \
  nohup nice -n 19 "$dir/graphix-fuzz" soak forever "$seed" "$mix" \
  > "$dir/soak.log" 2>&1 &
echo $! > "$dir/state/soak.pid"
sleep 2
kill -0 "$(cat "$dir/state/soak.pid")" 2>/dev/null || {
  echo "soak failed:"; tail -5 "$dir/soak.log"; exit 1; }
echo "SOAK UP (workers=$workers mix=$mix)"
