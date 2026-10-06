#!/usr/bin/env bash
# examples-02: bench/run.sh node-walks par_mandel.gx, which needs ~620 GB.
# run.sh globs bench/*.gx, which now includes the par_* benches written for
# par.sh, and runs every program a second time under --no-fusion.
# par_mandel.gx (800x600, 256 max iterations) node-walks as 22,784,592
# retained `iterate` activations in its first cycle, ~27 KB each: the
# node-walk pass can never finish, and uncapped it runs the host out of
# memory long before run.sh's 120 s timeout.
#
# command: bash design/review-2026-10-05/repro/examples-02.sh ~/tmp/target/quick/graphix
#   Runs an unmodified copy of bench/run.sh (1 iteration) over an unmodified
#   copy of bench/par_mandel.gx alone. Every graphix run is wrapped in a
#   systemd scope with MemoryMax=$CAP (default 6G) and no swap, so the probe
#   cannot take the host down; the wrapper logs each run's flags, exit code,
#   wall time and the scope's result. SLICE=<name>.slice puts the scopes in
#   a slice of your choosing.
#
# expected: run.sh leaves par_mandel to par.sh (or produces a node-walk
#   time for it).
# observed (HEAD c722befe, default fork mode), quick build:
#   bench                      jit(s)   node-walk(s)      speedup
#   par_mandel         0.433063268661499           fail            -
#   run: --no-netidx par_mandel.gx -> rc=0 wall=0.8s result=success
#   run: --no-netidx --no-fusion par_mandel.gx -> rc=137 wall=5.0s result=oom-kill
#   dev build: the same; jit 0.40-0.84 s, the node-walk killed at the cap
#   after 4.7-8.8 s. That is over 1 GB/s: a 62 GB box is out of memory in
#   about a minute.
# scale (peak RSS, --no-fusion, par_mandel cut to w x h and one cycle):
#   20x15: 14,042 activations, 437 MB; 40x30: 58,886 activations, 1,633 MB
#   (1,627 MB with GRAPHIX_PAR=off); fused, either size: 60 MB. About 27 KB
#   per activation (design/tail_calls_are_calls.md: ~25 KB).
set -u
GRAPHIX=${1:?usage: examples-02.sh /path/to/graphix}
CAP=${CAP:-6G}
repo=$(cd "$(dirname "$0")/../../.." && pwd)
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
cp "$repo/bench/run.sh" "$repo/bench/par_mandel.gx" "$dir/"
log="$dir/runs.log"
cat > "$dir/capped" <<EOF
#!/usr/bin/env bash
unit=examples02-\$\$-\$RANDOM
s=\$(date +%s.%N)
systemd-run --user --scope --quiet ${SLICE:+--slice=$SLICE} --unit="\$unit" \\
  -p MemoryMax=$CAP -p MemorySwapMax=0 -p OOMPolicy=stop -- "$GRAPHIX" "\$@"
rc=\$?
e=\$(date +%s.%N)
res=\$(systemctl --user show -p Result --value "\$unit.scope" 2>/dev/null)
systemctl --user reset-failed "\$unit.scope" 2>/dev/null
echo "run: \${*%%/*}\$(basename "\${@: -1}") -> rc=\$rc wall=\$(awk -v a=\$s -v b=\$e 'BEGIN{printf "%.1f", b-a}')s result=\${res:-gone}" >> "$log"
exit \$rc
EOF
chmod +x "$dir/capped"
timeout -s KILL 175 bash "$dir/run.sh" 1 "$dir/capped"
cat "$log"
