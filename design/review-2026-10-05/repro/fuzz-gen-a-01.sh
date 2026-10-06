#!/usr/bin/env bash
# fuzz-gen-a-01: soak ignores its base seed, and Rng::new(seed | 1) makes
# adjacent work orders identical.
#
# command: GRAPHIX_FUZZ=/path/to/graphix-fuzz bash design/review-2026-10-05/repro/fuzz-gen-a-01.sh
#
# A soak's k-th work order for source si has seed
# (si+1)*0x9E3779B97F4A7C15 + k (graphix-fuzz/src/lib.rs:4705); the base
# seed `soak` parses (main.rs:942) is only printed, run_aggregator has no
# seed parameter and seed_ctr starts at 0 in every process. So every host
# and every relaunch issues the same orders. The child seeds its
# generator with mutate::Rng::new(order.seed) = Rng(seed | 1)
# (mutate.rs:20), so seeds 2k and 2k+1 are one stream. `gen N S` is the
# generator a Generate work order (count N, seed S) runs: Rng::new(S),
# then gen_program N times (`--reactive`: the Reactive order's).
#
# expected: every order is new work, so seven distinct md5s.
# observed (HEAD c722befe, debug build):
#   generate order 1  a4c8ada494567bdc19516d16c2b23a3d
#   generate order 2  184761653b7d6c858c7c0511b315bf6c
#   generate order 3  184761653b7d6c858c7c0511b315bf6c   (replay of order 2)
#   generate order 4  5eab8ac1e26ce3430b894829b34fe695
#   reactive order 1  021fd68f761d97de4b20adea6c310089
#   reactive order 2  021fd68f761d97de4b20adea6c310089   (replay of order 1)
#   reactive order 3  43c50d3978677b2fb443263df9d96051
#   and the soak arm's `seed` lines: its parse, its banner, a comment.
# The real worker agrees: `printf 'generate 4354685564936845356 4 0 0 0\n'
# | graphix-fuzz gen-batch /abs/out` and the same with ...357 write the
# same verdicts and program texts (only the per-process shape hash in the
# `N` lines differs).

set -euo pipefail
gf=${GRAPHIX_FUZZ:-$HOME/tmp/target/debug/graphix-fuzz}
repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd -P)
batch=64

# order k's seed, (si+1)*0x9E3779B97F4A7C15 + k mod 2^64, for si = 1
# (Generate) and si = 2 (Reactive); literal, since the Reactive ones
# overflow bash arithmetic
gen_seeds=(4354685564936845355 4354685564936845356 4354685564936845357
    4354685564936845358)
rx_seeds=(15755400384260043840 15755400384260043841 15755400384260043842)

for i in "${!gen_seeds[@]}"; do
    printf 'generate order %s  %s\n' "$((i + 1))" \
        "$("$gf" gen "$batch" "${gen_seeds[$i]}" | md5sum | cut -d' ' -f1)"
done
for i in "${!rx_seeds[@]}"; do
    printf 'reactive order %s  %s\n' "$((i + 1))" \
        "$("$gf" gen --reactive "$batch" "${rx_seeds[$i]}" | md5sum | cut -d' ' -f1)"
done

echo "soak arm, every line naming seed:"
awk '/Some\("soak"\) =>/ { on = 1 } on && /Some\("minimize"\) =>/ { on = 0 } on' \
    "$repo/graphix-fuzz/src/main.rs" | grep -n '\bseed\b'
echo "run_aggregator's parameters:"
grep -A5 'pub async fn run_aggregator' "$repo/graphix-fuzz/src/lib.rs"
