#!/usr/bin/env bash
# fuzz-mutate-03: ring novelty compares shape hashes keyed per process,
# so a child's signatures never collide with another child's.
#
# mutate::shape_stats hashes each node with ahash::AHasher::default(),
# whose keys ahash (runtime-rng, the default feature) draws from
# getrandom once per process. In a soak, every work order runs in a
# fresh `gen-batch` child that computes the signatures it reports on its
# `N <sig> <len>` lines (lib.rs run_work_order), and the parent
# deduplicates them in ring_sigs (lib.rs run_aggregator). This script
# runs the SAME work order through two gen-batch children: both generate
# the identical program, and each reports a different signature for it.
#
# command: GRAPHIX_FUZZ=/path/to/graphix-fuzz bash design/review-2026-10-05/repro/fuzz-mutate-03.sh
#
# expected: an identical program has one shape signature in every
#   process, so the parent's ring_sigs.insert admits it once.
# observed (HEAD c722befe, debug build):
#   seed 2: same program text, sig 14060878187934638162 in one child,
#           14768325854679850623 in the other
#   seed 8: `{ let f = |x: i64| -> i64 x + 1; let f = |n: i64| -> i64
#           f(n) * 2; f(100) }`, sig 12871647510302336008 vs
#           15984182377221457343
#   so ring_sigs.insert succeeds for both, the ring holds two copies and
#   FuzzStats::novel counts it twice. (The numbers change every run.)
set -u
FUZZ=${GRAPHIX_FUZZ:-graphix-fuzz}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

# the program text of the first `N` record in a gen-batch output file
novel_prog() {
    local f=$1 ln len
    ln=$(grep -n -m1 '^N ' "$f" | cut -d: -f1)
    len=$(sed -n "${ln}p" "$f" | cut -d' ' -f3)
    tail -n +$((ln + 1)) "$f" | head -c "$len"
}
novel_sig() { grep -m1 '^N ' "$1" | cut -d' ' -f2; }

for seed in 2 8 3 9 12 13 14 15 16 17; do
    printf 'fuzz %s 5 0 0 0\n' "$seed" > "$dir/order"
    timeout -s KILL 170 "$FUZZ" gen-batch "$dir/a" < "$dir/order" > /dev/null 2>&1
    grep -q '^N ' "$dir/a" || continue
    timeout -s KILL 170 "$FUZZ" gen-batch "$dir/b" < "$dir/order" > /dev/null 2>&1
    grep -q '^N ' "$dir/b" || continue
    echo "work order: fuzz seed=$seed count=5"
    echo "program: $(novel_prog "$dir/a")"
    if [ "$(novel_prog "$dir/a")" = "$(novel_prog "$dir/b")" ]; then
        echo "same program text in both children: yes"
    else
        echo "same program text in both children: NO (inconclusive, try another seed)"
        continue
    fi
    echo "child 1 sig: $(novel_sig "$dir/a")"
    echo "child 2 sig: $(novel_sig "$dir/b")"
    if [ "$(novel_sig "$dir/a")" = "$(novel_sig "$dir/b")" ]; then
        echo "signatures agree: fixed"
    else
        echo "signatures differ: BUG (the parent's ring_sigs admits both)"
    fi
    exit 0
done
echo "no seed produced an admitted (N) subject twice; extend the seed list"
exit 2
