#!/usr/bin/env bash
# t-misc-02: image encode memory grows about cubically with type depth;
# the stack pin's `flattype` shape at its FLAT_DEPTH (3000) is OOM-killed.
#
# The program is deep_nesting.rs's `flattype`: `let x0 = 1; let x1 = [x0];
# .. let xN = [x(N-1)]; xN`. Each let fuses as a region of its own, and
# fusion freezes a fresh deep copy of its param type and its return type
# (kernel_abi::freeze_for_abi_d_inner, 2 Freeze calls per let). The
# program image keys every Arc node of each copy in `type_keys`
# (image::shared_key, graphix-types/src/image/mod.rs:1232), each entry
# holding the node's WHOLE key, its children's bytes included: d^2/2 bytes
# per copy of depth d, about N^3/3 bytes over the program.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-misc-02.sh [N..]
#   (default N = 1000 1500 2000; each cold run starts from an empty cache)
#
# expected: the cold run (image cache on, the default) costs about what
# the --no-cache run costs plus the image, which is a few MB; the design
# (design/program_image.md, "so the writer's walk is linear") says so.
#
# observed (HEAD c722befe, debug build), peak RSS:
#   N     --no-cache   cold --no-fusion   cold (default)
#   1000    197 MB          82 MB             605 MB
#   1500    347 MB          81 MB            1582 MB
#   2000    557 MB          96 MB            3424 MB
#   3000   1109 MB         113 MB            exit 251, 6161 MB: OOM-killed
#                                            by the review probe's 6 GB cap
# The excess over --no-cache (408 / 1235 / 2867 MB) tracks N^3/3 bytes
# (333 / 1125 / 2667 MB). valgrind massif at N=1000, peak 608 MB:
# shared_key's key boxes 293 MB, the type_keys table 154 MB, the frozen
# copies (freeze_for_abi_d_inner) 72 MB. The image file is 3.6 MB at
# N=2000, and the warm start from it peaks at 97 MB.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
peak() {
    python3 - "$@" <<'EOF'
import resource, subprocess, sys
p = subprocess.run(sys.argv[1:], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
rss = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss // 1024
print(f"exit {p.returncode}, peak {rss} MB")
EOF
}
ns=("$@")
[ ${#ns[@]} -eq 0 ] && ns=(1000 1500 2000)
for n in "${ns[@]}"; do
    f="$dir/flat$n.gx"
    {
        echo "let x0 = 1;"
        for ((i = 1; i <= n; i++)); do echo "let x$i = [x$((i - 1))];"; done
        echo "sys::exit(sys::time::after_idle(duration:100.ms, 0));"
        echo "x$n"
    } > "$f"
    echo "N=$n --no-cache:       $(peak timeout -s KILL 170 "$GRAPHIX" --no-cache "$f")"
    rm -rf "$dir/cache"
    echo "N=$n cold --no-fusion: $(peak timeout -s KILL 170 "$GRAPHIX" --no-fusion "$f")"
    rm -rf "$dir/cache"
    echo "N=$n cold:             $(peak timeout -s KILL 170 "$GRAPHIX" "$f")"
    rm -rf "$dir/cache"
done
