#!/bin/bash
# fuzz-main-aux-06: SOAK_ASAN exports hard_rss_limit_mb=2048 to the in-process
# regress gate (soak.sh start), whose single check_verdict on one corpus pin
# already peaks near 2 GB RSS without ASan.
#
# Usage: fuzz-main-aux-06.sh <graphix-fuzz binary>
# Part 1: `graphix-fuzz check` runs check_verdict in-process, exactly as one
#   regress slot does (main.rs sets the same stack/activation budgets for both).
#   Observed (debug build, no ASan, HEAD 7324207f):
#     tail-quiet-poll-rederivation-aug2026/01_depth_trip_then_refwrite.gx  maxrss 1983 MB
#     connect-instance-identity-aug2026/02_connect_in_rec_arg.gx           maxrss 1597 MB
#   ASan's shadow (1/8 of touched memory), redzones and quarantine add to that,
#   and regress runs at least two such slots at once.
# Part 2: ASan with hard_rss_limit_mb=2048 kills a process past 2 GB:
#   "AddressSanitizer: hard rss limit exhausted (2048Mb vs 2112Mb)", exit 1.
# Expected: the SOAK_ASAN gate (`"$binary" regress` under the exported
#   ASAN_OPTIONS) completes. Observed by the parts above: it cannot stay under
#   2048 MB, so start() fails under set -e.
set -e
bin=${1:?graphix-fuzz binary}
F=$(dirname "$0")/../../../graphix-fuzz/findings
w=$(mktemp -d)
grep -v '^//' "$F/tail-quiet-poll-rederivation-aug2026/01_depth_trip_then_refwrite.gx" > "$w/p.gx"
XDG_CACHE_HOME=$w/cache timeout -s KILL 180 python3 -I -c '
import resource, subprocess, sys
r = subprocess.run(sys.argv[1:], capture_output=True, text=True)
print(r.stdout.strip(), "| maxrss_MB", resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss // 1024)
' "$bin" check "$w/p.gx"
cat > "$w/big.c" <<'C'
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
int main(){volatile long s=0;for(int i=0;i<30;i++){char*p=malloc(100<<20);memset(p,i+1,100<<20);s+=p[i];usleep(50000);}printf("survived %ld\n",s);return 0;}
C
clang -fsanitize=address -O0 "$w/big.c" -o "$w/big"
ASAN_OPTIONS=hard_rss_limit_mb=2048 timeout -s KILL 60 "$w/big" 2>&1 | grep -m1 'hard rss limit' || true
rm -rf "$w"
