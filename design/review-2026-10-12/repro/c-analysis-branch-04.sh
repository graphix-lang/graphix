#!/bin/sh
# A warm image start never forks a fork point whose later child calls a
# function: the callee is still `Callee::Imaged` when the site decides
# its siblings, so the summary is opaque and the site is Dependent for
# good. Cold: "g 1" prints first (the tuple forked). Warm: "f ..." first.
# usage: c-analysis-branch-04.sh <graphix binary>
G=${1:-graphix}
D=$(dirname "$0")
C=$(mktemp -d)
echo "cold:"; GRAPHIX_PAR=force timeout -s KILL 60 "$G" --no-cache "$D/c-analysis-branch-04.gx" 2>&1 | grep -E '^(f|g) '
XDG_CACHE_HOME=$C timeout -s KILL 60 "$G" --warm "$D/c-analysis-branch-04.gx" >/dev/null 2>&1
echo "warm:"; XDG_CACHE_HOME=$C GRAPHIX_PAR=force timeout -s KILL 60 "$G" "$D/c-analysis-branch-04.gx" 2>&1 | grep -E '^(f|g) '
rm -rf "$C"
