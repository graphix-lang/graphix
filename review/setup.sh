#!/bin/bash
# setup.sh: build the debug binaries at HEAD and install the memory-capped
# wrappers the reviewers run (WORK/bin/graphix, WORK/bin/graphix-fuzz).
# REVIEW_DATE=YYYY-MM-DD review/setup.sh
set -euo pipefail
: "${REVIEW_DATE:?set REVIEW_DATE=YYYY-MM-DD}"
HERE=$(cd "$(dirname "$0")" && pwd); REPO=$(dirname "$HERE")
W=$HOME/tmp/review-$REVIEW_DATE; B=$W/bin
mkdir -p "$B/real" "$W/work" "$REPO/design/review-$REVIEW_DATE/repro"
cd "$REPO"
cargo build --bin graphix --bin graphix-fuzz
T=$(cargo metadata --format-version 1 --no-deps | python3 -c 'import json,sys; print(json.load(sys.stdin)["target_directory"])')
cp "$T/debug/graphix" "$T/debug/graphix-fuzz" "$B/real/"
cp "$HERE/probe-run" "$B/probe-run"; chmod +x "$B/probe-run"
printf '#!/bin/bash\n# Memory-capped probe binary: see probe-run (OOM kill => message + exit 251).\nexec %s/probe-run 6G %s/real/graphix "$@"\n' "$B" "$B" > "$B/graphix"
cat > "$B/graphix-fuzz" <<WRAP
#!/bin/bash
# Memory-capped probe binary: see probe-run. One program at a time: no soaks.
case "\$1" in check|run|minimize|typemorph) ;; *) echo "graphix-fuzz: only check|run|minimize|typemorph <file> here" >&2; exit 2;; esac
exec $B/probe-run 10G $B/real/graphix-fuzz "\$@"
WRAP
chmod +x "$B/graphix" "$B/graphix-fuzz"
git rev-parse --short=9 HEAD > "$W/HEAD"
echo "binaries at $(cat "$W/HEAD") in $B"
