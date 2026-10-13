#!/bin/sh
# t-image-02: a warm start prints abstract types as "abstract": the image
# never re-registers their names (ABSTRACT_NAMES is filled only by
# AbstractId::of/declared, which a restored session never calls).
#
# Run: sh design/review-2026-10-12/repro/t-image-02.sh [path/to/graphix]
# Expected: every run says "cannot compute Stdio + i64".
# Observed: run 1 (cold, writes the registration image) and --no-cache say
#   "Stdio"; run 2 and --check (warm, registration image restored) say
#   "cannot compute abstract + i64". Same with gui::color(..) and Color.
# Note: sys::io::Stdio is `type Stdio;` (no rep), so it has no
# Env::abstract_reps entry; re-registering from abstract_reps alone would
# not fix this case.
G=${1:-graphix}
D=$(mktemp -d)
printf 'let s = sys::io::stdout(null);\ns + 1\n' > "$D/s.gx"
for i in 1 2; do
  echo "== run $i"
  XDG_CACHE_HOME="$D/cache" timeout -s KILL 60 "$G" "$D/s.gx" 2>&1 | tail -1
done
echo "== --check (warm)"
XDG_CACHE_HOME="$D/cache" timeout -s KILL 60 "$G" --check "$D/s.gx" 2>&1 | tail -1
echo "== --no-cache"
timeout -s KILL 60 "$G" --no-cache "$D/s.gx" 2>&1 | tail -1
rm -rf "$D"
