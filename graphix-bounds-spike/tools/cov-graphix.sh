#!/bin/sh
# Stand-in for the graphix binary under tree.py / tree_examples.py: runs the
# coverage build and files its profile by whether the check by bounds is on.
# Profiles merge as they are written (a pool of 8 files per side), so a
# corpus of hundreds of runs stays a few hundred MB.
#   GRAPHIX=~/ptmp/bounds-study/tools/cov-graphix.sh COV_OUT=<dir> tree.py ..
#   COV_FORCE_OFF=1 runs every program with the check by bounds off.
#   COV_CACHED=1 drops --no-cache and keeps a session cache under COV_OUT, so
#   the packages are restored from an image and only the program's own check
#   is in the profile (run one program first to write the image, and delete
#   the profiles it left).
if [ -n "$COV_FORCE_OFF" ]; then unset GRAPHIX_BOUNDS GRAPHIX_BOUNDS_AUDIT; fi
if [ -n "$GRAPHIX_BOUNDS" ]; then d=on; else d=off; fi
mkdir -p "$COV_OUT/$d"
if [ -n "$COV_CACHED" ]; then
  export XDG_CACHE_HOME="$COV_OUT/cache"
  for a in "$@"; do
    shift
    [ "$a" = "--no-cache" ] || set -- "$@" "$a"
  done
fi
LLVM_PROFILE_FILE="$COV_OUT/$d/%8m.profraw" exec "$HOME/tmp/target/cov/debug/graphix" "$@"
