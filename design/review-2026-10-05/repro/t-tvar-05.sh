#!/usr/bin/env bash
# t-tvar-05: a module check that would weld two cells created outside the
# module (two unannotated outer lets) skips the weld and records nothing
# (TVar::merge_into, graphix-types/src/typ/tvar.rs:787 `(true, true) =>
# return`), so the module is not refused and the program runs with a type
# the check never established.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-tvar-05.sh
#
# expected (CLAUDE.md, Module system: "a module's check that would write a
# cell created outside the module is refused and the write not made"):
#   A1, B1. --check refuses module m ("the check decides a type the code
#           around the module left open: annotate the binding it decides"),
#           as the same statements without m.gxi are refused (A2, B2).
# observed (HEAD c722befe, debug build):
#   A1. exit 0, and GRAPHIX_TASK_AUDIT=1 prints no FOREIGN-WRITE (A3: 0)
#   A2. refused: "y is i64, inferred from an earlier use, and cannot hold string"
#   A4. JIT run: the runtime panics, "kernel param `x`: runtime String("s")
#       does not match the compiled Scalar(I64) slot", "Error: runtime did
#       not respond" (fusion/kernel.rs:243 panics unconditionally)
#   A5. --no-fusion run: "arith error ... Unexpected `s`": x holds "s", typed i64
#   B1. exit 0 (the connect's x ⊇ y meets two frozen task-0 cells:
#       OpenPair::Merge -> alias_cells -> merge_into -> (true, true) return)
#   B2. refused: "cannot compute '_: string + i64"
#   B3. JIT run prints [] [] [1]: the callback adds 1 to the string "s"
# Shape A takes the name-alias path (OpenPair::AliasLeft -> alias ->
# merge_into(Name)), shape B the cell-merge path; both end at the same
# unrecorded return.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
check() { timeout -s KILL 60 "$GRAPHIX" --check "$1" 2>&1 | tail -1; echo "  exit ${PIPESTATUS[0]}"; }
run() { timeout -s KILL 10 "$GRAPHIX" --no-cache "$@" 2>&1 | head -4; }

mkdir -p "$dir/a" "$dir/a2" "$dir/b" "$dir/b2"
cat > "$dir/a/main.gx" <<'EOF'
let x = never();
let y = never();
mod m;
x <- 1;
y <- "s";
x + 1
EOF
: > "$dir/a/m.gxi"
echo 'super::x <- super::y' > "$dir/a/m.gx"
cp "$dir/a/main.gx" "$dir/a/m.gx" "$dir/a2/"

cat > "$dir/b/main.gx" <<'EOF'
let x = [];
let y = [];
mod m;
y <- ["s"];
let r = array::map(x, |v| v + 1);
r
EOF
cp "$dir/a/m.gxi" "$dir/a/m.gx" "$dir/b/"
cp "$dir/b/main.gx" "$dir/b/m.gx" "$dir/b2/"

echo "A1. --check with m.gxi (expected: refused)"; check "$dir/a/main.gx"
echo "A2. --check without m.gxi"; check "$dir/a2/main.gx"
echo "A3. FOREIGN-WRITE lines under GRAPHIX_TASK_AUDIT=1:"
GRAPHIX_TASK_AUDIT=1 timeout -s KILL 60 "$GRAPHIX" --check "$dir/a/main.gx" 2>&1 | grep -c FOREIGN-WRITE
echo "A4. JIT run"; run "$dir/a/main.gx"
echo "A5. node-walk run"; run --no-fusion "$dir/a/main.gx"
echo "B1. --check with m.gxi (expected: refused)"; check "$dir/b/main.gx"
echo "B2. --check without m.gxi"; check "$dir/b2/main.gx"
echo "B3. JIT run"; run "$dir/b/main.gx"
