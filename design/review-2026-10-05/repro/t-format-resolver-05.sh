#!/usr/bin/env bash
# t-format-resolver-05: a nested `mod x;` whose file is not beside its
# parent is loaded from the module search path by its leaf name.
# resolve() (graphix-types/src/expr/resolver.rs:598) tries the body's
# prepend (`<dir>/a/` for a.gx) and then the whole global chain, whose
# FilesResolvers ignore the scope: the script's directory, then the
# data dir or GRAPHIX_MODPATH, each with the bare name `util`.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-format-resolver-05.sh
#
# expected (resolver.rs:728 "Sub-modules resolve beside the body's own
# source"; graphix-lsp/src/workspace.rs:188 models `mod foo` at scope
# <rel> as <base>/<rel>/foo.gx only; book: ui/widgets.gx for
# `mod widgets` in ui/mod.gx):
#   1, 2. "module util could not be found: <dir>/a/util.gx or
#         <dir>/a/util/mod.gx: no such file"
#   3.    "module b could not be found: <dir>/a/b.gx or <dir>/a/b/mod.gx"
# observed (HEAD c722befe, debug build):
#   1. ("top-level util", "top-level util"): a::util is <dir>/util.gx
#   2. "type mismatch T does not contain '_N: T": a::util is a second
#      instance of util.gx, so its abstract T is another nominal type
#   3. "import cycle: b -> a -> b": the cycle exists only through the
#      leaf-name fallback (graphix-shell/tests/import_cycle.rs too)
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

echo "1. nested mod util with no a/util.gx:"
mkdir -p "$dir/p1"
cat > "$dir/p1/util.gx" <<'EOF'
let v = "top-level util"
EOF
cat > "$dir/p1/a.gx" <<'EOF'
mod util;
let w = util::v
EOF
cat > "$dir/p1/main.gx" <<'EOF'
mod util;
mod a;
sys::exit(sys::time::after_idle(duration:100.ms, 0));
(util::v, a::w)
EOF
timeout -s KILL 30 "$GRAPHIX" --no-cache "$dir/p1/main.gx" 2>&1

echo "2. the nested copy is a second module instance:"
mkdir -p "$dir/p2"
cat > "$dir/p2/util.gx" <<'EOF'
type T = Abstract<i64>;
let make = || T(1);
let show = |t: T| t.0
EOF
cat > "$dir/p2/a.gx" <<'EOF'
mod util;
let made = util::make()
EOF
cat > "$dir/p2/main.gx" <<'EOF'
mod util;
mod a;
util::show(a::made)
EOF
timeout -s KILL 30 "$GRAPHIX" --check "$dir/p2/main.gx" 2>&1 | tail -1

echo "3. two top-level modules that each mod the other's name:"
mkdir -p "$dir/p3"
printf 'mod b;\nlet x = 1\n' > "$dir/p3/a.gx"
printf 'mod a;\nlet y = 2\n' > "$dir/p3/b.gx"
printf 'mod a;\nmod b;\na::x\n' > "$dir/p3/main.gx"
timeout -s KILL 30 "$GRAPHIX" --check "$dir/p3/main.gx" 2>&1
