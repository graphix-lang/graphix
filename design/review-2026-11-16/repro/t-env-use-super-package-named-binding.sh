#!/bin/sh
# Title: `use super::NAME` of a top-level binding named like a package is dropped
#
# Command: sh design/review-2026-11-16/repro/t-env-use-super-package-named-binding.sh
#   (GRAPHIX defaults to /home/eric/tmp/review-2026-11-16/bin/graphix)
#
# resolve_use_item (graphix-compiler/src/node/module.rs) returns
# UseInstall::Nothing for every import whose target scope is "/" and whose
# name is a registered package, "the prelude already provides every package
# name as a path root". That is right for `use db;`. It is wrong for
# `use super::db;` (or `use package::db;`) in a module declared at the top
# level, whose anchor is "/" too: where the program's top level declares a
# binding `db`, the import of the binding is lost with it.
#
# Expected: every program prints 43 (the dynamic one "ok 42").
# Observed:
#   == static, db:   Error ... db not defined
#   == static, dbx:  43                      (control: any other name works)
#   == static, db as d: 43                   (a rename works)
#   == static, glob: 43                      (`use super::*` works)
#   == dynamic, str: "err error:`DynamicLoadError(\"... str not defined\")"
#      the sig's `use super::str` is accepted by the check as a grant and
#      grants nothing: the body fails to load.
G=${GRAPHIX:-/home/eric/tmp/review-2026-11-16/bin/graphix}
D=$(mktemp -d "${TMPDIR:-/tmp}/t-env.XXXXXX")
trap 'rm -rf "$D"' EXIT
cd "$D" || exit 1
run() { timeout -s KILL 60 "$G" --no-cache main.gx 2>&1 | tail -3; }
main() { printf 'let %s = 42;\nmod a;\nsys::exit(sys::time::after_idle(duration:200.ms, 0));\na::k\n' "$1" > main.gx; }

echo "== static, db"
main db; printf 'use super::db;\nlet k = db + 1\n' > a.gx; run
echo "== static, dbx"
main dbx; printf 'use super::dbx;\nlet k = dbx + 1\n' > a.gx; run
echo "== static, db as d"
main db; printf 'use super::db as d;\nlet k = d + 1\n' > a.gx; run
echo "== static, glob"
main db; printf 'use super::*;\nlet k = db + 1\n' > a.gx; run
echo "== dynamic, str"
cat > main.gx <<'GX'
let str = 42;
let st = mod m dynamic { sig { use super::str; val w: i64 }; source "let w = str" };
sys::exit(sys::time::after_idle(duration:400.ms, 0));
select st { error as e => "err [e]", null as _ => "ok [m::w]" }
GX
run
