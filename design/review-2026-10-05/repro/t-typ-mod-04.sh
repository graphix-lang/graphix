#!/usr/bin/env bash
# t-typ-mod-04: --check compiles a script's names at the root, the run
# compiles them in a #do block, so the two disagree on a script's paths.
#
# GXRt::check (graphix-rt/src/gx.rs:858, compile_script at Scope::root())
# puts a script's top-level names at `/`. load_program, the run path
# (gx.rs:909-913), wraps the file in a Block, which compile() scopes as
# `/#do<id>` (graphix-compiler/src/node/compiler.rs:337). `self::x`
# anchors at mod_root(scope) (graphix-types/src/env.rs:820), which strips
# the `#do` level back to `/`, where the run has no names. The LSP checks
# through the same GXRt::check, so the editor shows these files clean.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-typ-mod-04.sh
#
# expected (CLAUDE.md: the check runs a script's file "as it runs";
# design/module_system.md: "Check mode ... and load mode ... agree"):
# each case gets the same verdict from --check and from the run.
#
# observed (HEAD c722befe, debug build):
#   A `self::a`:        check exit 0; run "self::a not defined"
#   B `self::T`:        check exit 0; run "undefined type self::T in #do<id>"
#   C `use self::m::x`: check exit 0; run "use: no module `self::m` in scope"
#   D `mod str;`:       check "duplicate module definition str"; run prints 42
#   (control `package::a`: 42 in both)
#   `graphix lsp`, driven over stdio with case A's file open, publishes no
#   diagnostic for it, and "self::zz not defined" for `self::zz + 1`.
# Case D is the same split the other way: the shell's module VFS serves
# the stdlib's `str` source for `mod str;` in both modes; the check puts
# it at `/str`, where the registered package already is, the run at
# `/#do<id>/str`.
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
exit_line='sys::exit(sys::time::after_idle(duration:100.ms, 0));'

mkdir -p "$dir/a" "$dir/b" "$dir/c" "$dir/d" "$dir/p"
printf '%s\n' 'let a = 41;' "$exit_line" 'self::a + 1' > "$dir/a/main.gx"
printf '%s\n' 'type T = i64;' 'let x: self::T = 41;' "$exit_line" 'x + 1' > "$dir/b/main.gx"
printf '%s\n' 'let x = 41' > "$dir/c/m.gx"
printf '%s\n' 'mod m;' 'use self::m::x;' "$exit_line" 'x + 1' > "$dir/c/main.gx"
printf '%s\n' 'mod str;' "$exit_line" '42' > "$dir/d/main.gx"
printf '%s\n' 'let a = 41;' "$exit_line" 'package::a + 1' > "$dir/p/main.gx"

for c in a b c d p; do
  echo "== case $c"
  check=$(timeout -s KILL 60 "$GRAPHIX" --check "$dir/$c/main.gx" 2>&1)
  echo "  check: exit $? $(printf '%s' "$check" | tail -n 1)"
  run=$(timeout -s KILL 60 "$GRAPHIX" --no-cache "$dir/$c/main.gx" 2>&1)
  echo "  run:   exit $? $(printf '%s' "$run" | tail -n 1)"
done
