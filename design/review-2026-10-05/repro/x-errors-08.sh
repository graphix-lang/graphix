#!/usr/bin/env bash
# x-errors-08: a type name that resolve_visible refuses reports "undefined
# type"; the real reason goes only to the log.
#
# TypeRef::resolve_pure (graphix-types/src/typ/mod.rs:605-615) maps every
# structural error of Env::resolve_visible (an ambiguous glob, `super` past
# the root, a missing module in a path) to log::warn! plus None, and
# lookup_ref_with / check_pending_names then report UnresolvableRef. The
# shell installs a logger only under --log-dir, so the user sees "undefined
# type". The same errors in value position report their reason
# (lookup_bind propagates it). design/module_system.md "Diagnostics" says
# an ambiguity "cannot masquerade as 'undefined type'".
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/x-errors-08.sh
#
# expected: each *_type case reports the same reason as its *_val twin:
#   glob:  `T` is ambiguous: both `a` and `b` provide it; import one explicitly
#   super: `super` goes above the package root
#   nomod: no module `nomod` in `/`
#
# observed (HEAD c722befe, debug build):
#   glob_type:  undefined type T in            glob_val:  `v` is ambiguous: ...
#   super_type: undefined type super::T in     super_val: `super` goes above the package root
#   nomod_type: undefined type self::nomod::T in   nomod_val: no module `nomod` in `/`
#   (a run without --check names the scope `#do4611686018427394743`)
#   With RUST_LOG=warn --log-dir D, D/graphix.log holds:
#   WARN [graphix_types::typ] resolving type `T` in ``: `T` is ambiguous: ...
set -u
G=${GRAPHIX:-graphix}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
printf 'type T = i64;\nlet v = 1\n' > "$D/a.gx"
printf 'type T = string;\nlet v = 2\n' > "$D/b.gx"
printf 'mod a;\nmod b;\nuse a::*;\nuse b::*;\nlet x: T = 1;\nx\n' > "$D/glob_type.gx"
printf 'mod a;\nmod b;\nuse a::*;\nuse b::*;\nlet x = v;\nx\n' > "$D/glob_val.gx"
printf 'let x: super::T = 1;\nx\n' > "$D/super_type.gx"
printf 'let y = 1;\nlet x = super::y;\nx\n' > "$D/super_val.gx"
printf 'let x: self::nomod::T = 1;\nx\n' > "$D/nomod_type.gx"
printf 'let y = 1;\nlet x = self::nomod::y;\nx\n' > "$D/nomod_val.gx"
for f in glob_type glob_val super_type super_val nomod_type nomod_val; do
    printf '%-11s ' "$f:"
    (cd "$D" && timeout -s KILL 30 "$G" --check "$f.gx" 2>&1 | tail -1 | sed 's/^ *1: //')
done
