#!/usr/bin/env bash
# package-01: `graphix package create` accepts names whose scaffold cannot
# build (dash, uppercase, digit-first, empty). create_package
# (graphix-package/src/lib.rs:180) admits any [A-Za-z0-9-] short name, and
# its message advertises graphix-package-[-a-z]+, but line 197 renders the
# raw short name into identifiers: skel/lib.rs NAME "{{name}}_example" and
# skel/mod.gx '{{name}}_example. The scaffold's build.rs runs
# graphix_ast_pack::emit, which parses mod.gx with the same parser::parse
# that `graphix fmt` uses, so the fmt step below is the build's first
# failure. defpackage! (graphix-derive/src/lib.rs:286-291) would also
# refuse NAME "my-pkg_example": a `-`, and it does not start with
# PACKAGE_NAME `my_pkg`.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/package-01.sh
#
# expected: `create my-pkg` is refused, or writes my_pkg_example; every
#   scaffold that create accepts parses (fmt prints it, rc=0), as the
#   `mypkg` control does.
# observed (HEAD c722befe, debug build):
#   create my-pkg rc=0, lib.rs `const NAME: &str = "my-pkg_example";`,
#     fmt: "Parse error at line: 2, column: 37 Unexpected `-`" rc=1
#   create Foo rc=0, fmt: "line: 2, column: 35 Unexpected `F`" rc=1
#   create 2d rc=0, fmt: "line: 2, column: 35 Unexpected `2`" rc=1
#   create graphix-package- rc=0 (empty short name), NAME "_example",
#     fmt: parse error at `_` rc=1
#   create mypkg rc=0, fmt prints the file rc=0 (control)
#   create my_pkg: "invalid package name, name must match
#     graphix-package-[-a-z]+" rc=1, so a multi-word name has no working
#     spelling: `_` is refused and `-` builds nothing
#   every Cargo.toml: repository = "https://github.com//graphix-package-<name>"
#     ({{user}} is never supplied)
#   create x --dir <missing>: "Error: No such file or directory (os error 2)"
#     (line 177's `?`, not the intended "base path ... does not exist")
set -u
G=${GRAPHIX:-graphix}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
export HOME=$T/home XDG_CACHE_HOME=$T/cache XDG_DATA_HOME=$T/data XDG_CONFIG_HOME=$T/config
mkdir -p "$HOME" "$T/out"
for n in mypkg my-pkg Foo 2d graphix-package-; do
    echo "=== create $n"
    timeout -s KILL 60 "$G" package create "$n" --dir "$T/out"
    echo "rc=$?"
    case $n in graphix-package-*) d=$n ;; *) d=graphix-package-$n ;; esac
    grep -n 'repository' "$T/out/$d/Cargo.toml"
    grep -n 'const NAME' "$T/out/$d/src/lib.rs"
    echo "--- fmt --stdout $d/src/graphix/mod.gx (the parse build.rs runs)"
    timeout -s KILL 60 "$G" fmt --stdout "$T/out/$d/src/graphix/mod.gx" 2>&1 | head -6
    echo "rc=${PIPESTATUS[0]}"
done
echo "=== create my_pkg"
timeout -s KILL 60 "$G" package create my_pkg --dir "$T/out" 2>&1 | tail -1
echo "rc=${PIPESTATUS[0]}"
echo "=== create x --dir <missing>"
timeout -s KILL 60 "$G" package create x --dir "$T/missing" 2>&1 | tail -2
