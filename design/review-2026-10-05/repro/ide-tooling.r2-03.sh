#!/usr/bin/env bash
# ide-tooling.r2-03: Zed builds a May-2026 grammar but its queries are
# symlinks to HEAD: no Zed highlighting.
#
# ide/editors/zed/extension.toml pins [grammars.graphix] to a rev: Zed
# fetches that rev from the public repo and compiles its
# ide/tree-sitter-graphix/src/parser.c. The extension's
# languages/graphix/{highlights,indents,locals}.scm are symlinks to the
# checkout's canonical queries. This script compiles the pinned rev's
# grammar with the tree-sitter CLI and runs each of the Zed query files
# against it, then against HEAD's grammar as the control.
#
# command: bash design/review-2026-10-05/repro/ide-tooling.r2-03.sh
#   (TS=/path/to/tree-sitter overrides the CLI; the default is the npm
#   one under ide/tree-sitter-graphix/node_modules)
#
# expected: every Zed query compiles against the grammar Zed builds
#   (exit 0)
# observed (HEAD c722befe, pin 4057fc63, tree-sitter 0.24.7), exit 1:
#   4057fc63 highlights.scm  Query error at 9:2. Invalid node type attribute
#   4057fc63 indents.scm     Query error at 6:4. Invalid node type seq_block
#   4057fc63 locals.scm      Query error at 6:2. Invalid node type seq_block
#   HEAD     highlights.scm  ok
#   HEAD     indents.scm     ok
#   HEAD     locals.scm      ok
set -u
root=$(git -C "$(dirname "$0")" rev-parse --show-toplevel) || exit 2
ts=${TS:-$root/ide/tree-sitter-graphix/node_modules/tree-sitter-cli/tree-sitter}
if [ ! -x "$ts" ]; then
    ts=$(command -v tree-sitter) || { echo "no tree-sitter CLI found" >&2; exit 2; }
fi
pin=$(sed -n 's/^rev = "\([0-9a-f]*\)"$/\1/p' "$root/ide/editors/zed/extension.toml")
[ -n "$pin" ] || { echo "no grammar rev in extension.toml" >&2; exit 2; }
queries=$root/ide/editors/zed/languages/graphix
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
printf '{"parser-directories": []}\n' >"$tmp/config.json"
printf 'let x = 42;\nx + 1\n' >"$tmp/sample.gx"
status=0
for rev in "$pin" HEAD; do
    dir=$tmp/$rev
    mkdir -p "$dir"
    git -C "$root" archive "$rev" ide/tree-sitter-graphix | tar -x -C "$dir"
    for q in highlights indents locals; do
        if out=$(cd "$dir/ide/tree-sitter-graphix" &&
            XDG_CACHE_HOME="$tmp/cache" TREE_SITTER_LIBDIR="$tmp/lib-$rev" \
                "$ts" query --config-path "$tmp/config.json" -q \
                "$queries/$q.scm" "$tmp/sample.gx" 2>&1); then
            res=ok
        else
            res=$(printf '%s\n' "$out" | grep -o 'Query error.*' || printf '%s\n' "$out" | tail -n 1)
            [ "$rev" = "$pin" ] && status=1
        fi
        printf '%-8s %-15s %s\n' "${rev:0:8}" "$q.scm" "$res"
    done
done
exit $status
