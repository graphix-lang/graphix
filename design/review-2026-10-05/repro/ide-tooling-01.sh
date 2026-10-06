#!/usr/bin/env bash
# ide-tooling-01: the Zed extension pins its grammar to rev 4057fc63
# (2026-05-06) while its query files are symlinks to HEAD's queries;
# highlights.scm and indents.scm do not compile against that grammar, so
# Zed fails to load the Graphix language at all.
#
# Zed (extension_builder.rs checkout_repo/compile_grammar) fetches
# [grammars.graphix] repository@rev and compiles that rev's
# ide/tree-sitter-graphix/src/parser.c. language_core Grammar::with_queries
# compiles highlights first and propagates a Query::new error with `?`
# ("Error loading highlights query"); language_registry load_language then
# logs "failed to load language Graphix" and the language never loads:
# .gx/.gxi open as plain text and graphix-lsp, bound to the language, never
# starts. (Checked against Zed 1.1.6, sha 96962914, the installed version.)
# Zed does not read locals.scm.
#
# This script builds the pinned rev's grammar with the tree-sitter CLI and
# compiles each of the extension's query files against it, then against
# HEAD's grammar as the control. Nothing in the checkout is written.
#
# command: bash design/review-2026-10-05/repro/ide-tooling-01.sh
#   (TS=/path/to/tree-sitter overrides the CLI; the default is the npm one
#   under ide/tree-sitter-graphix/node_modules)
#
# expected: every query Zed loads compiles against the grammar Zed builds
#   (exit 0)
# observed (HEAD c722befe, tree-sitter 0.24.7), exit 1:
#   pin 4057fc63, 32 grammar.js commits behind HEAD
#   4057fc63 highlights.scm  Query error at 9:2. Invalid node type attribute
#   4057fc63 indents.scm     Query error at 6:4. Invalid node type seq_block
#   HEAD     highlights.scm  ok
#   HEAD     indents.scm     ok
set -u
root=$(git -C "$(dirname "$0")" rev-parse --show-toplevel) || exit 2
ts=${TS:-$root/ide/tree-sitter-graphix/node_modules/tree-sitter-cli/tree-sitter}
[ -x "$ts" ] || ts=$(command -v tree-sitter) || { echo "no tree-sitter CLI" >&2; exit 2; }
pin=$(sed -n 's/^rev = "\([0-9a-f]*\)"$/\1/p' "$root/ide/editors/zed/extension.toml")
[ -n "$pin" ] || { echo "no grammar rev in extension.toml" >&2; exit 2; }
behind=$(git -C "$root" log --oneline "$pin..HEAD" -- ide/tree-sitter-graphix/grammar.js | wc -l)
echo "pin ${pin:0:8}, $behind grammar.js commits behind HEAD"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
printf '{"parser-directories": []}\n' >"$tmp/config.json"
printf 'let x = 1;\nx + 1\n' >"$tmp/probe.gx"
status=0
for rev in "$pin" HEAD; do
    mkdir -p "$tmp/$rev"
    git -C "$root" archive "$rev" ide/tree-sitter-graphix | tar -x -C "$tmp/$rev"
    for q in highlights indents; do
        if out=$(cd "$tmp/$rev/ide/tree-sitter-graphix" &&
            XDG_CACHE_HOME="$tmp/cache" TREE_SITTER_LIBDIR="$tmp/lib-$rev" \
                "$ts" query --config-path "$tmp/config.json" -q \
                "$root/ide/editors/zed/languages/graphix/$q.scm" "$tmp/probe.gx" 2>&1); then
            res=ok
        else
            res=$(printf '%s\n' "$out" | grep -o 'Query error.*' || printf '%s\n' "$out" | tail -n 1)
            [ "$rev" = "$pin" ] && status=1
        fi
        printf '%-8s %-15s %s\n' "${rev:0:8}" "$q.scm" "$res"
    done
done
exit $status
