#!/usr/bin/env bash
# ide-tooling.r2-05: tree-sitter grammar: a lambda body ends at its first
# binary operator, so `|acc, x| acc + x;` parses as `(|acc, x| acc) + x`.
#
# ide/tree-sitter-graphix/grammar.js:717 gives `lambda` no precedence while
# every binary_expression arm is prec.left, and the declared conflicts
# [_expression, lambda], [lambda, _primary_expression], ... let the GLR
# parser keep both readings; when any token follows the lambda (`;`, `,`,
# `)`) it keeps the one that reduces the lambda early (a lambda that ends
# the file parses right). The real parser's lambda body is a full expr()
# (graphix-types/src/expr/parser/lambdaexp.rs:172-176). Every lambda whose
# body's top operator is binary (`|x| x + 1`, `|e| e.kind == p`,
# `|x| x ~ y`, `|x| f(x) + 1`) is truncated. Over the 385 .gx/.gxi files of
# this repo and ../netidx (fuzz findings excluded) the committed parser
# makes 69 lambdas the left operand of a binary_expression, 8 of them in
# graphix-package-netidx-admin, 11 in book examples, 1 in core's mod.gxi.
# The locals query scopes parameters to (lambda), so a parameter used after
# the operator is outside its scope: a locals-aware highlighter (the
# tree-sitter CLI, Helix) colors it as an unresolved name, and node-based
# selection/navigation (Emacs defun/sexp over "lambda", expand-selection)
# sees the short extent. ts_expr/ts_pp (graphix-types/src/expr/test.rs)
# only look for ERROR/MISSING nodes, and there is no test/corpus, so no
# gate pins tree shapes.
#
# The script parses and highlights two lines with the committed parser
# (ide/tree-sitter-graphix/src), then regenerates the grammar with lambda
# wrapped in prec.right(-1, ...) (what let_binding/connect/catch_stmt use)
# and does the same. With that change `tree-sitter generate` reports the
# seven lambda conflicts as unnecessary; with them also removed the corpus
# parses the same (383/385, the same two pre-existing failures) and no
# lambda is a binary operator's left operand.
#
# command (from the repo root; needs node, python3, a C compiler and the
# tree-sitter CLI, by default ide/tree-sitter-graphix/node_modules):
#   bash design/review-2026-10-05/repro/ide-tooling.r2-05.sh
# optionally GRAPHIX=/path/to/graphix to show the real parser's reading.
#
# expected: the lambda holds the sum, (lambda (lambda_params P P)
#   (binary_expression ID ID)), and every acc and x is a <variable>; the
#   real parser prints 3.
#
# observed (HEAD c722befe, tree-sitter 0.24.7; P = a plain parameter,
# ID = a plain reference):
#   == committed parser
#   (let_binding pattern: PAT value: (binary_expression
#     (lambda (lambda_params P P) ID) ID))
#   (apply ID (apply_args (apply_arg NUM) (apply_arg NUM)))
#   let f<variable> = |acc<variable>, x<variable>| acc<variable> + x<namespace>;
#   == lambda: prec.right(-1, ...)
#   (let_binding pattern: PAT value: (lambda (lambda_params P P)
#     (binary_expression ID ID)))
#   (apply ID (apply_args (apply_arg NUM) (apply_arg NUM)))
#   let f<variable> = |acc<variable>, x<variable>| acc<variable> + x<variable>;
#   == real parser: let f = |acc, x| acc + x; f(1, 2)
#   3
set -euo pipefail
REPO=$(cd "$(dirname "$0")/../../.." && pwd)
G=$REPO/ide/tree-sitter-graphix
TS=${TREE_SITTER:-$G/node_modules/.bin/tree-sitter}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
printf 'let f = |acc, x| acc + x;\nf(1, 2)\n' > "$T/x.gx"

setup() { # $1 = name
    mkdir -p "$T/$1/parsers/tree-sitter-graphix" "$T/$1/lib"
    cp -r "$G/grammar.js" "$G/tree-sitter.json" "$G/package.json" \
        "$G/queries" "$G/src" "$T/$1/parsers/tree-sitter-graphix/"
    cat > "$T/$1/config.json" <<EOF
{"parser-directories": ["$T/$1/parsers"],
 "theme": {"variable": {"color": 1}, "namespace": {"color": 2}}}
EOF
}

compact() {
    python3 -c '
import re, sys
t = re.sub(r" \[\d+, \d+\] - \[\d+, \d+\]", "", sys.stdin.read())
t = re.sub(r"\s+", " ", t).strip()
for a, b in [(r"\(reference \(module_path \(identifier\)\)\)", "ID"),
             (r"\(structure_pattern \(pattern_bind name: \(identifier\)\)\)", "PAT"),
             (r"\(lambda_param PAT\)", "P"), (r"\(literal \(number\)\)", "NUM")]:
    t = re.sub(a, b, t)
t = t[len("(source_file "):-1]
depth, cur = 0, ""
for ch in t:
    cur += ch
    depth += (ch == "(") - (ch == ")")
    if ch == ")" and depth == 0:
        print(cur.strip()); cur = ""
'
}

show() { # $1 = name
    local env=(env TREE_SITTER_LIBDIR="$T/$1/lib")
    (cd "$T/$1" && "${env[@]}" "$TS" parse --config-path config.json \
        --scope source.graphix "$T/x.gx" 2>/dev/null) | compact
    (cd "$T/$1" && "${env[@]}" "$TS" highlight --config-path config.json \
        --scope source.graphix "$T/x.gx" 2>/dev/null) | head -1 |
        sed -E 's/\x1b\[38;5;0?1m([^\x1b]*)\x1b\[0m/\1<variable>/g;
                s/\x1b\[38;5;0?2m([^\x1b]*)\x1b\[0m/\1<namespace>/g;
                s/\x1b\[[0-9;]*m//g'
}

echo "== committed parser"
setup head
show head

echo "== lambda: prec.right(-1, ...)"
setup fixed
P=$T/fixed/parsers/tree-sitter-graphix
python3 - "$P/grammar.js" <<'EOF'
import sys
p = sys.argv[1]
s = open(p).read()
old = "    lambda: $ => seq(\n"
assert s.count(old) == 1
i = s.index(old)
j = s.index("    ),\n", i)
s = s[:i] + "    lambda: $ => prec.right(-1, seq(\n" + s[i + len(old):j] \
    + "    )),\n" + s[j + len("    ),\n"):]
open(p, "w").write(s)
EOF
(cd "$P" && "$TS" generate 2>&1 | sed 's/^/  generate: /')
show fixed

if [ -n "${GRAPHIX:-}" ]; then
    echo "== real parser: let f = |acc, x| acc + x; f(1, 2)"
    cat > "$T/run.gx" <<'EOF'
let f = |acc, x| acc + x;
println("[f(1, 2)]");
sys::exit(sys::time::after_idle(duration:100.ms, 0))
EOF
    XDG_CACHE_HOME=$T/cache timeout -s KILL 60 "$GRAPHIX" --no-cache "$T/run.gx"
fi
