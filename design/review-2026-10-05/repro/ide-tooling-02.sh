#!/usr/bin/env bash
# ide-tooling-02: highlights.scm colors every plain reference and every
# unqualified call as @namespace, and @function never wins; `trait`,
# `impl`, `for` and the checked operators (`+?` ..) get no capture.
#
# command: bash design/review-2026-10-05/repro/ide-tooling-02.sh
#   (TREE_SITTER=/path/to/tree-sitter overrides the CLI; default is the
#   grammar's pinned devDependency, ide/tree-sitter-graphix/node_modules,
#   0.24.7. With helix and tmux on PATH it also renders the program in an
#   isolated Helix config that links the same queries, as install.sh does.)
#
# The program (it passes `graphix --check`) is printed with each identifier
# tagged [fn:..], [var:..] or [ns:..] by the color its capture got, and
# each keyword/operator tagged [kw:..]/[op:..]; an untagged word got none.
#
# expected (the file's own comments: later wins, a qualified call ends up
# "namespace::function", a plain reference is @variable):
#   [kw:trait] Size {   [kw:impl] Size [kw:for] Counter {
#   [kw:let] y [op:=] [fn:f]([fn:len]([var:xs])) [op:+] [ns:array]::[fn:len]([var:xs]) [op:+?] 1;
#   [fn:println]([var:y])
# observed (HEAD c722befe), both renderers: `trait`, `impl`, `for` and `+?`
# untagged, and no identifier ever [fn:..]:
#   tree-sitter 0.24.7 (a reference to a `let` of this file takes the
#   binding's @variable through locals.scm; any other is ns):
#     [kw:let] [var:y] [op:=] [var:f]([ns:len]([var:xs])) [op:+] [ns:array]::[var:len]([var:xs]) +? 1;
#     [ns:println]([var:y])
#   helix 25.07.1 (every plain reference is ns, local or not):
#     [kw:let] y [op:=] [ns:f]([ns:len]([ns:xs])) [op:+] [ns:array]::[var:len]([ns:xs]) +? 1;
#     [ns:println]([ns:y])
# With line 130 narrowed to qualifiers, `(module_path (identifier)
# @namespace . "::")` (and use_path alike), the call rule moved below it as
# `(apply (reference (module_path (identifier) @function .)))`, and the
# keywords and checked operators listed, Helix prints the expected lines
# exactly; tree-sitter does too, except `f`, which locals.scm keeps var.
set -euo pipefail
REPO="${GRAPHIX_REPO:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)}"
TS="${TREE_SITTER:-$REPO/ide/tree-sitter-graphix/node_modules/.bin/tree-sitter}"
T="$(mktemp -d)"
trap 'tmux -L ide02-$$ kill-server 2>/dev/null || true; rm -rf "$T"' EXIT

G="$T/dirs/tree-sitter-graphix"
mkdir -p "$G" "$T/home" "$T/cache"
cp -r "$REPO/ide/tree-sitter-graphix/src" "$REPO/ide/tree-sitter-graphix/queries" \
    "$REPO/ide/tree-sitter-graphix/tree-sitter.json" "$G/"

cat > "$T/prog.gx" <<'EOF'
// ide-tooling-02
use array::len;
trait Size {
    val size: fn(self) -> i64
};
type Counter = Abstract<i64>;
impl Size for Counter {
    let size = |c| c.0
};
let xs = [1, 2, 3];
let f = |a| a;
let y = f(len(xs)) + array::len(xs) +? 1;
println(y)
EOF

# one color per capture: 01 function, 02 variable, 03 namespace, 04 keyword, 05 operator
cat > "$T/config.json" <<EOF
{"parser-directories": ["$T/dirs"],
 "theme": {"function": "#000001", "variable": "#000002", "namespace": "#000003",
           "keyword": "#000004", "keyword.operator": "#000004", "operator": "#000005"}}
EOF

tag() {  # turn the color of each token into a tag
    local e=$'\033'
    sed -e "s/${e}\[38;2;10;20;1m\([^${e}]*\)/[fn:\1]/g" \
        -e "s/${e}\[38;2;10;20;2m\([^${e}]*\)/[var:\1]/g" \
        -e "s/${e}\[38;2;10;20;3m\([^${e}]*\)/[ns:\1]/g" \
        -e "s/${e}\[38;2;10;20;4m\([^${e} ]*\)/[kw:\1]/g" \
        -e "s/${e}\[38;2;10;20;5m\([^${e} ]*\)/[op:\1]/g" \
        -e "s/${e}\[[0-9;]*m//g"
}

echo "== tree-sitter $("$TS" --version | cut -d' ' -f2) highlight"
XDG_CACHE_HOME="$T/cache" HOME="$T/home" "$TS" highlight --html \
        --config-path "$T/config.json" "$T/prog.gx" |
    grep "class=line>" |
    sed -e 's/<td class=line-number>[0-9]*<\/td><td class=line>//' \
        -e "s#<span style='color: \#000001'>\([^<]*\)</span>#[fn:\1]#g" \
        -e "s#<span style='color: \#000002'>\([^<]*\)</span>#[var:\1]#g" \
        -e "s#<span style='color: \#000003'>\([^<]*\)</span>#[ns:\1]#g" \
        -e "s#<span style='color: \#000004'>\([^<]*\)</span>#[kw:\1]#g" \
        -e "s#<span style='color: \#000005'>\([^<]*\)</span>#[op:\1]#g" \
        -e 's/<[^>]*>//g' -e 's/&lt;/</g' -e 's/&gt;/>/g' -e 's/&amp;/\&/g' |
    grep -v '^//'

command -v helix >/dev/null && command -v tmux >/dev/null || exit 0
# Helix: the queries as install.sh links them, the grammar the CLI just built
HX="$T/hx/helix"
mkdir -p "$HX/themes" "$HX/runtime/grammars" "$HX/runtime/queries"
cp "$T/cache/tree-sitter/lib/graphix.so" "$HX/runtime/grammars/"
ln -s "$REPO/ide/tree-sitter-graphix/queries" "$HX/runtime/queries/graphix"
printf 'theme = "probe"\n[editor]\ntrue-color = true\n[editor.lsp]\nenable = false\n' \
    > "$HX/config.toml"
cat > "$HX/themes/probe.toml" <<'EOF'
"function" = "#0a1401"
"variable" = "#0a1402"
"namespace" = "#0a1403"
"keyword" = "#0a1404"
"operator" = "#0a1405"
"ui.text" = "#c8c8c8"
"ui.selection" = { bg = "#202020" }
EOF
printf '[[language]]\nname = "graphix"\nscope = "source.graphix"\nfile-types = ["gx"]\nlanguage-servers = []\nroots = []\n' \
    > "$HX/languages.toml"
echo "== $(helix --version)"
tmux -L ide02-$$ -f /dev/null new-session -d -x 120 -y 20 \
    "env -i PATH=/usr/bin:/bin TERM=tmux-256color HOME=$T/home XDG_CONFIG_HOME=$T/hx \
     XDG_CACHE_HOME=$T/cache XDG_DATA_HOME=$T/home XDG_STATE_HOME=$T/home helix $T/prog.gx"
sleep 4
tmux -L ide02-$$ capture-pane -e -p -t 0 | head -13 | tag | sed -e 's/^ *[0-9]*  //' -e 's/ *$//' |
    grep -v '^//'
