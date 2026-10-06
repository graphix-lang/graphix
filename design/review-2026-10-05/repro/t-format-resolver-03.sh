#!/usr/bin/env bash
# t-format-resolver-03: a script root's .gxi is half applied: its vals
# are never checked and a trailing declaration lands in the value slot.
#
# RootFile::load (graphix-types/src/expr/resolver.rs:541) pairs every root
# .gx with the .gxi beside it and add_interface_modules splices the
# interface's types, uses, mods and traits into the statements; the
# script path (graphix-rt/src/gx.rs:746, load_exprs, used by run and by
# --check / the LSP) keeps `(root.ori, root.exprs)` and drops `root.sig`.
# A lone module (a .gx no other file's `mod` reaches) is checked this way
# by `graphix --check foo.gx` and by the LSP.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-format-resolver-03.sh
#
# Case A: foo.gx `let x = 1; let z = x + 1`, foo.gxi `val x: i64; val z: i64;
#   type T = i64` (a valid module: `mod foo; foo::z` checks and prints 2).
# Case B: foo.gx `let x = 1; x`, foo.gxi `val x: string; type T = i64`
#   (an invalid module: `mod foo; foo::x` is refused).
# expected: foo.gx checked (or run) as a root gets the verdict it gets as
#   a module: A ok, B the signature mismatch
# observed (HEAD c722befe, debug build):
#   A  --check main.gx: ok
#   A  --check foo.gx:  refused at foo.gxi:3:1 "a type definition is not an
#      expression — it may only appear as a statement in a block or module
#      body, not where a value is expected"; `graphix foo.gx`: the same
#   B  --check main.gx: refused, signature mismatch "val x: ...",
#      signature has type string, implementation has type i64
#   B  --check foo.gx:  ok; `graphix foo.gx` runs and prints 1
#   A  LSP, foo.gx open, no main.gx in the workspace: the foo.gxi:3:1 error
#      above; with main.gx beside it: no diagnostic
#   B  LSP, foo.gx open, no main.gx: no diagnostic; with main.gx: the type
#      mismatch
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"

check() { # check <dir> <file>: ok, or the error's site and message
    printf '%-4s --check %-8s ' "$(basename "$1")" "$2"
    local out
    if out=$(cd "$1" && timeout -s KILL 60 "$GRAPHIX" --check "$2" 2>&1); then
        echo ok
    else
        printf '%s\n' "$out" | grep -v '^\s*$' | tail -2 \
            | sed -E -e 's#in file [^ ]*/([^/ ]*)#in file \1#g' -e 's/^ *[0-9]*: //' \
            | tr '\n' ' '
        echo
    fi
}

# lsp <workspace> <file>: open <file> in `graphix lsp`, print its diagnostics
lsp() {
    printf '%-4s LSP %s/%-8s ' "$(basename "$(dirname "$1")")" "$(basename "$1")" "$2"
    timeout -s KILL 120 python3 - "$GRAPHIX" "$1" "$2" <<'EOF'
import json, os, subprocess, sys
graphix, root, f = sys.argv[1], os.path.abspath(sys.argv[2]), sys.argv[3]
p = subprocess.Popen([graphix, "lsp"], stdin=subprocess.PIPE,
                     stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
def send(m):
    b = json.dumps(m).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(b) + b); p.stdin.flush()
def recv():
    n = 0
    while True:
        l = p.stdout.readline().decode().strip()
        if not l: break
        k, v = l.split(":", 1)
        if k.lower() == "content-length": n = int(v)
    return json.loads(p.stdout.read(n))
uri = lambda f: "file://" + os.path.join(root, f)
send({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
    "processId": None, "capabilities": {},
    "workspaceFolders": [{"uri": "file://" + root, "name": "probe"}]}})
while recv().get("id") != 1: pass
send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {
    "textDocument": {"uri": uri(f), "languageId": "graphix", "version": 1,
                     "text": open(os.path.join(root, f)).read()}}})
# a request is a barrier: diagnostics the open caused arrive first
send({"jsonrpc": "2.0", "id": 2, "method": "textDocument/hover", "params": {
    "textDocument": {"uri": uri(f)}, "position": {"line": 0, "character": 4}}})
out = []
while True:
    m = recv()
    if m.get("id") == 2: break
    if m.get("method") == "textDocument/publishDiagnostics":
        g = m["params"]["uri"].rsplit("/", 1)[1]
        for d in m["params"]["diagnostics"]:
            s = d["range"]["start"]
            out.append(f"{g}:{s['line'] + 1}:{s['character'] + 1} {d['message']}")
print("; ".join(out) or "no diagnostic")
send({"jsonrpc": "2.0", "id": 3, "method": "shutdown", "params": None})
while recv().get("id") != 3: pass
send({"jsonrpc": "2.0", "method": "exit", "params": None})
p.wait(timeout=10)
EOF
}

mk() { # mk <dir> <foo.gx> <foo.gxi> [<main.gx>]
    mkdir -p "$1"
    printf '%s\n' "$2" > "$1/foo.gx"
    printf '%s\n' "$3" > "$1/foo.gxi"
    [ $# -gt 3 ] && printf '%s\n' "$4" > "$1/main.gx"
    return 0
}

mk "$dir/A" 'let x = 1;
let z = x + 1' 'val x: i64;
val z: i64;
type T = i64' 'mod foo;
foo::z'
mk "$dir/B" 'let x = 1;
x' 'val x: string;
type T = i64' 'mod foo;
foo::x'

for c in A B; do
    check "$dir/$c" main.gx
    check "$dir/$c" foo.gx
done
for c in A B; do # run foo.gx; the timeout ends a run that does not fail
    printf '%-4s run foo.gx       ' "$c"
    (cd "$dir/$c" && timeout -s KILL 10 "$GRAPHIX" --no-cache foo.gx 2>&1) 2>/dev/null \
        | grep -v '^\s*$' | tail -1 | sed 's/^ *[0-9]*: //'
done
for c in A B; do
    mkdir -p "$dir/lsp-$c/alone" "$dir/lsp-$c/main"
    cp "$dir/$c/foo.gx" "$dir/$c/foo.gxi" "$dir/lsp-$c/alone/"
    cp "$dir/$c/foo.gx" "$dir/$c/foo.gxi" "$dir/$c/main.gx" "$dir/lsp-$c/main/"
    lsp "$dir/lsp-$c/alone" foo.gx
    lsp "$dir/lsp-$c/main" foo.gx
done
