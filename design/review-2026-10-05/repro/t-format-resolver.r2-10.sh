#!/usr/bin/env bash
# t-format-resolver.r2-10: an interface beside the other module layout is
# dropped without a word, so the module's private items are public.
#
# resolve_from_files (graphix-types/src/expr/resolver.rs:279) and
# resolve_from_vfs (:240) pair foo.gx with foo.gxi and foo/mod.gx with
# foo/mod.gxi only, and report nothing about a .gxi beside the other
# layout or about foo/mod.gx shadowed by foo.gx. The LSP's workspace scan
# (graphix-lsp/src/workspace.rs:296) does resolve `mod foo` to a stray
# foo.gxi, so it leaves foo/mod.gx out of main.gx's project.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-format-resolver.r2-10.sh
#
# Cases (main.gx `mod foo; foo::secret`, the .gxi `val pub_x: i64`,
# the implementation `let pub_x = 1; let secret = 2`):
#   control  foo/mod.gx + foo/mod.gxi
#   stray    foo/mod.gx + foo.gxi
#   strayi   foo.gx + foo/mod.gxi
#   both     foo.gx `let v = "from foo.gx"` + foo/mod.gx `let v = "from
#            foo/mod.gx"`, main `mod foo; foo::v`
#   lsp      main.gx `let k = 5; mod foo; foo::pub_x`, foo/mod.gx
#            `let pub_x = super::k; let secret = 2`, foo/mod.gx opened in
#            `graphix lsp`, with the interface at foo/mod.gxi and at foo.gxi
# expected: stray and strayi refused as control is (or a diagnostic naming
#   the stray interface); both refused as ambiguous; lsp: no diagnostic in
#   either layout, since `graphix --check main.gx` passes in both
# observed (HEAD c722befe, debug build):
#   control  foo::secret not defined
#   stray    prints 2
#   strayi   prints 2
#   both     prints "from foo.gx"
#   lsp      foo/mod.gxi: no diagnostic; foo.gxi: foo/mod.gx:1:13 `super`
#            goes above the package root (foo/mod.gx checked as a root)
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
gate='sys::exit(sys::time::after_idle(duration:200.ms, 0));'

layout() { # layout <case> <intf path|-> <impl path>
    mkdir -p "$dir/$1/foo"
    [ "$2" != - ] && printf 'val pub_x: i64;\n' > "$dir/$1/$2"
    printf 'let pub_x = 1;\nlet secret = 2\n' > "$dir/$1/$3"
    printf 'mod foo;\n%s\nfoo::secret\n' "$gate" > "$dir/$1/main.gx"
}
layout control foo/mod.gxi foo/mod.gx
layout stray foo.gxi foo/mod.gx
layout strayi foo/mod.gxi foo.gx
mkdir -p "$dir/both/foo"
printf 'let v = "from foo.gx"\n' > "$dir/both/foo.gx"
printf 'let v = "from foo/mod.gx"\n' > "$dir/both/foo/mod.gx"
printf 'mod foo;\n%s\nfoo::v\n' "$gate" > "$dir/both/main.gx"

for c in control stray strayi both; do
    printf '%-8s ' "$c"
    (cd "$dir/$c" && timeout -s KILL 60 "$GRAPHIX" --no-cache main.gx 2>&1) \
        | grep -v '^\s*$' | tail -1 | sed 's/^ *[0-9]*: //'
done

# lsp <workspace> <file>: open <file> in `graphix lsp`, print its diagnostics
lsp() {
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
        g = m["params"]["uri"].split(root + "/", 1)[-1]
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

for intf in foo/mod.gxi foo.gxi; do
    w="$dir/lsp-${intf//\//_}"
    mkdir -p "$w/foo"
    printf 'val pub_x: i64;\n' > "$w/$intf"
    printf 'let pub_x = super::k;\nlet secret = 2\n' > "$w/foo/mod.gx"
    printf 'let k = 5;\nmod foo;\nfoo::pub_x\n' > "$w/main.gx"
    printf 'lsp %-12s --check main.gx: ' "$intf"
    (cd "$w" && timeout -s KILL 60 "$GRAPHIX" --check main.gx >/dev/null 2>&1) \
        && echo ok || echo refused
    printf 'lsp %-12s open foo/mod.gx: ' "$intf"
    lsp "$w" foo/mod.gx
done
