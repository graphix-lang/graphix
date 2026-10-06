#!/usr/bin/env bash
# x-panics-13: Unvalidated graphixfmt.json indent: a huge value panics, aborts or
# OOMs fmt and the LSP.
#
# FormatConfig (graphix-types/src/expr/format.rs:40) takes any usize for
# `indent`, from graphixfmt.json and from `graphix fmt --indent`, and
# PrettyBuf::push_indent (graphix-types/src/expr/print.rs:390) writes
# indent * depth spaces at the start of every nested line. Width is harmless
# at any value (0 and 2^64-1 both format).
#
# command:  bash design/review-2026-10-05/repro/x-panics-13.sh [graphix binary]
#           (default ~/tmp/target/debug/graphix; the fmt runs are held to
#           4 GiB of address space, so the runaway case aborts instead of
#           eating the machine)
#
# expected: an out-of-range indent is refused as an error naming the file
#           (exit 1, like a malformed graphixfmt.json); the language server
#           answers the formatting request and stays up.
# observed at c722befe (debug build):
#   fmt  indent 4                     rc=0, formatted
#   fmt  indent 18446744073709551615  rc=101, panicked at raw_vec/mod.rs: capacity overflow
#   fmt  indent 1099511627776         rc=134, memory allocation of 1099511627799 bytes failed
#   fmt  indent 1000000000            rc=134, memory allocation of 8000000184 bytes failed
#                                     (without the cap: past 6 GB within 15 s, OOM-killed)
#   lsp  indent 18446744073709551615  no reply to textDocument/formatting, server exit 101
#                                     (capacity overflow); with 1099511627776 it aborts, 134
set -u
G=${1:-$HOME/tmp/target/debug/graphix}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
cat > "$T/prog.gx" <<'EOF'
let f = |x| select x {
    0 => { let a = 1; let b = 2; a + b },
    n => { let c = n * 2; select c { 4 => 1, _ => { let d = c; d + 1 } } }
};
f(3)
EOF
for i in 4 18446744073709551615 1099511627776 1000000000; do
    printf '{ "indent": %s }\n' "$i" > "$T/graphixfmt.json"
    err=$( (ulimit -c 0; ulimit -v 4194304
            timeout -s KILL 120 "$G" fmt --stdout "$T/prog.gx" 2>&1 >/dev/null
            echo "rc=$?") | grep -E 'panicked|capacity|allocation|rc=' | tr '\n' ' ')
    echo "fmt  indent $i: $err"
done

printf '{ "indent": 18446744073709551615 }\n' > "$T/graphixfmt.json"
XDG_CACHE_HOME=$T/cache XDG_CONFIG_HOME=$T/config timeout -s KILL 150 python3 - "$G" "$T" <<'EOF'
import json, os, subprocess, sys
graphix, ws = sys.argv[1], sys.argv[2]
text = open(os.path.join(ws, "prog.gx")).read()
p = subprocess.Popen([graphix, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=subprocess.PIPE)
def send(m):
    b = json.dumps(dict(m, jsonrpc="2.0")).encode()
    try:
        p.stdin.write(b"Content-Length: %d\r\n\r\n%s" % (len(b), b)); p.stdin.flush()
    except OSError:
        pass
def recv(rid):
    while True:
        n = None
        while (l := p.stdout.readline()) not in (b"", b"\r\n", b"\n"):
            if l.lower().startswith(b"content-length:"):
                n = int(l.split(b":")[1])
        if n is None:
            return None
        m = json.loads(p.stdout.read(n))
        if m.get("id") == rid and "method" not in m:
            return m
root, uri = "file://" + ws, "file://" + ws + "/prog.gx"
send({"id": 1, "method": "initialize",
      "params": {"processId": os.getpid(), "rootUri": root, "capabilities": {}}})
recv(1)
send({"method": "initialized", "params": {}})
send({"method": "textDocument/didOpen", "params": {"textDocument": {
    "uri": uri, "languageId": "graphix", "version": 1, "text": text}}})
send({"id": 2, "method": "textDocument/formatting", "params": {
    "textDocument": {"uri": uri}, "options": {"tabSize": 4, "insertSpaces": True}}})
reply = recv(2)
if reply is not None:
    send({"id": 3, "method": "shutdown", "params": None}); recv(3)
    send({"method": "exit", "params": None})
rc = p.wait(timeout=60)
why = [l for l in p.stderr.read().decode(errors="replace").splitlines()
       if "panicked" in l or "capacity" in l]
print("lsp  indent 18446744073709551615: reply=%s server exit=%d %s"
      % ("yes" if reply else "none", rc, " ".join(why)))
EOF
