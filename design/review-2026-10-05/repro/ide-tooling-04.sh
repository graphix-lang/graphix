#!/usr/bin/env bash
# ide-tooling-04: Helix grammar `rev = "main"` never advances after the
# first fetch; the linked queries then fail to compile and .gx files lose
# their syntax coloring.
#
# ide/editors/helix/languages.toml (Option B, the block install.sh
# appends) sets `rev = "main"`. `helix --grammar fetch` runs
# `git fetch --depth 1 origin main` and then `git checkout main`; after the
# first install a local `main` exists, so the checkout never moves it, and
# helix still prints "graphix now on main". install.sh LINKS the queries to
# the user's checkout and says to re-run it after a grammar change. After a
# pull that adds a node the queries use, the re-run keeps the old grammar
# ("1 grammars already built") and helix refuses highlights.scm.
#
# A local repo stands in for github.com/graphix-lang/graphix: it holds the
# ide/ tree of C1 (55237d7a, before the `abort`/`flush` tokens), then of C2
# (c722befe, where grammar.js and highlights.scm both have them). Only the
# URL in the copied languages.toml is rewritten. HOME, XDG_CONFIG_HOME and
# XDG_CACHE_HOME are sandboxed; helix, git and install.sh run unmodified.
# Step 3 is the control: the same block with `rev` pinned to C2's SHA.
#
# command: bash design/review-2026-10-05/repro/ide-tooling-04.sh
#
# expected: after the pull and the re-run (step 2), helix's grammar
# checkout is at C2, helix.log is clean, and the sample is colored as in
# step 1.
#
# observed (helix 25.07.1, git 2.56.0, HEAD c722befe):
#   step 1: grammar checkout = c1, no helix.log error, 8 foreground colors
#   step 2: install.sh prints "1 updated grammars / graphix now on main"
#     and "1 grammars already built"; grammar checkout still = c1 while
#     upstream and the user's checkout are at c2; helix.log:
#     helix_core::syntax [ERROR] Failed to compile highlights for
#     'graphix': invalid node type "abort"; 4 foreground colors (the UI's
#     alone); `helix --health graphix` still reports Highlight queries ✓
#   step 3 (rev pinned to c2's SHA): checkout = c2, no error, 8 colors
set -u
REPO=$(git -C "$(dirname "$0")" rev-parse --show-toplevel)
C1=55237d7a
C2=c722befe
HELIX_BIN=$(command -v helix || command -v hx) || { echo "no helix/hx on PATH"; exit 1; }
P=$(mktemp -d)
trap 'rm -rf "$P"' EXIT
mkdir -p "$P/home" "$P/cfg/helix" "$P/cache/helix" "$P/bin"
ln -s "$HELIX_BIN" "$P/bin/helix"
export GIT_CONFIG_NOSYSTEM=1
G() { git -c user.name=probe -c user.email=probe@example.com -c init.defaultBranch=main "$@"; }
# No ~/.cargo/bin on PATH: the editor runs start no graphix LSP.
sandbox() {
    env PATH="$P/bin:/usr/local/bin:/usr/bin:/bin" HOME="$P/home" \
        XDG_CONFIG_HOME="$P/cfg" XDG_CACHE_HOME="$P/cache" "$@"
}
snapshot() {
    rm -rf "$P/up/ide"
    git -C "$REPO" archive --format=tar "$1" ide/editors/helix ide/tree-sitter-graphix |
        tar -x -C "$P/up"
    sed -i "s|https://github.com/graphix-lang/graphix|file://$P/up|" \
        "$P/up/ide/editors/helix/languages.toml"
    G -C "$P/up" add -A
    G -C "$P/up" commit -qm "$2"
}

cat > "$P/hxopen.py" <<'PY'
# Open a file in helix in a pty for 4 s, quit, count the foreground colors drawn.
import fcntl, os, pty, re, select, signal, struct, sys, termios, time
pid, fd = pty.fork()
if pid == 0:
    os.execvpe("helix", ["helix", sys.argv[1]],
               dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor"))
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 30, 100, 0, 0))
out = b""
def pump(secs):
    global out
    end = time.time() + secs
    while time.time() < end:
        if select.select([fd], [], [], 0.05)[0]:
            try:
                data = os.read(fd, 65536)
            except OSError:
                return
            if not data:
                return
            out += data
pump(4.0)
os.write(fd, b"\x1b")
pump(0.5)
os.write(fd, b":q!\r")
pump(3.0)
if os.waitpid(pid, os.WNOHANG)[0] == 0:
    os.kill(pid, signal.SIGKILL)
    os.waitpid(pid, 0)
fg = set(re.findall(rb"\x1b\[(?:[0-9;]*;)?38;2;(\d+;\d+;\d+)", out))
fg |= set(re.findall(rb"\x1b\[(?:[0-9;]*;)?38;5;(\d+)", out))
print(f"  foreground colors on screen: {len(fg)}")
PY

report() {
    local S=$P/cfg/helix/runtime/grammars/sources/graphix
    echo "  helix's grammar checkout: $(git -C "$S" log --oneline -1 --no-ext-diff)"
    echo "  upstream main:            $(git -C "$P/up" log --oneline -1 --no-ext-diff)"
    echo "  user's checkout:          $(git -C "$P/checkout" log --oneline -1 --no-ext-diff)"
    echo "  \"abort\" in the grammar helix built: $(grep -c '"value": "abort"' "$S/ide/tree-sitter-graphix/src/grammar.json")" \
        " / in the linked highlights.scm: $(grep -c '"abort"' "$P/cfg/helix/runtime/queries/graphix/highlights.scm")"
    : > "$P/cache/helix/helix.log"
    sandbox timeout -s KILL 30 python3 "$P/hxopen.py" "$P/sample.gx"
    echo "  helix.log errors after opening a .gx file:"
    grep -E 'ERROR' "$P/cache/helix/helix.log" | cut -c1-200 | sed 's/^/    /'
}

G init -q "$P/up"
snapshot "$C1" "c1 = $C1"
G clone -q "$P/up" "$P/checkout"
cp "$REPO/book/src/examples/tui/gauge_threshold.gx" "$P/sample.gx"
# A user setting that keeps `--grammar fetch/build` to graphix alone
# (otherwise helix clones every built-in grammar from the network).
echo 'use-grammars = { only = ["graphix"] }' > "$P/cfg/helix/languages.toml"

echo "=== 1. install.sh at c1"
sandbox timeout -s KILL 170 bash "$P/checkout/ide/editors/helix/install.sh" 2>&1 |
    grep -E 'grammar|now on|built' | sed 's/^/  | /'
report

echo "=== 2. upstream main moves to c2; the user pulls and re-runs install.sh"
snapshot "$C2" "c2 = $C2"
G -C "$P/checkout" pull -q
sandbox timeout -s KILL 170 bash "$P/checkout/ide/editors/helix/install.sh" 2>&1 |
    grep -E 'grammar|now on|built' | sed 's/^/  | /'
report
echo "  helix --health graphix says:"
sandbox timeout -s KILL 30 helix --health graphix 2>&1 | sed 's/\x1b\[[0-9;]*m//g' |
    grep -E 'Tree-sitter|Highlight' | sed 's/^/    /'

echo "=== 3. control: the same block with rev pinned to c2's SHA"
sed -i "s|rev = \"main\"|rev = \"$(git -C "$P/up" rev-parse HEAD)\"|" "$P/cfg/helix/languages.toml"
sandbox timeout -s KILL 60 helix --grammar fetch 2>&1 | sed 's/^/  | /'
sandbox timeout -s KILL 170 helix --grammar build 2>&1 | sed 's/^/  | /'
report
