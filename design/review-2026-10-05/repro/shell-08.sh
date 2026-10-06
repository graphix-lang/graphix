#!/usr/bin/env bash
# shell-08: standalone binaries cannot take arguments: the first positional
# is run as a script.
#
# A standalone binary is this binary: `graphix package build-standalone`
# (graphix-package/src/lib.rs:1485-1574) builds graphix-shell's `graphix`
# (src/main.rs) with the package's `standalone` feature on, so its
# main_program() returns main.gx, and copies target/release/graphix to
# <package>/<name>. main.rs parses its own Params: the first positional is
# `file` (main.rs:223), program_args only collects what follows it, and an
# unknown flag is a clap error. The embedded program replaces only
# Mode::Repl (lib.rs:245-249), i.e. only a run with no positional, and that
# run's argument list is empty. So every probe below is what `myapp <args>`
# does; the embedded program can never receive an argument through
# sys::args() or args::parse.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/shell-08.sh
#
# expected: positional arguments and unknown flags reach the embedded
#   program (sys::args(), args::parse), as they reach a script after its
#   file name.
# observed (HEAD c722befe, debug build):
#   myapp hello            Error: No such file or directory (os error 2), exit 1
#   myapp input.csv        Error: parsing in file .../input.csv ... Unexpected `,`
#                          (the CSV is compiled as Graphix), exit 1
#   myapp --verbose        error: unexpected argument '--verbose' found
#                          (tip: use '-- --verbose'), exit 2
#   myapp -- --verbose     Error: No such file or directory (os error 2), exit 1
#                          (clap's tip makes it the script file)
#   myapp fmt              runs the shell's formatter on stdin, exit 0
#   showargs.gx hello --verbose
#                          ARGS [".../showargs.gx", "hello", "--verbose"]:
#                          arguments reach sys::args() only after a script file
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
cd "$dir" || exit 1
printf 'name,count\nfoo,1\n' > input.csv
cat > showargs.gx <<'EOF'
let a = sys::args();
println("ARGS [a]");
sys::exit(sys::time::after_idle(duration:100.ms, 0))
EOF
run() {
    echo "=== myapp $*"
    timeout -s KILL 30 "$GRAPHIX" --no-netidx --no-cache "$@" </dev/null 2>&1 | head -12
    echo "exit=${PIPESTATUS[0]}"
}
run hello
run input.csv
run --verbose
run -- --verbose
echo "=== myapp fmt (stdin: 1 + 2)"
echo '1 + 2' | timeout -s KILL 30 "$GRAPHIX" fmt 2>&1
echo "exit=$?"
run showargs.gx hello --verbose
