#!/usr/bin/env bash
# t-format-resolver-01: a script's top-level `mod <stdlib package name>;`
# loads the package's source from the stdlib VFS and never reads the
# file beside the script; `--check` (and the LSP, same path) refuses the
# same script with "duplicate module definition".
#
# A root file's top-level statements resolve at scope `/` with no
# prepend (resolver.rs resolve_modules_in_scope), and the shell and the
# LSP put the stdlib VfsResolver first in the chain, which holds every
# package root as `/<pkg>/mod.gx`; so `mod str;` is answered by
# `/str/mod.gx`. Check compiles the file's statements at the root (where
# `/str` is the registered package), run inside the `#do` block.
#
# command: GRAPHIX=/path/to/graphix bash design/review-2026-10-05/repro/t-format-resolver-01.sh
#
# expected (book/src/shell.md "Module Search Path Priority": the file's
# parent directory first; design/module_system.md: a module named like a
# package wins over it, and check mode and load mode agree):
#   1. 1000 (str.gx's len)
#   2. 1000 (control: the same module named mystr)
#   3. a parse error in str.gx, as the control mystr.gx reports one
#   4. "module str could not be found" (no str.gx anywhere)
#   5. --check of case 1 accepts it, as the run does
#   6. a dynamic `mod str` (no resolver involved): run and --check agree
# observed (HEAD c722befe, debug build):
#   1. 4 (the stdlib's str::len)
#   2. 1000
#   3. 4: str.gx is never read; the control reports
#      "could not resolve module mystr ... parse error"
#   4. 4: the VFS supplies the stdlib str source as the user's module
#   5. "duplicate module definition str", exit 1
#   6. run prints 1000 (the user's str wins), --check refuses with
#      "duplicate module definition str", exit 1
# The same holds for every package the binary registers (this build:
# args array bench core db gui hbs http json list map pack rand re sqlite
# str sys toml tui xls); a local db.gx defining `connect` gives
# "db::connect not defined", a local core.gx "core::answer not defined".
set -u
GRAPHIX=${GRAPHIX:-graphix}
dir=$(mktemp -d)
trap 'rm -rf "$dir"' EXIT
export XDG_CACHE_HOME="$dir/cache"
gate='sys::exit(sys::time::after_idle(duration:100.ms, 0));'
run() {
    (cd "$1" && timeout -s KILL 30 "$GRAPHIX" --no-cache "${@:2}" main.gx 2>&1 \
        | grep -E '^[0-9]+$|could not|duplicate|parse error at' | head -n 3
     echo "  exit ${PIPESTATUS[0]}")
}

mkdir -p "$dir/c1" "$dir/c2" "$dir/c3" "$dir/c3ctl" "$dir/c4" "$dir/c6"
echo 'let len = |s: string| 1000' > "$dir/c1/str.gx"
printf 'mod str;\n%s\nstr::len("abcd")\n' "$gate" > "$dir/c1/main.gx"
echo 'let len = |s: string| 1000' > "$dir/c2/mystr.gx"
printf 'mod mystr;\n%s\nmystr::len("abcd")\n' "$gate" > "$dir/c2/main.gx"
echo 'let = not graphix (((' > "$dir/c3/str.gx"
cp "$dir/c1/main.gx" "$dir/c3/main.gx"
echo 'let = not graphix (((' > "$dir/c3ctl/mystr.gx"
cp "$dir/c2/main.gx" "$dir/c3ctl/main.gx"
cp "$dir/c1/main.gx" "$dir/c4/main.gx"
cat > "$dir/c6/main.gx" <<EOF
let status = mod str dynamic {
    sandbox whitelist [core];
    sig { val len: fn(s: string) -> i64 };
    source "let len = |s: string| 1000"
};
$gate
select status { error as e => never(dbg(e)), null as _ => str::len("abcd") }
EOF

echo "1. mod str; beside str.gx (len = 1000):"; run "$dir/c1"
echo "2. control, mod mystr; beside mystr.gx:"; run "$dir/c2"
echo "3. mod str; beside a str.gx that does not parse:"; run "$dir/c3"
echo "   control, mod mystr; beside a mystr.gx that does not parse:"; run "$dir/c3ctl"
echo "4. mod str; with no str.gx anywhere:"; run "$dir/c4"
echo "5. --check of case 1:"; run "$dir/c1" --check
echo "6. dynamic mod str, run:"; run "$dir/c6"
echo "   dynamic mod str, --check:"; run "$dir/c6" --check
